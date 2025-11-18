## ###########################################
## TRABAJO 3: AGRUPAMIENTO DE ZONAS CENSALES
## ###########################################

rm(list = ls())
## 1. LIBRERÍAS Y CONEXIÓN ####

# Librerías para Clustering y Visualización
library(factoextra) # Para el método del codo (fviz_nbclust)
library(ggplot2)    # Para gráficos y mapas
library(cowplot)    # Para composición de mapas
library(dplyr)      # Para manipulación de datos

# Librerías Geoespaciales y de Base de Datos
library(sf)         # Simple Features (manejo de datos espaciales)
library(DBI)        # Interfaz de conexión a bases de datos
library(RPostgres)  # Driver para PostgreSQL

# ----------------------------------------------------
# 1.1. Definición de Parámetros de Conexión (Reemplaza si es necesario)
# ----------------------------------------------------
db_host     = "localhost"     # servidor de BD
db_port     = 5432            # puerto de escucha
db_name     = "censo_rm_2017" # nombre de la base
db_user     = "postgres"      # usuario de conexión
db_password = "postgres"      # clave de usuario

# ----------------------------------------------------
# 1.2. Establecer Conexión a BD
# ----------------------------------------------------
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

## 2. OBTENCIÓN Y UNIÓN DE VARIABLES (Final) ####

# Nombre de la tabla de geometría según la captura:
TABLA_GEOMETRIA = "dpa.zonas_censales_rm" # Nombre confirmado

sql_base_geometria_esc = paste0("
WITH zonas_indicadores AS (
    -- 1. Calcular el porcentaje de escolaridad por zona censal (zc)
    SELECT
        z.geocodigo,
        ROUND(
            COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
            / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
        2) AS ptje_esc_mayor_16
    FROM public.personas AS p
    JOIN public.hogares AS h ON p.hogar_ref_id = h.hogar_ref_id
    JOIN public.viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
    JOIN public.zonas AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
    JOIN public.comunas AS c ON z.codigo_comuna = c.codigo_comuna 
    GROUP BY z.geocodigo
    HAVING COUNT(*) > 10
)
-- 2. Unir el cálculo de escolaridad con la tabla de geometría y FILTRAR por Santiago
SELECT 
    g.geocodigo::text AS geocodigo, -- Convertimos a TEXT para que el st_read lo reconozca
    g.nom_comuna,
    g.geom,  
    zi.ptje_esc_mayor_16

FROM ", TABLA_GEOMETRIA, " AS g      
JOIN zonas_indicadores AS zi         
    ON g.geocodigo::text = zi.geocodigo -- ¡CONVERSIÓN AQUÍ!
WHERE g.nom_provin = 'SANTIAGO' 
")

# Ejecutar y crear el objeto espacial base
zonas_gs_base = st_read(con, query = sql_base_geometria_esc)

cat(paste0("Cargadas ", nrow(zonas_gs_base), " zonas censales con geometría y escolaridad.\n"))

# ----------------------------------------------------
# 2.2. CONSULTA OBESIDAD (Microsimulada T2)
# ----------------------------------------------------
sql_obesidad = "SELECT geocodigo, tasa_sobrepeso_obeso FROM out.zc_obesidad_microsim"
df_obesidad = dbGetQuery(con, sql_obesidad)

# ----------------------------------------------------
# 2.3. CONSULTA INGRESO (Microsimulada /Clases)
# ----------------------------------------------------
sql_ingreso = "SELECT geocodigo, mediana_ingreso FROM out.zc_ingreso_microsim"
df_ingreso = dbGetQuery(con, sql_ingreso)

# ----------------------------------------------------
# 2.4. UNIÓN FINAL DE VARIABLES EN R y Limpieza
# ----------------------------------------------------

zonas_gs_clusters = zonas_gs_base %>%
  # Unir con Obesidad
  left_join(df_obesidad, by = "geocodigo") %>%
  # Unir con Ingreso
  left_join(df_ingreso, by = "geocodigo") %>%
  # Estandarización final y limpieza de NA
  mutate(geocodigo = as.character(geocodigo)) %>%
  filter(!is.na(tasa_sobrepeso_obeso) & !is.na(mediana_ingreso) & !is.na(ptje_esc_mayor_16))

# Desconectar la BD (liberar recursos)
dbDisconnect(con) 

cat(paste0("Total de Zonas Censales listas para clustering (sin NA): ", nrow(zonas_gs_clusters), ".\n"))

## 3. CLUSTERING K-MEANS Y ASIGNACIÓN ####

# ----------------------------------------------------
# 3.1. Selección de Variables y Escalado
# ----------------------------------------------------
# El objeto zonas_gs_clusters ya está listo con las 3 variables: Obesidad, Ingreso, Escolaridad
vars_clusters = zonas_gs_clusters %>%
  st_drop_geometry() %>%
  select(tasa_sobrepeso_obeso, mediana_ingreso, ptje_esc_mayor_16)

# Escalado (OBLIGATORIO para K-Means)
vars_scaled = scale(vars_clusters)

# ----------------------------------------------------
# 3.2. Determinación de K (Método del Codo)
# ----------------------------------------------------
# Genera el gráfico para justificar el número óptimo de K
wss_plot = fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del Codo para K-Means", x = "Número de clusters (k)", y = "WSS") +
  theme_minimal()
print(wss_plot) 
# 

# ----------------------------------------------------
# 3.3. Ejecución de K-Means 
# ----------------------------------------------------
K = 4 
set.seed(123)
km_result = kmeans(vars_scaled, centers = K, nstart = 25)

# Asignar los clusters de vuelta al dataframe espacial
zonas_gs_clusters$cluster = as.factor(km_result$cluster)


## 4. ANÁLISIS E INTERPRETACIÓN DE CLUSTERS (K=4) ####

# ----------------------------------------------------
# 4.1. Perfil estadístico de cada Cluster
# ----------------------------------------------------
cluster_perfil = zonas_gs_clusters %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    N_Zonas = n(),
    Tasa_Obesidad_Media = mean(tasa_sobrepeso_obeso, na.rm = TRUE),
    Ingreso_Mediano_Media = mean(mediana_ingreso, na.rm = TRUE),
    Escolaridad_Media = mean(ptje_esc_mayor_16, na.rm = TRUE),
    .groups = 'drop'
  )

print(cluster_perfil)


# ----------------------------------------------------
## 4.2. Asignación de Etiquetas  ####

# Definición de las etiquetas basadas en el análisis de perfiles (Nombres definitivos para el informe):
etiquetas_cluster_df = data.frame(
  cluster = as.factor(c(1, 2, 3, 4)),
  cluster_etiqueta = c(
    "Clase Media Consolidada",                        # Cluster 1: Ingreso Medio, ALTA Escolaridad (Baja Obs)
    "Doble Vulnerabilidad Crítica",                   # Cluster 2: Máximo Riesgo
    "Déficit de Capital Humano y Riesgo Sanitario",   # Cluster 3: Ingreso Bajo, BAJA Escolaridad (Media Obs)
    "Alto Bienestar/Capital Humano"                   # Cluster 4: Mínimo Riesgo
  )
)

# Unir las etiquetas al dataframe principal.
zonas_gs_clusters = zonas_gs_clusters %>%
  left_join(etiquetas_cluster_df, by = "cluster")

# ----------------------------------------------------
# 4.3. Ordenamiento del Factor para la Leyenda (ROJO -> NARANJA -> AZUL -> VERDE)
# ----------------------------------------------------
# Ordenamos para la visualización del riesgo:
# El orden ahora es: Crítica -> Periférica -> Consolidada -> Bienestar.
zonas_gs_clusters$cluster_etiqueta = factor(
  zonas_gs_clusters$cluster_etiqueta,
  levels = c(
    "Doble Vulnerabilidad Crítica",                    # 1. Máximo Riesgo (ROJO)
    "Déficit de Capital Humano y Riesgo Sanitario",    # 2. Riesgo Alto/Medio (NARANJA)
    "Clase Media Consolidada",                         # 3. Riesgo Controlado (AZUL)
    "Alto Bienestar/Capital Humano"                    # 4. Mínimo Riesgo (VERDE)
  )
)

## 4.5. GRÁFICOS DE DISPERSIÓN 2D PARA VISUALIZAR LOS CLUSTERS (K=4) ####
# Objetivo: Mostrar cómo se distribuyen los 4 clusters en el espacio de las variables originales
# (Ingreso, Obesidad, Escolaridad) para una interpretación directa.

# ----------------------------------------------------
# Gráfico 1: Riesgo Sanitario vs. Riesgo Económico (Obesidad vs. Ingreso)
# ----------------------------------------------------
plot_obs_ingreso = ggplot(zonas_gs_clusters, aes(x = mediana_ingreso, y = tasa_sobrepeso_obeso, color = cluster_etiqueta)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Densidad de Clusters: Obesidad vs. Ingreso",
    x = "Mediana de Ingreso Percápita Estimada (CLP)",
    y = "Tasa de Sobrepeso/Obesidad Estimada (%)"
  ) +
  scale_color_manual(
    name = "Perfil de Vulnerabilidad",
    values = c(
      "Doble Vulnerabilidad Crítica" = "#E41A1C",           # Rojo
      "Déficit de Capital Humano y Riesgo Sanitario" = "#FF7F00",  # Naranja
      "Clase Media Consolidada" = "#377EB8",                # Azul
      "Alto Bienestar/Capital Humano" = "#4DAF4A"           # Verde
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(plot_obs_ingreso)

# ----------------------------------------------------
# Gráfico 2: Bienestar vs. Capital Humano (Ingreso vs. Escolaridad)
# ----------------------------------------------------
plot_ingreso_esc = ggplot(zonas_gs_clusters, aes(x = ptje_esc_mayor_16, y = mediana_ingreso, color = cluster_etiqueta)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Distribución de Clusters: Ingreso vs. Escolaridad",
    x = "% Población con >= 16 años de Escolaridad (Capital Humano)",
    y = "Mediana de Ingreso Percápita Estimada (CLP)"
  ) +
  scale_color_manual(
    name = "Perfil de Vulnerabilidad",
    values = c(
      "Doble Vulnerabilidad Crítica" = "#E41A1C",           # Rojo
      "Déficit de Capital Humano y Riesgo Sanitario" = "#FF7F00",  # Naranja
      "Clase Media Consolidada" = "#377EB8",                # Azul
      "Alto Bienestar/Capital Humano" = "#4DAF4A"           # Verde
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(plot_ingreso_esc)

## 5. VISUALIZACIÓN DE CLUSTERS (MAPA 1 - K=4) ####

# Reestablecer Conexión (si ya se cerró en la Sección 2)
db_host     = "localhost"; db_port = 5432; db_name = "censo_rm_2017"; 
db_user     = "postgres"; db_password = "postgres"

con = dbConnect(Postgres(), dbname=db_name, host=db_host, port=db_port, user=db_user, password=db_password)

# Cargar Geometría Comunal
sql_comunas = "
SELECT nom_comuna, geom, nom_provin
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago = st_read(con, query = sql_comunas)
dbDisconnect(con) 

# Calcular Centroides y Bounding Box (necesario para el mapa)
centroides_comuna = st_centroid(sf_comunas_santiago)
bbox_clusters = st_bbox(zonas_gs_clusters)

# Reutilizamos las geometrías y límites calculados previamente (sf_comunas_santiago, centroides_comuna, bbox_clusters)

mapa_clusters_k4 = ggplot() +
  geom_sf(data = zonas_gs_clusters, aes(fill = cluster_etiqueta), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf_text(data = centroides_comuna, aes(label = nom_comuna), size = 2.5, fontface = "bold") +
  
  # === Mapeo de color para 4 grupos (ROJO -> NARANJA -> AZUL CLARO -> VERDE) ===
  scale_fill_manual(
    name = "Perfil de Vulnerabilidad",
    values = c(
      "Doble Vulnerabilidad Crítica" = "#E41A1C",           # 1. Rojo (Máximo Riesgo)
      "Déficit de Capital Humano y Riesgo Sanitario" = "#FF7F00",  # 2. Naranja (Alto Riesgo Sanitario)
      "Clase Media Consolidada" = "#377EB8",                # 3. Azul (Riesgo Controlado)
      "Alto Bienestar/Capital Humano" = "#4DAF4A"           # 4. Verde (Mínimo Riesgo)
    )
  ) +
  
  labs(
    title = "Mapa de Clusters (K=4) de Zonas Censales",
    subtitle = "Segmentación Multivariada basada en Obesidad, Ingreso y Escolaridad"
  ) +
  coord_sf(
    xlim = c(bbox_clusters["xmin"], bbox_clusters["xmax"]),
    ylim = c(bbox_clusters["ymin"], bbox_clusters["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_clusters_k4)
## 6. ÍNDICE DE SHANNON (VARIABILIDAD INTRA-COMUNAL) ####

# ----------------------------------------------------
# 6.1. Cálculo del Índice de Shannon
# ----------------------------------------------------
# El Índice de Shannon (H) mide la diversidad de los clusters (la heterogeneidad)
# dentro de cada unidad geográfica superior (la comuna).

shannon_df = zonas_gs_clusters %>%
  st_drop_geometry() %>%
  group_by(nom_comuna) %>%
  mutate(total_zc = n()) %>% # Total de Zonas Censales (ZC) por Comuna
  group_by(nom_comuna, cluster) %>%
  summarise(
    n_cluster = n(),
    total_zc = first(total_zc),
    .groups = 'drop_last'
  ) %>%
  # 1. Calcular la Proporción (p_ij) de cada cluster por comuna
  mutate(p_ij = n_cluster / total_zc) %>%
  # 2. Aplicar la fórmula de Shannon (p_ij * ln(p_ij))
  mutate(shannon_comp = p_ij * log(p_ij)) %>%
  # 3. Sumar y Negar el resultado (H = -SUM(p_ij * ln(p_ij)))
  summarise(
    indice_shannon = -sum(shannon_comp, na.rm = TRUE)
  )

# ----------------------------------------------------

## 6.2. Mapa de Variabilidad Interna (SOLUCIÓN FINAL DE ETIQUETAS - MATRIZ DE COORDENADAS) ####

# ----------------------------------------------------
# Define los límites del mapa usando las Zonas Censales:
# ----------------------------------------------------
bbox_clusters = st_bbox(zonas_gs_clusters)

# 1. Volvemos a obtener el centroide original (Limpio)
# Necesitas reestablecer la conexión con la BD si la habías cerrado
db_host     = "localhost"; db_port = 5432; db_name = "censo_rm_2017"; 
db_user     = "postgres"; db_password = "postgres"

con = dbConnect(Postgres(), dbname=db_name, host=db_host, port=db_port, user=db_user, password=db_password)

sql_comunas = "SELECT nom_comuna, geom, nom_provin FROM dpa.comunas_rm_shp WHERE nom_provin = 'SANTIAGO';"
sf_comunas_santiago = st_read(con, query = sql_comunas)
dbDisconnect(con) 
sf_shannon = sf_comunas_santiago %>%
  left_join(shannon_df, by = "nom_comuna")

# Recalculamos el centroide para evitar el objeto SF roto
centroides_original = st_centroid(sf_comunas_santiago)
centroides_final = centroides_original 

# 2. Creamos un vector de desplazamiento (X, Y)
coordenadas_ajustadas = st_coordinates(centroides_final)

# 3. Aplicamos el desplazamiento a las coordenadas (basado en el índice de la comuna)
for (i in 1:nrow(centroides_final)) {
  comuna = centroides_final$nom_comuna[i]
  
  if (comuna == "LO BARNECHEA") {
    coordenadas_ajustadas[i, "X"] = -70.47
    coordenadas_ajustadas[i, "Y"] = -33.30
  } else if (comuna == "PUDAHUEL") {
    coordenadas_ajustadas[i, "X"] = -70.81
    coordenadas_ajustadas[i, "Y"] = -33.40
  } else if (comuna == "MAIPU") {
    coordenadas_ajustadas[i, "X"] = -70.80
    coordenadas_ajustadas[i, "Y"] = -33.60
  }
}

# 4. Creamos una nueva geometría (sfc) a partir de las coordenadas ajustadas
# Usamos el CRS original (necesario para la compatibilidad del mapa)
crs_original = st_crs(centroides_final) 

# Creamos la nueva colección de puntos (sfc)
nueva_geometria = st_sfc(
  sapply(1:nrow(coordenadas_ajustadas), function(i) {
    st_point(coordenadas_ajustadas[i, ])
  }, simplify = FALSE),
  crs = crs_original
)

# 5. Reasignamos la nueva geometría al objeto SF original
st_geometry(centroides_final) = nueva_geometria

# ----------------------------------------------------
# 5. Generación del Mapa Final de Shannon (Usamos centroides_final)
# ----------------------------------------------------


mapa_shannon_final = ggplot(sf_shannon) +
  geom_sf(aes(fill = indice_shannon), color = "black", linewidth = 0.5) +
  
  scale_fill_viridis_c(
    option = "plasma", 
    direction = -1, 
    name = "Índice de Shannon (H)"
  ) +
  
  # Usamos el objeto con las etiquetas corregidas:
  geom_sf_text(
    data = centroides_final, # <<-- OBJETO FINAL CORREGIDO
    aes(label = nom_comuna), 
    size = 2.5, 
    fontface = "bold"
  ) +
  
  labs(
    title = "Diversidad de Clusters (Índice de Shannon) por Comuna",
    subtitle = "Alto H: Alta segregación interna de perfiles de vulnerabilidad."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  
  coord_sf(
    xlim = c(bbox_clusters["xmin"], bbox_clusters["xmax"]),
    ylim = c(bbox_clusters["ymin"], bbox_clusters["ymax"]),
    expand = FALSE
  )

print(mapa_shannon_final)

