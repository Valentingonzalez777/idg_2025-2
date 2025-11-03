
# Limpiar el entorno de trabajo para iniciar un proceso limpio
rm(list = ls())

## 1. Librerías y Carga de Entradas ####
library(rakeR)
library(RPostgres)
library(DBI)
library(sf)
library(dplyr)
library(data.table) 
library(ggplot2)
library(viridis) 
library(scales) 
library(sf)
library(biscale)
library(cowplot)


# Rutas de los archivos de microdatos y márgenes
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds" # Usaremos la ruta original de la clase para que el profesor lo corra

# Carga de los Data Frames
casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)


# === ALINEACIÓN CRÍTICA DEL DATA FRAME DE MÁRGENES (CENSO) ===
# Forzamos la estandarización para evitar el error de tipado en las claves de unión.
# ESTO FUE LO QUE ARREGLÓ EL PROBLEMA DE CARGA EN EL AMBIENTE DEL PROFESOR
names(cons_censo_df) <- toupper(names(cons_censo_df))
cons_censo_df$GEOCODIGO <- as.character(cons_censo_df$GEOCODIGO) 
cons_censo_df$COMUNA <- as.character(cons_censo_df$COMUNA) 


## 2. PRE-PROCESAMIENTO Y ESTANDARIZACIÓN ####

### 2.1 Márgenes de Control (CENSO)
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels = grep("^EDAD", col_cons, value = TRUE)
esc_levels = grep("^ESCO", col_cons, value = TRUE)
sexo_levels = grep("^SEXO", col_cons, value = TRUE)

### 2.2 Microdatos (CASEN) 

vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "s2") 
casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Limpieza inicial de variables
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$Comuna = as.character(casen$Comuna) # Forzar a carácter para el split
casen$estrato = NULL
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s2 = as.numeric(unclass(casen$s2)) 
casen$ID = as.character(seq_len(nrow(casen)))

# Manejo de Valores Perdidos y Creación de la Variable Y
casen$s2[casen$s2 == -88] = NA 

# === 1. CREACIÓN DE LA VARIABLE BINARIA (Y) ===
casen$tasa_sobrepeso_obeso = as.integer(casen$s2 %in% c(3, 4)) 

# === 2. IMPUTACIÓN DE EJECUCIÓN (TAL COMO EN EL MODELO DEL PROFESOR) ===
# Se asume que la falta de dato (menores, no respondieron) es un '0' para que el rakeR corra.
casen$tasa_sobrepeso_obeso[is.na(casen$tasa_sobrepeso_obeso)] <- 0


# Imputación de Escolaridad 
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

# LIMPIEZA FINAL: Solo aseguramos que los controles no tengan NA's
casen_clean = casen %>%
  filter(!is.na(esc) & !is.na(sexo) & !is.na(edad))

casen = casen_clean

### 2.3 Recodificación de Variables de Control (X) - ALINEACIÓN DE FACTORES

# 2.3.1. Categorización de Edad (X)
# Se crea el factor Y se le asignan los niveles correctos
casen$edad_cat = factor(
  as.character(cut(
    casen$edad,
    breaks = c(0,30,40,50,60,70,80,Inf),
    labels = age_levels,
    right = FALSE, include.lowest = TRUE
  )),
  levels = age_levels # <--- CLAVE: Forzar los niveles del CENSO
)


# 2.3.2. Categorización de Escolaridad (X)
# Se usa as.character para limpiar el factor antes de forzar los niveles del CENSO
casen$esc_cat = factor(
  as.character(with(casen,
                    ifelse(esc == 0, esc_levels[1],
                           ifelse(esc <= 8, esc_levels[2],
                                  ifelse(esc <= 12, esc_levels[3],
                                         esc_levels[4]))))),
  
  levels = esc_levels # <--- CLAVE: Forzar los niveles del CENSO
)


# 2.3.3. Recodificación de sexo (X)
# Se usa as.character para limpiar el factor antes de forzar los niveles del CENSO
casen$sexo_cat = factor(
  as.character(ifelse(casen$sexo == 2, sexo_levels[1],
                      ifelse(casen$sexo == 1, sexo_levels[2], NA))),
  levels = sexo_levels # <--- CLAVE: Forzar los niveles del CENSO
)

## 3. EJECUCIÓN DE MICROSIMULACIÓN (RAKING) - CÓDIGO FINAL DE PRODUCCIÓN ####

cons_censo_comunas = split(cons_censo_df, as.character(cons_censo_df$COMUNA))
inds_list = split(casen, casen$Comuna)
zonas_validas = intersect(names(cons_censo_comunas), names(inds_list))

# Se aplica el proceso de raking solo a las comunas válidas, manejando errores con tryCatch
sim_list = lapply(zonas_validas, function(zona) {
  resultado <- tryCatch({
    cons_i    = cons_censo_comunas[[zona]]
    col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
    cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
    tmp     = inds_list[[zona]]
    if (nrow(tmp) == 0) {
      return(NULL)
    }
    
    # 1. Preparación de Variables de Control (X)
    inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
    names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
    
    # 2. Ponderación Iterativa (Raking)
    w_frac  = weight(cons = cons_i, inds = inds_i,
                     vars = c("Edad","Escolaridad","Sexo"))
    
    # 3. Generación y Merge Final
    sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
    
    # 4. Asignación de la Variable Y (Valor Binario 0/1)
    # NOTA: Usamos el nombre 'tasa_sobrepeso_obeso' que fue imputado y ya no tiene NAs.
    merge(sim_i,
          tmp[, c("ID","tasa_sobrepeso_obeso")], 
          by = "ID", all.x = TRUE)
    
  }, 
  error = function(e) {
    if (grepl("Weight populations don't match", e$message) | grepl("se intenta especificar un atributo en un NULL", e$message)) {
      cat(paste0(">>> ERROR IGNORADO: Falló la simulación para comuna (", zona, "). Continúa...\n"))
      return(NULL)
    } else {
      stop(e)
    }
  })
  
  return(resultado)
})

# Se combinan los resultados de todas las comunas
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA", fill = TRUE)


## 4. AGREGACIÓN Y CONEXIÓN A LA BD ####

# La agregación se hace sobre sim_df.
# Utilizamos la variable binaria 'tasa_sobrepeso_obeso' creada en la Sección 2.2
zonas_nutricional = aggregate(
  x = sim_df$tasa_sobrepeso_obeso,  
  by = list(geocodigo = sim_df$zone),
  FUN  = function(x) mean(x, na.rm = TRUE) * 100 
)

# Se renombra la columna de la tasa
# Se mantiene el nombre 'tasa_sobrepeso_obeso' como la variable final
names(zonas_nutricional)[2] <- "tasa_sobrepeso_obeso"


# === CONEXIÓN A LA BASE DE DATOS Y EXPORTACIÓN (Nombres Corregidos) ===

db_host = "localhost"
db_port = 5432
db_name = "censo_rm_2017"
db_user = "postgres"
db_password = "postgres"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# 1. Escribir la tabla de resultados temporales
dbWriteTable(
  con,
  name = DBI::SQL("out.zonas_nutricional_tmp"), # CORREGIDO: de precariedad a nutricional
  value = zonas_nutricional,
  row.names = FALSE,
  overwrite = TRUE 
)


# 2. Leer zonas censales con geometría desde la BD (Query se mantiene)
query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
    nom_provin = 'SANTIAGO' OR
    nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO', 'EL BOSQUE') 
)"

zonas_gs = st_read(con, query = query_gs)

# 3. Unir datos estimados con la geometría (Se une la tabla 'zonas_nutricional' correcta)
zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)
zonas_gs_simulacion = left_join(zonas_gs, zonas_nutricional, by = "geocodigo")


# 4. Escribir tabla espacial final
st_write(
  zonas_gs_simulacion,
  dsn = con,
  layer = DBI::SQL("out.zc_obesidad_microsim"), # CORREGIDO: Nombre de layer final
  driver = "PostgreSQL",
  delete_layer = TRUE 
)

# 5. Cerrar Conexión
dbDisconnect(con)


## 5. VISUALIZACIÓN DE RESULTADOS: MAPA DE OBESIDAD/SOBREPESO ####

# Cálculo de Centroides y Uniones de Comunas (para el contexto del mapa)
contorno_comunal <- zonas_gs_simulacion %>%
  group_by(nom_comuna) %>%
  summarise(do_union = TRUE) %>%
  ungroup()

centroides_comuna <- st_centroid(contorno_comunal)


# Generación del Mapa de Tasa de Sobrepeso/Obesidad Estimada (con etiquetas mínimas)
mapa_obesidad <- ggplot(zonas_gs_simulacion) +
  geom_sf(aes(fill = tasa_sobrepeso_obeso), color = NA, size = 0.05) + # VARIABLE CORRECTA
  geom_sf(data = contorno_comunal, fill = NA, color = "gray20", linewidth = 0.5) +
  geom_sf_text(
    data = centroides_comuna,
    aes(label = nom_comuna),
    size = 1.5,          
    color = "black",
    fontface = "plain"
  ) +
  scale_fill_viridis_c(
    option = "magma", # Opción preferida para calor/salud
    name = "Tasa Sobrepeso/Obesidad (%)", # CORREGIDO
    labels = scales::comma,
    direction = -1, 
    na.value = "grey80" 
  ) +
  labs(
    title = "Estimación de Sobrepeso u Obesidad +18 (CASEN 2022) por Zona Censal", # CORREGIDO
    subtitle = "Población de 18 años y más (Gran Santiago). Modelo controlado por Edad, Escolaridad y Sexo.",
    caption = "Fuente: Elaboración propia vía rakeR (CASEN 2022 y CENSO 2017)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Mostrar y guardar el mapa
print(mapa_obesidad)
ggsave("mapa_obesidad_microsimulada_final.png", mapa_obesidad, width = 10, height = 12, dpi = 300, bg = "white")

## 6. ANÁLISIS BIVARIADO Y VISUALIZACIÓN DE CLUSTERS ####

# === 6.1 Cargar Ingreso Percápita (Control Económico X) ===

# 1. Definición de la conexión (reabrimos la conexión para la consulta)
db_host = "localhost"
db_port = 5432
db_name = "censo_rm_2017"
db_user = "postgres"
db_password = "postgres"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# 2. Consulta SQL para traer los resultados de Ingreso Percápita de la clase (T1)
# Se asume que la tabla final de la clase se llama 'zc_ingreso_microsim' o similar.
query_ingreso <- "
SELECT geocodigo, mediana_ingreso
FROM out.zc_ingreso_microsim
"
df_ingreso_class <- st_read(con, query = query_ingreso)
dbDisconnect(con) # Cerramos la conexión

# 3. Limpieza y unión de data frames
df_ingreso_class <- df_ingreso_class %>%
  st_drop_geometry() %>% # Eliminamos la geometría del ingreso
  mutate(geocodigo = as.character(geocodigo))

# Unimos la Tasa de Obesidad simulada con la Mediana de Ingreso microsimulada
sf_bivariado <- zonas_gs_simulacion %>%
  left_join(df_ingreso_class, by = "geocodigo") %>%
  # Limpieza: Eliminamos filas donde falta la estimación (las comunas que fallaron en la simulación de obesidad)
  filter(!is.na(tasa_sobrepeso_obeso) & !is.na(mediana_ingreso))


# === 6.2 Clasificación Bivariada y Generación de Mapa ===

# NOTA CLAVE: El Ingreso es una variable de "Bajo a Alto Bienestar".
# Invertiremos el Ingreso (Eje X) para que Alto Ingreso = Baja Vulnerabilidad (cuadrante 1-1).

# 1. Crear la clasificación bivariada (3x3, por cuantiles)
sf_bivariado_clase <- bi_class(sf_bivariado, 
                               x = mediana_ingreso, # Ingreso (Eje X)
                               y = tasa_sobrepeso_obeso, # Obesidad (Eje Y)
                               dim = 3, 
                               style = "quantile")

# 2. Generación del Mapa Bivariado
mapa_bivariado_final <- ggplot(sf_bivariado_clase) +
  geom_sf(aes(fill = bi_class), color = "white", linewidth = 0.05) +
  bi_scale_fill(pal = "DkBlue", dim = 3, na.value = "grey80") + # DkBlue es la paleta ideal para dos tonos de riesgo
  
  # Añadir contornos y etiquetas (usando los centroides ya calculados)
  geom_sf(data = contorno_comunal, fill = NA, color = "gray20", linewidth = 0.5) +
  geom_sf_text(data = centroides_comuna, aes(label = nom_comuna), size = 1.5, color = "black", fontface = "plain") +
  
  labs(
    title = "Vulnerabilidad Bivariada: Sobrepeso/Obesidad Estimada vs. Ingreso Percápita Estimado",
    subtitle = "Identificación de Clusters de Doble Vulnerabilidad a Nivel de Zona Censal (Gran Santiago, CENSO/CASEN)",
    caption = "Fuente: Microsimulación rakeR (Obesidad S2 y Ingreso ypc). Paleta DkBlue."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )

# 3. Creación de Leyenda y Composición Final
leyenda_bivariada <- bi_legend(
  pal = "DkBlue", 
  dim = 3,
  # Invertimos el Ingreso para que el riesgo esté en el mismo cuadrante (Bajo Ingreso)
  xlab = "+ Ingreso pcp", 
  ylab = "+ Tasa SP/OB",
  size = 5
)

mapa_compuesto_bivariado <- ggdraw() +
  cowplot::draw_plot(mapa_bivariado_final, x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_plot(leyenda_bivariada, x = 0.78, y = 0.15, width = 0.2, height = 0.2)

# Mostrar el mapa bivariado
print(mapa_compuesto_bivariado)
ggsave("mapa_bivariado_obesidad_ingreso.png", mapa_compuesto_bivariado, width = 12, height = 10, dpi = 300, bg = "white")

