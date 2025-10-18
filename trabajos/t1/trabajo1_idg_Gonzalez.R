# =============================================================================
# ANÁLISIS ESPACIAL: DOBLE VULNERABILIDAD EN ADULTOS MAYORES (VERSIÓN FINAL)
# Código base mantenido, solo se modifica el Punto 9.
# =============================================================================

#------------------------------------------------------------------------------
# 1. CARGAR LIBRERÍAS ---------------------------------------------------------
#------------------------------------------------------------------------------
if (!require(DBI)) install.packages("DBI")
if (!require(RPostgres)) install.packages("RPostgres")
if (!require(sf)) install.packages("sf")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(biscale)) install.packages("biscale")
if (!require(cowplot)) install.packages("cowplot")
if (!require(viridis)) install.packages("viridis")
if (!require(ggrepel)) install.packages("ggrepel")

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(dplyr)
library(biscale)
library(cowplot)
library(viridis)
library(ggrepel)

#------------------------------------------------------------------------------
# 2. CONEXIÓN A LA BASE DE DATOS ----------------------------------------------
#------------------------------------------------------------------------------

cat("Conectando a la base de datos...\n")
con <- dbConnect(
  Postgres(),
  dbname= "censo_rm_2017",
  host= "localhost",
  port= 5432,
  user= "postgres",
  password = "postgres"
)
cat("✓ Conexión exitosa\n")

#------------------------------------------------------------------------------
# 3. CONSULTA DE INDICADORES --------------------------------------------------
#-----------------------------------------------------------------------------

cat("Extrayendo indicadores...\n")

sql_indicadores <- "
SELECT
 z.geocodigo::double precision AS geocodigo,
 c.nom_comuna,
 -- % Adultos mayores que trabajan (p17 = 1)
 COALESCE(ROUND(
 COUNT(*) FILTER (WHERE p.p17 = 1) * 100.0 /
 NULLIF(COUNT(*), 0), 2), 0) AS porc_am_trabajando,
 -- % Hogares con AM en hacinamiento (ind_hacin_rec IN (2,3,4))
 COALESCE(ROUND(
 COUNT(DISTINCT h.hogar_ref_id) FILTER (WHERE v.ind_hacin_rec IN (2,3,4)) * 100.0 /
 NULLIF(COUNT(DISTINCT h.hogar_ref_id), 0), 2), 0) AS porc_hacinamiento_am,
 COUNT(*) as total_adultos_mayores,
 COUNT(DISTINCT h.hogar_ref_id) as total_hogares_con_am
FROM public.personas AS p
JOIN public.hogares AS h ON p.hogar_ref_id = h.hogar_ref_id
JOIN public.viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN public.comunas AS c ON z.codigo_comuna= c.codigo_comuna
JOIN public.provincias AS pr ON c.provincia_ref_id = pr.provincia_ref_id
WHERE p.p09 >= 65
 AND pr.nom_provincia = 'SANTIAGO'
GROUP BY z.geocodigo, c.nom_comuna;
"
df_indicadores <- dbGetQuery(con, sql_indicadores)
cat("✓ Indicadores extraídos:", nrow(df_indicadores), "zonas censales\n")

#------------------------------------------------------------------------------
# 4. CONSULTA DE GEOMETRÍA ----------------------------------------------------
#-----------------------------------------------------------------------------

cat("Extrayendo geometría...\n")
sql_geometria <- "
SELECT
 geocodigo::double precision AS geocodigo,
 geom
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO' AND urbano = 1;
"
sf_zonas <- st_read(con, query = sql_geometria)
cat("✓ Geometría extraída:", nrow(sf_zonas), "zonas urbanas\n")

# 4b. CONSULTA DE GEOMETRÍA DE COMUNAS PARA ETIQUETAS -------------------------
cat("Extrayendo geometría de comunas para etiquetas...\n")
sql_comunas <- "
SELECT nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas <- st_read(con, query = sql_comunas)
sf_comunas_centroides <- st_centroid(sf_comunas)
cat("✓ Geometría de comunas extraída:", nrow(sf_comunas), "comunas\n")

#------------------------------------------------------------------------------
# 5. UNIR DATOS Y GEOMETRÍA ---------------------------------------------------
#------------------------------------------------------------------------------

cat("Uniendo datos...\n")
sf_mapa <- merge(sf_zonas, df_indicadores, by = "geocodigo", all.x = FALSE)
cat("✓ Datos unidos:", nrow(sf_mapa), "registros finales\n")

#------------------------------------------------------------------------------
# 6. ANÁLISIS DESCRIPTIVO (Cálculos para mapas) -------------------------------
#------------------------------------------------------------------------------

mediana_trabajo <- median(sf_mapa$porc_am_trabajando, na.rm = TRUE)
mediana_hacinamiento <- median(sf_mapa$porc_hacinamiento_am, na.rm = TRUE)
limites_trabajo <- quantile(sf_mapa$porc_am_trabajando, c(0.05, 0.95), na.rm = TRUE)
limites_hacinamiento <- quantile(sf_mapa$porc_hacinamiento_am, c(0.05, 0.95), na.rm = TRUE)
cat("\n=== ANÁLISIS DESCRIPTIVO ===\n")
cat("Rango de % Trabajo:", range(sf_mapa$porc_am_trabajando, na.rm = TRUE), "\n")
cat("Rango de % Hacinamiento:", range(sf_mapa$porc_hacinamiento_am, na.rm = TRUE), "\n")
cat("Mediana Trabajo:", mediana_trabajo, "\n")
cat("Mediana Hacinamiento:", mediana_hacinamiento, "\n")

#------------------------------------------------------------------------------
# 7. MAPAS INDIVIDUALES -------------------------------------------------------
#------------------------------------------------------------------------------

cat("\nGenerando mapas individuales...\n")
# Mapa 1: % Adultos mayores que trabajan
map_trabajo <- ggplot(sf_mapa) +
  geom_sf(aes(fill = porc_am_trabajando), color = "white", size = 0.05) +
  scale_fill_viridis_c(
    option = "plasma", 
    name = "%",
    limits = limites_trabajo,
    na.value = "grey90"
  ) +
  labs(
    title = "Adultos Mayores que Trabajan (65+ años)", 
    subtitle = "% que trabajó por un pago la semana pasada"
  ) +
  theme_void() + theme(plot.title = element_text(face = "bold"))

# Mapa 2: % Hacinamiento en hogares con AM
map_hacinamiento <- ggplot(sf_mapa) +
  geom_sf(aes(fill = porc_hacinamiento_am), color = "white", size = 0.05) +
  scale_fill_viridis_c(
    option = "viridis", 
    name = "%",
    limits = limites_hacinamiento,
    na.value = "grey90"
  ) +
  labs(
    title = "Hacinamiento en Hogares con Adultos Mayores", 
    subtitle = "% de hogares con AM en viviendas con hacinamiento"
  ) +
  theme_void() + theme(plot.title = element_text(face = "bold"))

#------------------------------------------------------------------------------
# 8. GRÁFICO DE DISPERSIÓN ----------------------------------------------------
#-----------------------------------------------------------------------------

cat("Generando gráfico de dispersión...\n")
sf_mapa$cuadrante <- with(sf_mapa, ifelse(
  porc_am_trabajando >= mediana_trabajo & porc_hacinamiento_am >= mediana_hacinamiento, 
  'Q1: Alto trabajo / Alto hacinamiento',
  ifelse(porc_am_trabajando >= mediana_trabajo & porc_hacinamiento_am < mediana_hacinamiento, 
         'Q2: Alto trabajo / Bajo hacinamiento',
         ifelse(porc_am_trabajando < mediana_trabajo & porc_hacinamiento_am < mediana_hacinamiento, 
                'Q3: Bajo trabajo / Bajo hacinamiento',
                'Q4: Bajo trabajo / Alto hacinamiento'))))

grafico_dispersion <- ggplot(sf_mapa, aes(x = porc_am_trabajando, y = porc_hacinamiento_am)) +
  geom_point(aes(color = cuadrante), size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = mediana_trabajo, linetype = "dashed", color = "red") +
  geom_hline(yintercept = mediana_hacinamiento, linetype = "dashed", color = "red") +
  labs(x = "% Adultos Mayores que Trabajan", y = "% Hacinamiento en Hogares con AM",
       title = "Dispersión: Trabajo vs Hacinamiento en Adultos Mayores", color = "Cuadrante") +
  theme_minimal()

#------------------------------------------------------------------------------
# 9. MAPA BIVARIADO OPTIMIZADO (AJUSTES FINOS DE ETIQUETAS Y LEYENDA) ---------
#------------------------------------------------------------------------------

cat("Generando mapa bivariado optimizado (Ajustes de tamaño de etiquetas y leyenda)...\n")
# Crear clasificación bivariada
sf_mapa_bi <- bi_class(sf_mapa, 
                       x = porc_hacinamiento_am, 
                       y = porc_am_trabajando, 
                       dim = 3, 
                       style = "quantile")

# Filtrar comunas para el mapa (manteniendo la solución anterior)
comunas_a_eliminar <- c("LO BARNECHEA", "PUDAHUEL") 
sf_comunas_filtradas <- sf_comunas %>% filter(!nom_comuna %in% comunas_a_eliminar)
sf_centroides_filtrados <- sf_comunas_centroides %>% filter(!nom_comuna %in% comunas_a_eliminar)

# Crear el mapa bivariado
mapa_bivariado <- ggplot() +
  
  # 1. Capa base: zonas censales bivariadas (color = NA elimina líneas divisorias)
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA, size = 0.05) +
  
  # 2. Capa de bordes de comunas (USA EL OBJETO FILTRADO)
  geom_sf(data = sf_comunas_filtradas, fill = NA, color = "black", size = 0.3, alpha = 0.5) +
  
  # 3. Etiquetas de comunas centradas (AJUSTE CLAVE: size = 2.0)
  geom_sf_text(data = sf_centroides_filtrados, 
               aes(label = nom_comuna), 
               size = 2.0, # Reducido de 2.5 a 2.0
               color = 'black', 
               fontface = 'bold') +
  
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = "Doble Vulnerabilidad en Adultos Mayores: Distribución Urbana",
       subtitle = "Hacinamiento vs. Persistencia Laboral (65+ años) - Provincia de Santiago") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        plot.caption = element_text(hjust = 0.95, face = "italic"))

# Leyenda (AJUSTES CLAVE: size de texto y posición)
leyenda_bivariada <- bi_legend(
  pal = 'DkBlue', 
  dim = 3,
  xlab = '+ Hacinamiento', 
  ylab = '+ Trabajo',
  size = 5 # Reducido de 6 a 5 para el texto de la leyenda
)

# Composición final (Ajuste de posición de la leyenda en el plot)
mapa_final <- cowplot::ggdraw() +
  cowplot::draw_plot(mapa_bivariado, x = 0, y = 0, width = 1, height = 1) +
  # Ajuste de x e y para la leyenda
  cowplot::draw_plot(leyenda_bivariada, x = 0.78, y = 0.1, width = 0.2, height = 0.2) 

cat("✓ Bloque del Mapa Bivariado (Punto 9) ajustado para etiquetas y leyenda.\n")

#------------------------------------------------------------------------------
# 10. ANÁLISIS DE PATRONES ESPACIALES -----------------------------------------
#------------------------------------------------------------------------------

cat("Analizando patrones espaciales...\n")
zonas_criticas <- sf_mapa_bi %>%
  st_drop_geometry() %>%
  filter(bi_class %in% c('3-3', '3-2', '2-3')) %>%
  select(geocodigo, nom_comuna, porc_am_trabajando, porc_hacinamiento_am, bi_class) %>%
  arrange(desc(porc_am_trabajando + porc_hacinamiento_am))

cat("\n=== ZONAS CON DOBLE VULNERABILIDAD ===\n")
print(head(zonas_criticas, 10))

#-----------------------------------------------------------------------------
# 11. VISUALIZACIÓN -----------------------------------------------------------
#-----------------------------------------------------------------------------

cat("Mostrando resultados...\n")
print(map_trabajo)
print(map_hacinamiento)
print(grafico_dispersion)
print(mapa_final)

#------------------------------------------------------------------------------
# 12. GUARDAR RESULTADOS ------------------------------------------------------
#------------------------------------------------------------------------------

cat("Guardando resultados...\n")
ggsave("mapa_trabajo_am.png", map_trabajo, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("mapa_hacinamiento_am.png", map_hacinamiento, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("grafico_dispersion.png", grafico_dispersion, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("mapa_bivariado_final_optimizado.png", mapa_final, width = 12, height = 8, dpi = 300, bg = "white") 
write.csv(zonas_criticas, "zonas_doble_vulnerabilidad.csv", row.names = FALSE, fileEncoding = "UTF-8")

#-----------------------------------------------------------------------------
# 13. CERRAR CONEXIÓN ---------------------------------------------------------
#-----------------------------------------------------------------------------

dbDisconnect(con)
cat("✓ Conexión cerrada\n")

#------------------------------------------------------------------------------
# 14. RESUMEN FINAL -----------------------------------------------------------
#------------------------------------------------------------------------------

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("Archivos generados:\n")
cat("- mapa_trabajo_am.png\n")
cat("- mapa_hacinamiento_am.png\n")
cat("- grafico_dispersion.png\n")
cat("- mapa_bivariado_final_optimizado.png\n")
cat("- zonas_doble_vulnerabilidad.csv\n")
cat("\nTotal zonas analizadas:", nrow(sf_mapa), "\n")
cat("Zonas con doble vulnerabilidad:", nrow(zonas_criticas), "\n")

cat("\n¡Proceso completado exitosamente! 🎉\n")

