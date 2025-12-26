# ==============================================================================
# TRABAJO FINAL: INTELIGENCIA DE DATOS GEOESPACIALES - LA FLORIDA
# OBJETIVO: OPTIMIZACIÓN DE LOCALIZACIÓN COMERCIAL (PASTELERÍA)
# ==============================================================================

rm(list = ls())
library(sf)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. CARGA DE BASES DE DATOS E INDICADORES TERRITORIALES
# ------------------------------------------------------------------------------

# Carga de cartografía censal con datos de población
zonas_florida <- st_read("data/mapa_florida_poblacion.geojson") %>% 
  st_transform(32719) # Transformamos a metros para cálculos espaciales

# Carga de resultados de microsimulación (Tarea 2)
casen_pred <- readRDS("data/predicciones_gasto.rds")

# ------------------------------------------------------------------------------
# 2. CÁLCULO DE DEMANDA POTENCIAL (Punto 1 del Enunciado)
# ------------------------------------------------------------------------------

# Cálculo del Gasto Promedio a partir de las predicciones del modelo (Indicador Territorial)
gasto_medio <- mean(as.numeric(casen_pred$gasto_estimado), na.rm = TRUE)

# Integración: Población por zona * Gasto estimado individual
zonas_florida <- zonas_florida %>%
  mutate(demanda_potencial = poblacion * gasto_medio)

# ------------------------------------------------------------------------------
# 3. ANÁLISIS DE LA OFERTA EXISTENTE (Punto 2 del Enunciado)
# ------------------------------------------------------------------------------

# Carga de Manto de accesibilidad (Buffers de 10 min calculados en Python/Colab) de mis 63 puntos de oferta encontrados
manto_63 <- st_read("data/manto_oferta_florida.geojson") %>% 
  st_transform(32719)

# ------------------------------------------------------------------------------
# 4. DIAGNÓSTICO DE BRECHAS TERRITORIALES (Punto 3 del Enunciado)
# ------------------------------------------------------------------------------

# Operación espacial: Restamos el área de influencia de la competencia al mapa de demanda
brecha_territorial <- st_difference(zonas_florida, st_union(manto_63))

# ------------------------------------------------------------------------------
# 5. MODELO DE OPTIMIZACIÓN DE LOCALIZACIÓN (Punto 5 del Enunciado)
# ------------------------------------------------------------------------------

# Criterio: Selección de la zona con mayor demanda capturable desatendida
ubicacion_propuesta <- brecha_territorial %>%
  arrange(desc(demanda_potencial)) %>%
  slice(1) # Seleccionamos la zona con el valor más alto


# ------------------------------------------------------------------------------
# SECCIÓN 6: EXTRACCIÓN DE COORDENADAS PARA GOOGLE MAPS
# ------------------------------------------------------------------------------

centroide_optimo <- st_centroid(st_transform(ubicacion_propuesta, 4326))
coords <- st_coordinates(centroide_optimo)

cat("\n--- RESULTADOS PARA EL TRABAJO FINAL ---\n")
cat("Gasto promedio por persona: $", round(gasto_medio), "\n")
cat("Zona Censal seleccionada (ID):", ubicacion_propuesta$geocodigo, "\n")
cat("Demanda potencial en la zona: $", round(ubicacion_propuesta$demanda_potencial), "\n")
cat("Coordenadas finales (Lat, Lon):", coords[2], ",", coords[1], "\n")
cat("----------------------------------------\n")

# ------------------------------------------------------------------------------
# SECCIÓN 7: PERFIL SOCIOECONÓMICO DEL PÚBLICO OBJETIVO (PUNTO 6)
# ------------------------------------------------------------------------------

perfil_resumen <- casen_pred %>% 
  filter(e9com_cod == 13110) %>%
  filter(!is.na(ytotcorh)) # Aseguramos que tengan ingreso reportado

cat("--- PERFIL DEL PÚBLICO OBJETIVO (FINAL) ---\n")

ing_h <- mean(perfil_resumen$ytotcorh, na.rm = TRUE)
if(is.nan(ing_h)) ing_h <- mean(perfil_resumen$ytotcor, na.rm = TRUE) * 2.5 

edad_v <- mean(perfil_resumen$edad[perfil_resumen$edad >= 18], na.rm = TRUE)
esc_v <- mean(perfil_resumen$esc[perfil_resumen$esc >= 0], na.rm = TRUE)
gasto_v <- mean(perfil_resumen$gasto_estimado, na.rm = TRUE)

# IMPRESIÓN DE RESULTADOS SOCIODEMOGRÁFICOS
cat("- Ingreso promedio del Hogar: $", round(ing_h), "\n")
cat("- Edad promedio (adultos):", round(edad_v), "años\n")
cat("- Años de escolaridad:", round(esc_v), "años\n")
cat("- Gasto estimado por persona: $", round(gasto_v), "\n")

# ------------------------------------------------------------------------------
# SECCIÓN 8: GENERACIÓN DE LÁMINAS INDIVIDUALES (PRODUCTOS FINALES)
# ------------------------------------------------------------------------------

# --- PRODUCTO 1: MAPA DEMANDA POTENCIAL ---
mapa_gasto <- ggplot(zonas_florida) +
  geom_sf(aes(fill = demanda_potencial), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", 
                       name = "Gasto Total Estimado ($)",
                       labels = function(x) paste0("$", format(x, big.mark = ".", scientific = FALSE))) +
  labs(title = "Sección 1: Mapa de Calor de Gasto",
       subtitle = "Demanda Potencial por Zona Censal en La Florida",
       caption = paste("Basado en Microsimulación (Gasto Promedio: $", round(gasto_medio), ")")) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

print(mapa_gasto)
ggsave("lamina1_mapa_gasto.png", mapa_gasto, width = 10, height = 8, dpi = 300)

# --- PRODUCTO 2: MAPA OFERTA ACTUAL ---
mapa_oferta <- ggplot() +
  geom_sf(data = zonas_florida, fill = "#f0f0f0", color = "white", size = 0.1) +
  geom_sf(data = manto_63, fill = "red", alpha = 0.2, color = "red", size = 0.2) +
  labs(title = "Sección 2: Análisis de la Oferta Actual",
       subtitle = "Áreas de influencia de 63 locales (Manto de 10 minutos)",
       caption = "Fuente: OpenStreetMap (OSMNX) + Buffers de accesibilidad") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

print(mapa_oferta)
ggsave("lamina2_mapa_oferta.png", mapa_oferta, width = 10, height = 8, dpi = 300)

# --- PRODUCTO 3: MAPA BRECHA Y OPTIMIZACION ---
propuesta_ganadora <- brecha_territorial %>% arrange(desc(demanda_potencial)) %>% slice(1)

mapa_final_ggplot <- ggplot() +
  geom_sf(data = zonas_florida, aes(fill = demanda_potencial), color = "white", alpha = 0.5, size = 0.1) +
  scale_fill_viridis_c(option = "magma", name = "Demanda ($)") +
  geom_sf(data = manto_63, fill = "red", alpha = 0.2, color = "red") +
  geom_sf(data = propuesta_ganadora, fill = "yellow", color = "black", size = 0.8) +
  labs(title = "Sección 3: Modelo de Optimización de Localización",
       subtitle = "Punto óptimo detectado en zona de alta demanda y nula cobertura",
       caption = "Criterio: Máxima Demanda Capturable desatendida") +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())

print(mapa_final_ggplot)
ggsave("lamina3_optimizacion.png", mapa_final_ggplot, width = 10, height = 8, dpi = 300)

