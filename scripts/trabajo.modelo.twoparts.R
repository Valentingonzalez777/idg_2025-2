rm(list = ls())
#-------------------------------
# 0. CARGA DE LIBRERÍAS 
#---------------------------------------------

library(pROC)
library(dplyr)
library(haven)

# --------------------------------------------------------------------------------------
# 1. INICIALIZACIÓN Y CARGA DE DATOS
# --------------------------------------------------------------------------------------

# --- CARGA DE DATOS EPF ---
# Se cargan las bases fundamentales para el análisis.
personas  <- read_dta("data/data_epf/base-personas-ix-epf-stata.dta")
gastos  <- read_dta("data/data_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/data_epf/base-cantidades-ix-epf-stata.dta")
ccif  <- read_dta("data/data_epf/ccif-ix-epf-stata.dta")

# --- DEFINICIÓN DE CCIFs FINALES ---
# Se definen los dos códigos de Pastelería que se deben sumar para capturar la demanda total.
ccif_pasteleria_list <- c("11.1.1.01.03", "11.1.1.02.16")

# --------------------------------------------------------------------------------------
# 2. LIMPIEZA INICIAL Y CÁLCULO DEL INGRESO PER CÁPITA
# --------------------------------------------------------------------------------------

# Limpieza y Creación de Ingreso Per Cápita
personas_gs <- personas %>%
  # Creación del ID único para las uniones (joins).
  mutate(id_persona = paste(as.character(folio), as.character(n_linea), sep = "_")) %>%
  # Filtro geográfico y limpieza de datos perdidos.
  filter(as.numeric(macrozona) == 2 & 
           !(edad %in% c(-99, -88, -77)) & 
           !(edue %in% c(-99, -88, -77)) & 
           ing_disp_hog_hd_ai >= 0) %>%
  mutate(ing_pc = ing_disp_hog_hd_ai / npersonas) 

# --------------------------------------------------------------------------------------
# 3. CÁLCULO DE LA VARIABLE DEPENDIENTE (Gasto Agregado)
# --------------------------------------------------------------------------------------

# Suma Agregada del Gasto en Pastelería (Variable dependiente)
gasto_final_por_persona <- cantidades %>%
  mutate(id_persona = paste(as.character(folio), as.character(n_linea), sep = "_")) %>%
  
  # ******************** SUMA DE CÓDIGOS ********************
  # Se filtran ambos CCIF (11.1.1.01.03 y 11.1.1.02.16) y se suman los montos para asegurar 
  # que la variable dependiente capte la DEMANDA TOTAL del rubro, resolviendo el problema de 
  # doble codificación.
  filter(gasto > 0, ccif %in% ccif_pasteleria_list) %>%
  group_by(id_persona) %>%
  summarise(gasto_pasteleria_total = sum(gasto, na.rm = TRUE)) 

# Creación de la Variable Binaria y Gasto Cero
personas_gs <- personas_gs %>%
  # Se unen los gastos calculados con la base sociodemográfica, llenando con 0 los casos sin gasto.
  left_join(gasto_final_por_persona, by = "id_persona") %>%
  mutate(gasto_pasteleria_total = ifelse(is.na(gasto_pasteleria_total), 0, gasto_pasteleria_total),
         # Variable dependiente binaria (decisión de gasto: 1 si gasta, 0 si no).
         incurre_gasto = ifelse(gasto_pasteleria_total > 0, 1, 0))

# --- VERIFICACIÓN DE LA MUESTRA FINAL ---

# Filtramos la base solo para quienes gastan realmente
base_gastadores_pasteleria <- subset(personas_gs, incurre_gasto == 1)

# Cálculo de Frecuencias consolidadas
frecuencias_consolidadas <- base_gastadores_pasteleria %>%
  summarise(
    n_personas_unicas = n_distinct(id_persona),
    gasto_total_consolidado = sum(gasto_pasteleria_total, na.rm = TRUE)
  )

cat("\n### 📊 Muestra Consolidada: Gasto Total en Pastelería (Final) ###\n")
print(frecuencias_consolidadas)

# ***************JUSTIFICACIÓN**********************************************************************

# --------------------------------------------------------------------------------------
# 4. CREACIÓN DE VARIABLES CATEGÓRICAS PARA EL MODELO
# --------------------------------------------------------------------------------------

# --- 4. AGRUPACIÓN DE ESCOLARIDAD ---
# Se discretiza la variable 'edue' (años de escolaridad) en tramos para analizar el efecto
# del nivel educativo sobre el gasto y la probabilidad de incurrir en él.
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)

# --------------------------------------------------------------------------------------
# 5. TRANSFORMACIONES Y PREPARACIÓN PARA EL MODELO LINEAL (tabla_gasto)
# --------------------------------------------------------------------------------------

# --- 5.1. BASE PARA MODELO CONTINUO (Solo quienes gastan) ---
# Se crea la sub-base 'tabla_gasto' excluyendo a quienes tienen gasto cero (incurre_gasto = 0).
tabla_gasto <- subset(personas_gs, gasto_pasteleria_total > 0)

# Se seleccionan solo las columnas que se usarán como variables predictoras y dependiente.
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_pasteleria_total", "grupo_escolaridad", "npersonas")]

# --- 5.2. TRANSFORMACIONES DE VARIABLES ---

# Se transforma la variable 'sexo' en factor con etiquetas claras ("Hombre", "Mujer") para el modelo.
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))

# Se aplica el logaritmo al Ingreso per cápita. Aunque en el modelo final usaremos 'ing_pc', 
# esta transformación es útil para el análisis exploratorio y la consistencia con la plantilla.
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)

# Transformación de la variable dependiente de gasto: Se aplica el logaritmo natural
# a (Gasto + 1). Esto linealiza la relación y estabiliza la varianza del gasto, 
# cumpliendo los supuestos de la regresión lineal.
tabla_gasto$log_gasto_pasteleria <- log(tabla_gasto$gasto_pasteleria_total + 1)

# Se crea la variable categórica 'rango_edad' para el modelo Lineal, similar a la agrupación de escolaridad.
tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)

# --------------------------------------------------------------------------------------
# 6. FILTRO DE OUTLIERS (Percentil 1 y 99)
# --------------------------------------------------------------------------------------
# Se calcula el percentil 1% y 99% para el Ingreso per cápita (ing_pc) y el Gasto Total en Pastelería.

# Cálculo de cuantiles para Ingreso y Gasto
q_ing <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99), na.rm = TRUE)
q_gasto <- quantile(tabla_gasto$gasto_pasteleria_total, probs = c(0.01, 0.99), na.rm = TRUE)

# Se excluyen de la base del modelo Lineal (tabla_gasto) todos los valores que estén fuera de este rango.
tabla_gasto <- subset(tabla_gasto,
                      ing_pc >= q_ing[1] & ing_pc <= q_ing[2] &
                        gasto_pasteleria_total >= q_gasto[1] & gasto_pasteleria_total <= q_gasto[2]
)

cat("\n✅ DATOS LISTOS PARA EL MODELADO ECONOMÉTRICO.\n")
cat("La base 'tabla_gasto' está limpia y transformada para el Modelo Lineal.\n")
cat("La base 'personas_gs' contiene la variable binaria para el Modelo Logit.\n")


# --- GRÁFICOS EXPLORATORIOS ---
# Suprime la notación científica para los números grandes en los ejes de los gráficos
options(scipen=999)


# 1. DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "#FFD700",
     main = "Distribución del Ingreso per Cápita (Compradores de Pastelería)", 
     xlab = "Ingreso per cápita (en pesos CLP)")
# 

# 2. DISTRIBUCIÓN DEL GASTO EN PASTELERÍA (Escala Original)
hist(tabla_gasto$gasto_pasteleria_total, breaks = 30, col = "#87CEFA",
     main = "Distribución del Gasto en Pastelería", 
     xlab = "Gasto Total en Pastelería (en pesos CLP)")
# 

# 3. GASTO EN FUNCIÓN DEL INGRESO (Relación Bivariada)
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_pasteleria_total,
     main = "Ingreso vs Gasto en Pastelería", 
     xlab = "Ingreso per cápita (en pesos CLP)", ylab = "Gasto en Pastelería",
     pch = 20, col = rgb(0, 0, 0.5, 0.4))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_pasteleria_total), col = "red", lwd = 3)
# 

# 4. BOXPLOT GASTO SEGÚN ESCOLARIDAD (Gasto promedio por nivel)
boxplot(gasto_pasteleria_total ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Pastelería según Escolaridad", 
        xlab = "Nivel de Escolaridad", ylab = "Gasto en Pastelería (en pesos CLP)",
        col = c("#FFB6C1", "#ADD8E6", "#90EE90", "#F08080"))
# 

# 5. GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_pasteleria_total,
     main = "Edad vs Gasto en Pastelería", 
     xlab = "Edad", ylab = "Gasto en Pastelería (en pesos CLP)",
     pch = 20, col = rgb(0.5, 0, 0, 0.4))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_pasteleria_total), col = "blue", lwd = 3)
#

# -------------------------------------------------------------
# 7. MODELADO ECONOMÉTRICO (REGENERACIÓN CON MODEL.MATRIX PARA ROBUSTEZ)
# -------------------------------------------------------------

# Base Logit (limpia)
# 🚨 Esta sección asegura que modelo_data esté disponible para el Logit. 🚨
modelo_data = subset(personas_gs, !is.na(edad) & !is.na(grupo_escolaridad) & !is.na(sexo))
modelo_data$sexo <- factor(modelo_data$sexo, labels = c("Hombre", "Mujer"))


# --- MODELO LOGIT (Probabilidad de Gasto) ---
# 1. Crear la matriz de diseño (dummies) para el entrenamiento EPF
X_logit_epf <- model.matrix(incurre_gasto ~ sexo + edad + grupo_escolaridad + ing_pc, data = modelo_data)
# 2. Entrenamos el Logit usando la matriz de diseño (sin el intercepto)
modelo_logit <- glm(modelo_data$incurre_gasto ~ X_logit_epf[, -1], data = modelo_data, family = binomial)
# 3. Guardar los nombres de las columnas para la predicción CASEN (OBJETO CRÍTICO)
columnas_logit_epf <- colnames(X_logit_epf)

cat("\n### 1. SUMMARY MODELO LOGIT (Probabilidad de Gasto en Pastelería) ###\n")
summary(modelo_logit)

# --- MODELO LINEAL (Monto del Gasto) ---
# Se mantiene la sintaxis original.
modelo_lineal = lm(log_gasto_pasteleria ~ edue + edad + npersonas, data = tabla_gasto)

cat("\n### 2. SUMMARY MODELO LINEAL (Monto del Gasto Total en Pastelería) ###\n")
summary(modelo_lineal)

# -------------------------------------------------------------
# 8. EVALUACIÓN DETALLADA DEL MODELO LOGIT (Justificación de AUC)
# -------------------------------------------------------------

# --- PREDICCIONES DE PROBABILIDAD ---
# Calculamos la probabilidad predicha de incurrir en gasto según el modelo
modelo_data$prob_predicha <- predict(modelo_logit, type = "response")

# --- EVALUACIÓN INICIAL CON UMBRAL POR DEFECTO (0.5) ---
# Clasificamos: si la probabilidad es ≥ 0.5 → predice que incurre en gasto
modelo_data$clasificacion_05 <- ifelse(modelo_data$prob_predicha >= 0.5, 1, 0)

cat("\n---- Evaluación con Umbral 0.5 (Por Defecto) ----\n")
conf_05 <- table(Real = modelo_data$incurre_gasto,
                 Predicha = modelo_data$clasificacion_05)
print(conf_05)

# Calculamos la precisión total (accuracy)
accuracy_05 <- mean(modelo_data$incurre_gasto == modelo_data$clasificacion_05)
cat("Accuracy:", accuracy_05, "\n")

# --- CÁLCULO DE MÉTRICAS DEL UMBRAL 0.5 ---
# Se calcula la sensibilidad y especificidad a este umbral.
# El error es que R no acepta el subíndice por string, por lo que convertimos la tabla a matriz
conf_05_matrix <- as.matrix(conf_05) 
TN <- conf_05_matrix["0", "0"]
FP <- conf_05_matrix["0", "1"]
TP <- conf_05_matrix["1", "1"]
FN <- conf_05_matrix["1", "0"]

# Especificidad (TN / (TN + FP)): Tasa de acierto de los que NO gastan.
especificidad_05 <- TN / (TN + FP)
cat("Especificidad (umbral 0.5):", especificidad_05, "\n")
# Sensibilidad (TP / (TP + FN)): Tasa de acierto de los que SÍ gastan.
sensibilidad_05 <- TP / (TP + FN)
cat("Sensibilidad (umbral 0.5):", sensibilidad_05, "\n")

cat("\n# COMENTARIO METODOLÓGICO (Justificación del AUC):\n")
cat("# El alto Accuracy junto con una baja Sensibilidad demuestran que el modelo predice\n")
cat("# bien la clase mayoritaria (No Gasto), pero falla en detectar a los compradores reales.\n")
cat("# Esto justifica la necesidad de utilizar el AUC y el umbral óptimo (Youden).\n")

# -------------------------------------------------------------
# 9. CURVA ROC Y CÁLCULO DEL UMBRAL ÓPTIMO (YOUDEN)
# -------------------------------------------------------------

# 9.1. Curva ROC y Área Bajo la Curva (AUC)
library(pROC)
roc_obj <- roc(modelo_data$incurre_gasto, modelo_data$prob_predicha)

cat("\n### 3. INDICADOR DE RENDIMIENTO: AUC ###\n")
cat("AUC:", auc(roc_obj), "\n")
# Opcional: para generar la gráfica de la curva ROC en RStudio
plot(roc_obj, col = "blue", main = "Curva ROC - Probabilidad de Gasto en Pastelería") 

# 9.2. Cálculo del Umbral Óptimo (Criterio de Youden)
# Maximiza la suma de sensibilidad y especificidad.
coords_opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

umbral_optimo <- as.numeric(coords_opt["threshold"])

cat("\n### 4. UMBRAL ÓPTIMO (YOUDEN) ###\n")
cat("Umbral óptimo:", umbral_optimo, "\n")
cat("Sensibilidad óptima (Youden):", coords_opt["sensitivity"][[1]], "\n")
cat("Especificidad óptima (Youden):", coords_opt["specificity"][[1]], "\n")

# --- EVALUACIÓN CON UMBRAL ÓPTIMO (Muestra la mejora) ---
modelo_data$clasificacion_optima <- ifelse(modelo_data$prob_predicha >= umbral_optimo, 1, 0)

cat("\n---- Evaluación con Umbral Óptimo (Mejor Balance) ----\n")
conf_opt <- table(Real = modelo_data$incurre_gasto,
                  Predicha = modelo_data$clasificacion_optima)
print(conf_opt)

# Cálculo explícito de la Sensibilidad final para comparación
conf_opt_matrix <- as.matrix(conf_opt)
TP_opt <- conf_opt_matrix["1", "1"]
FN_opt <- conf_opt_matrix["1", "0"]
sensibilidad_opt <- TP_opt / (TP_opt + FN_opt)
cat("Sensibilidad (final con Umbral Óptimo):", sensibilidad_opt, "\n")


#-------------------------------------------------------------------------
#### CASEN: Imputación del Gasto en Pastelería (CÓDIGO FINAL ROBUSTO) ####
#-------------------------------------------------------------------------

# 1. Cargamos la base que SÍ tiene las variables necesarias
casen <- readRDS("data/casen_rm.rds") 

# --- 1. PREPARACIÓN DE VARIABLES CASEN ---
casen$ing_pc <- casen$ypc
casen$edue   <- casen$esc      # Años de escolaridad (Variable continua)
casen$edad   <- casen$edad     # Edad (Variable continua)
casen$npersonas <- casen$numper # Número de personas (Variable que pidió el profe)

# Limpieza de NAs en las variables del modelo (Crucial para que no falle el predict)
casen <- casen %>%
  filter(!is.na(edad) & !is.na(ing_pc) & !is.na(edue) & !is.na(npersonas) & !is.na(sexo))

# Ajuste de niveles de factores para el Logit (que aún usa sexo y grupos)
casen$sexo <- factor(as.character(casen$sexo), levels = c(1, 2), labels = c("Hombre", "Mujer"))
casen$grupo_escolaridad <- cut(casen$edue, breaks = c(-Inf, 12, 14, 16, Inf), 
                               labels = levels(personas_gs$grupo_escolaridad), right = TRUE)

# --- 2. MODELO LOGIT Y CLASIFICACIÓN ---
X_logit_casen <- model.matrix(~ sexo + edad + grupo_escolaridad + ing_pc, data = casen)
X_logit_casen <- X_logit_casen[, columnas_logit_epf] # Alineamos con EPF

predicciones_limpias <- predict(modelo_logit, newdata = as.data.frame(X_logit_casen), type = "response")
casen$prob_predicha <- NA
casen$prob_predicha[as.numeric(rownames(X_logit_casen))] <- predicciones_limpias
casen$clasificacion <- ifelse(casen$prob_predicha >= umbral_optimo, 1, 0)

# Filtramos solo a los que el modelo dice que SÍ compran
casen_pred <- casen[casen$clasificacion == 1, ]

# --- 3. MODELO LINEAL (PREDICCIÓN SEGÚN PROFESOR) ---
# Aquí el modelo usa edue, edad y npersonas (todas están en casen_pred ahora)
casen_pred$log_gasto_estimado <- predict(modelo_lineal, newdata = casen_pred)
casen_pred$gasto_estimado <- exp(casen_pred$log_gasto_estimado) - 1

# Winzorización (Control de Outliers para el gráfico)
q_999_epf <- quantile(tabla_gasto$gasto_pasteleria_total, 0.999, na.rm = TRUE)
casen_pred$gasto_estimado_wins <- pmin(casen_pred$gasto_estimado, q_999_epf)

# --- 4. GRAFICAR (Validación Final) ---

options(scipen=999)

plot(density(tabla_gasto$gasto_pasteleria_total), col = "blue", lwd = 2,
     main = "Densidad: EPF vs CASEN imputado (Gasto en Pastelería)",
     xlab = "Gasto Mensual Estimado (CLP)")

lines(density(casen_pred$gasto_estimado_wins, na.rm = TRUE), col = "red", lwd = 2)

legend("topright", legend = c("EPF (Real)", "CASEN (Imputado)"), col = c("blue", "red"), lwd = 2)

# --- PASO FINAL: GUARDAR RESULTADOS PARA EL ANÁLISIS ESPACIAL ---

# Verificamos que el objeto exista y tenga la columna de gasto
if(exists("casen_pred")) {
  # Guardamos el objeto en la carpeta data
  # Usamos el formato .rds que es muy rápido y liviano
  saveRDS(casen_pred, "data/predicciones_gasto.rds")
  cat("✅ Resultados guardados exitosamente en data/predicciones_gasto.rds\n")
} else {
  stop("❌ El objeto 'casen_pred' no fue encontrado. Revisa los nombres de tu script.")
}









