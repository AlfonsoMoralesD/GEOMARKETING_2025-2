# --- CARGA DE LIBRERIAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(dplyr)
library(hexbin)
library(scales)

# --- CARGA DE DATOS EPF ---
personas    <- read_dta("Data/datos_epf/EPF/base-personas-ix-epf-stata.dta") 
gastos      <- read_dta("Data/datos_epf/EPF/base-gastos-ix-epf-stata.dta") 
cantidades  <- read_dta("Data/datos_epf/EPF/base-cantidades-ix-epf-stata.dta") 
ccif        <- read_dta("Data/datos_epf/EPF/ccif-ix-epf-stata.dta") 


# ============================================================
#   TAREA 2 - EPF: LIMPIEZA Y VARIABLES
# ============================================================

# --- FILTRADO: EPF nacional y datos válidos ---
valores_invalidos <- c(-99, -88, -77)

personas_gs <- subset(
  personas,
  !(edad %in% valores_invalidos) &
    !(edue %in% valores_invalidos) &
    ing_disp_hog_hd_ai >= 0
)

# --- VARIABLES DERIVADAS ---
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$id_persona <- paste0(personas_gs$folio, "-", personas_gs$n_linea, ".")
personas_gs$sexo <- ifelse(personas_gs$sexo == 1, "Hombre",
                           ifelse(personas_gs$sexo == 2, "Mujer", NA))
personas_gs$sexo <- factor(personas_gs$sexo, levels = c("Hombre", "Mujer"))

# ============================================================
#   SECCIÓN EPF: ARANCELES DE EDUCACIÓN (MENSUALIZADOS)
#   CCIF: varios códigos de arancel
# ============================================================

codigos_arancel_colegio <- c(
  "10.1.1.01.02",  
  "10.1.1.02.02",  
  "10.2.1.01.02",  
  "10.2.1.02.02"   
)

# --- FILTRO GASTO EN ARANCELES DE EDUCACIÓN ---
gastos_arancel <- subset(
  gastos,
  ccif %in% codigos_arancel_colegio
)

# --- NÚMERO DE REGISTROS EN ESTA CATEGORÍA---
n_registros_arancel <- nrow(gastos_arancel)
cat("Número de registros en ARANCELES DE EDUCACIÓN (EPF, todos):",
    n_registros_arancel, "\n")

# --- SUMA GASTO TOTAL EN ARANCELES POR HOGAR (FOLIO) ---
gasto_arancel_por_hogar <- aggregate(
  gasto ~ folio,
  data = gastos_arancel,
  sum
)
names(gasto_arancel_por_hogar)[2] <- "gasto_arancel"

# --- MERGE: Gasto con personas (cada persona hereda el gasto de su hogar) ---
personas_gs <- merge(
  personas_gs,
  gasto_arancel_por_hogar,
  by = "folio",
  all.x = TRUE
)

# Reemplazar NA por 0 (hogares sin gasto en aranceles)
personas_gs$gasto_arancel[is.na(personas_gs$gasto_arancel)] <- 0

# --- VARIABLE BINARIA DE GASTO EN ARANCELES ---
personas_gs$incurre_arancel <- ifelse(personas_gs$gasto_arancel > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right  = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes gastan en aranceles) ---
tabla_gasto <- subset(personas_gs, gasto_arancel > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc",
                               "gasto_arancel", "grupo_escolaridad",
                               "npersonas", "ecivil", "sprincipal", "mh09")]

# Limpieza de códigos inválidos en estas variables
tabla_gasto$ecivil[tabla_gasto$ecivil %in% valores_invalidos]        <- NA
tabla_gasto$sprincipal[tabla_gasto$sprincipal %in% valores_invalidos] <- NA
tabla_gasto$mh09[tabla_gasto$mh09 %in% valores_invalidos]            <- NA

# Factores
tabla_gasto$ecivil     <- factor(tabla_gasto$ecivil)
tabla_gasto$sprincipal <- factor(tabla_gasto$sprincipal)
tabla_gasto$mh09       <- factor(tabla_gasto$mh09)

# --- TRANSFORMACIONES DE VARIABLES ---
tabla_gasto$log_ing_pc        <- log(tabla_gasto$ing_pc + 1)
tabla_gasto$log_gasto_arancel <- log(tabla_gasto$gasto_arancel + 1)

tabla_gasto$rango_edad <- cut(
  tabla_gasto$edad,
  breaks = c(0, 29, 44, 64, Inf),
  labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)

# --- FILTRO DE OUTLIERS (percentil 1 y 99) ---
q_ing   <- quantile(tabla_gasto$ing_pc,         probs = c(0.01, 0.99), na.rm = TRUE)
q_gasto <- quantile(tabla_gasto$gasto_arancel,  probs = c(0.01, 0.99), na.rm = TRUE)

tabla_gasto <- subset(
  tabla_gasto,
  ing_pc         >= q_ing[1]   & ing_pc         <= q_ing[2] &
    gasto_arancel >= q_gasto[1] & gasto_arancel <= q_gasto[2]
)

# ============================================================
#   GRÁFICOS EXPLORATORIOS
# ============================================================

# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso per cápita (EPF)",
     xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN ARANCELES
hist(tabla_gasto$gasto_arancel, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Aranceles de Educación",
     xlab = "Gasto en aranceles")

# GASTO EN ARANCELES SEGÚN SEXO
boxplot(gasto_arancel ~ sexo, data = tabla_gasto,
        main = "Gasto en Aranceles según Sexo",
        xlab = "Sexo",
        col  = c("tomato", "lightgreen"))

# Edad vs Gasto en aranceles (hexágonos, amarillo → rojo)
ggplot(tabla_gasto, aes(x = edad, y = gasto_arancel)) +
  stat_binhex(bins = 50) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_fill_gradient(
    name  = "Frecuencia",
    low   = "yellow",
    high  = "red"
  ) +
  scale_y_continuous(labels = label_number(suffix = "MM", scale = 1e-6)) +
  labs(
    title = "Edad vs Gasto en Aranceles de Educación",
    x     = "Edad",
    y     = "Gasto en aranceles"
  ) +
  theme_minimal()

# Ingreso per cápita vs Gasto en aranceles (hexágonos, amarillo → rojo)
ggplot(tabla_gasto, aes(x = ing_pc, y = gasto_arancel)) +
  stat_binhex(bins = 50) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_fill_gradient(
    name  = "Frecuencia",
    low   = "yellow",
    high  = "red"
  ) +
  scale_x_continuous(labels = label_number(suffix = "MM", scale = 1e-6, big.mark = ".")) +
  scale_y_continuous(labels = label_number(suffix = "MM", scale = 1e-6, big.mark = ".")) +
  labs(
    title = "Ingreso vs Gasto en Aranceles de Educación",
    x     = "Ingreso per cápita",
    y     = "Gasto en aranceles"
  ) +
  theme_minimal()

# BOXPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_arancel ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Aranceles según Escolaridad",
        xlab = "Escolaridad",
        col  = "skyblue")

# ============================================================
#   MODELO DE DOS PARTES
# ============================================================

## -------------------------
## 1) MODELO LOGIT (incurre/no incurre en gasto)
## -------------------------

modelo_data <- subset(
  personas_gs,
  !is.na(edad) & !is.na(sexo) & !is.na(grupo_escolaridad) & !is.na(ing_pc)
)

modelo_data$log_ing_pc <- log(modelo_data$ing_pc + 1)

modelo_logit <- glm(
  incurre_arancel ~ sexo + edad + grupo_escolaridad + log_ing_pc,
  data   = modelo_data,
  family = binomial
)
summary(modelo_logit)

# Probabilidades predichas
modelo_data$prob_predicha <- predict(modelo_logit, type = "response")

# Umbral estándar 0.5
modelo_data$clasificacion_05 <- ifelse(modelo_data$prob_predicha >= 0.5, 1, 0)

cat("---- Evaluación con umbral 0.5 ----\n")
conf_05 <- table(
  Real     = modelo_data$incurre_arancel,
  Predicha = modelo_data$clasificacion_05
)
print(conf_05)

accuracy_05 <- mean(modelo_data$incurre_arancel == modelo_data$clasificacion_05)
cat("Accuracy (0.5):", accuracy_05, "\n")

# Curva ROC y AUC
roc_obj <- roc(modelo_data$incurre_arancel, modelo_data$prob_predicha)

plot(
  roc_obj,
  col = "blue",
  main = "Curva ROC - Logit Aranceles Educación",
  legacy.axes = TRUE,             # hace que el eje X vaya de 0 a 1
  xlab = "1 - Especificidad (FPR)",
  ylab = "Sensibilidad (TPR)"
)

auc_val <- auc(roc_obj)
cat("AUC:", auc_val, "\n")
# Umbral óptimo (Youden)
coords_opt <- coords(roc_obj, "best",
                     ret = c("threshold", "sensitivity", "specificity"))
umbral_optimo <- as.numeric(coords_opt["threshold"])
cat("Umbral óptimo:", umbral_optimo, "\n")
cat("Sensibilidad óptima:",  coords_opt["sensitivity"][[1]],  "\n")
cat("Especificidad óptima:", coords_opt["specificity"][[1]], "\n")

# Evaluación con umbral óptimo
modelo_data$clasificacion_optima <- ifelse(
  modelo_data$prob_predicha >= umbral_optimo, 1, 0
)

cat("\n---- Evaluación con umbral óptimo ----\n")
conf_opt <- table(
  Real     = modelo_data$incurre_arancel,
  Predicha = modelo_data$clasificacion_optima
)
print(conf_opt)

accuracy_opt <- mean(modelo_data$incurre_arancel == modelo_data$clasificacion_optima)
cat("Accuracy (óptimo):", accuracy_opt, "\n")

## -------------------------
## 2) MODELO LINEAL (monto dado que gasta)
## -------------------------

modelo_lineal <- lm(
  log_gasto_arancel ~ sexo + edad + grupo_escolaridad + log_ing_pc,
  data = tabla_gasto
)

summary(modelo_lineal)

# Métricas de desempeño del modelo lineal
resid_log <- residuals(modelo_lineal)
rmse_log  <- sqrt(mean(resid_log^2))
cat("RMSE (escala log):", rmse_log, "\n")

# Pasando a escala original de gasto (pesos)
pred_log   <- predict(modelo_lineal)                 # mismo data que modelo
obs_log    <- modelo_lineal$model$log_gasto_arancel  # vector usado en la estimación
obs_monto  <- exp(obs_log) - 1
pred_monto <- exp(pred_log) - 1

rmse_nivel <- sqrt(mean((obs_monto - pred_monto)^2))
cat("RMSE (escala original, pesos):", rmse_nivel, "\n")
# ============================================================
#   CASEN: LIMPIEZA Y VARIABLES EXPLICATIVAS
# ============================================================

# Ajusta la ruta según tu carpeta (Data/ vs data/)
casen <- readRDS("Data/casen_base_preprocesado.rds")

# Ingreso per cápita
casen$ing_pc <- casen$ypc

# Sexo igual que en EPF
casen$sexo <- ifelse(casen$sexo == 1, "Hombre",
                     ifelse(casen$sexo == 2, "Mujer", NA))
casen$sexo <- factor(casen$sexo, levels = c("Hombre", "Mujer"))

# Escolaridad en grupos (mismos cortes que EPF)
casen$grupo_escolaridad <- cut(
  casen$esc,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right  = TRUE
)

# Rango de edad (mismos cortes)
casen$rango_edad <- cut(
  casen$edad,
  breaks = c(0, 29, 44, 64, Inf),
  labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)

casen$log_ing_pc <- log(casen$ing_pc + 1)

# Quitamos casos con NA en variables clave
casen <- subset(
  casen,
  !is.na(ing_pc) &
    !is.na(edad) &
    !is.na(sexo) &
    !is.na(grupo_escolaridad)
)

# ============================================================
#   IMPUTACIÓN DEL GASTO EN ARANCELES EN CASEN
# ============================================================

# 1) Probabilidad de incurrir en gasto según modelo logit
casen$prob_predicha <- predict(
  modelo_logit,
  newdata = casen,
  type    = "response"
)

# Clasificación según umbral óptimo
casen$clasificacion <- ifelse(casen$prob_predicha >= umbral_optimo, 1, 0)
table(casen$clasificacion)

# 2) Para quienes se proyecta que sí gastan, predecimos el monto
casen_pred <- subset(casen, clasificacion == 1)

casen_pred$log_gasto_estimado <- predict(
  modelo_lineal,
  newdata = casen_pred
)

casen_pred$gasto_estimado <- exp(casen_pred$log_gasto_estimado) - 1

# Winzorización al 99.9% para controlar outliers
p99_9 <- quantile(casen_pred$gasto_estimado, 0.999, na.rm = TRUE)
casen_pred$gasto_estimado_wins <- pmin(casen_pred$gasto_estimado, p99_9)

# Estadísticas básicas de los gastos imputados
summary(casen_pred$gasto_estimado_wins)
sd(casen_pred$gasto_estimado_wins, na.rm = TRUE)

hist(casen_pred$gasto_estimado_wins, breaks = 50,
     main = "Distribución gasto imputado en aranceles de educación (CASEN)",
     xlab = "Gasto imputado (winsorizado)")

# Comparación EPF vs CASEN imputado
summary(tabla_gasto$gasto_arancel)
sd(tabla_gasto$gasto_arancel)

plot(
  density(tabla_gasto$gasto_arancel),
  col  = "blue", lwd = 2,
  main = "Densidad: EPF vs CASEN imputado (Aranceles educación)",
  xlab = "Gasto en aranceles de educación"
)
lines(
  density(casen_pred$gasto_estimado_wins),
  col  = "red", lwd = 2
)
legend("topright",
       legend = c("EPF", "CASEN imputado"),
       col    = c("blue", "red"),
       lwd    = 2
)
getwd()
file.exists("images/1.png")
list.files("images")
