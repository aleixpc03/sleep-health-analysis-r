################################################################################
#           ANÁLISIS 6: Regularización Lasso y Ridge                           #
################################################################################

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(glmnet)  # Para Lasso y Ridge
})

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ANÁLISIS 6: Regularización Lasso y Ridge                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Definir rutas
data_folder <- "data"
output_folder <- data_folder

# Cargar datos
cat("📂 Cargando datos...\n")
data_path <- file.path(data_folder, "Sleep_health_and_lifestyle_dataset.csv")
df <- read_csv(data_path, show_col_types = FALSE) %>% clean_names()

# Limpiar categorías BMI
df <- df %>%
  mutate(bmi_category = case_when(
    bmi_category == "Normal Weight" ~ "Normal",
    bmi_category == "Normal" ~ "Normal",
    bmi_category == "Overweight" ~ "Overweight",
    bmi_category == "Obese" ~ "Obese",
    TRUE ~ bmi_category
  ))

# Crear variables numéricas y dummy
df <- df %>%
  mutate(
    bmi_numeric = case_when(
      bmi_category == "Normal" ~ 0,
      bmi_category == "Overweight" ~ 1,
      bmi_category == "Obese" ~ 2
    ),
    gender_male = ifelse(gender == "Male", 1, 0),
    has_disorder = ifelse(sleep_disorder == "None", 0, 1),
    has_insomnia = ifelse(sleep_disorder == "Insomnia", 1, 0),
    has_apnea = ifelse(sleep_disorder == "Sleep Apnea", 1, 0)
  )

cat("✓ Datos cargados:", nrow(df), "observaciones\n\n")

# ==============================================================================
# PASO 1: PREPARACIÓN DE DATOS PARA GLMNET
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 1: Preparación de datos para regularización\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔧 Preparando matriz de predictores y vector de respuesta...\n\n")

# Seleccionar variables predictoras
predictores <- c("sleep_duration", "physical_activity_level", "stress_level",
                 "heart_rate", "daily_steps", "gender_male", "bmi_numeric", 
                 "has_disorder")

# Crear matriz X (predictores) y vector y (respuesta)
X <- as.matrix(df[, predictores])
y <- df$quality_of_sleep

cat("📊 Dimensiones de los datos:\n")
cat(sprintf("   • Número de observaciones: %d\n", nrow(X)))
cat(sprintf("   • Número de variables predictoras: %d\n", ncol(X)))
cat(sprintf("   • Variable respuesta: quality_of_sleep\n\n"))

# Dividir en train y test (80-20)
set.seed(42)
train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))
test_indices <- setdiff(1:nrow(X), train_indices)

X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[test_indices, ]
y_test <- y[test_indices]

cat("📊 División train-test:\n")
cat(sprintf("   • Train: %d observaciones (80%%)\n", length(train_indices)))
cat(sprintf("   • Test:  %d observaciones (20%%)\n\n", length(test_indices)))

# ==============================================================================
# PASO 2: REGRESIÓN RIDGE (L2 Regularization)
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 2: Regresión Ridge (Regularización L2)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔍 ¿Qué es Ridge?\n")
cat("   Ridge añade una penalización L2 a los coeficientes: λ * Σ(β²)\n")
cat("   → Reduce el tamaño de los coeficientes sin eliminarlos\n")
cat("   → Útil cuando hay multicolinealidad\n")
cat("   → NUNCA elimina variables (coeficientes → 0 pero no = 0)\n\n")

cat("📈 Ajustando modelo Ridge con validación cruzada...\n\n")

# Cross-validation para encontrar mejor lambda
set.seed(42)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)

cat("✓ Validación cruzada completada\n\n")

cat("🎯 MEJOR VALOR DE LAMBDA (Ridge):\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("   • Lambda óptimo: %.6f\n", cv_ridge$lambda.min))
cat(sprintf("   • Lambda 1-SE:   %.6f (más parsimonioso)\n", cv_ridge$lambda.1se))
cat(sprintf("   • MSE mínimo:    %.6f\n\n", min(cv_ridge$cvm)))

# Ajustar modelo final con lambda óptimo
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)

# Coeficientes Ridge
coef_ridge <- as.matrix(coef(ridge_model))

cat("📋 COEFICIENTES RIDGE:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-30s %15s\n", "Variable", "Coeficiente"))
cat("────────────────────────────────────────────────────────────────\n")
for (i in 1:nrow(coef_ridge)) {
  var_name <- rownames(coef_ridge)[i]
  coef_val <- coef_ridge[i, 1]
  cat(sprintf("%-30s %+15.6f\n", var_name, coef_val))
}
cat("\n")

# Predicciones Ridge
pred_ridge_train <- predict(ridge_model, X_train)
pred_ridge_test <- predict(ridge_model, X_test)

# Métricas Ridge
rmse_ridge_train <- sqrt(mean((y_train - pred_ridge_train)^2))
rmse_ridge_test <- sqrt(mean((y_test - pred_ridge_test)^2))
r2_ridge_train <- 1 - sum((y_train - pred_ridge_train)^2) / sum((y_train - mean(y_train))^2)
r2_ridge_test <- 1 - sum((y_test - pred_ridge_test)^2) / sum((y_test - mean(y_test))^2)

cat("📊 RENDIMIENTO RIDGE:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12s %12s\n", "", "Train", "Test"))
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12.4f %12.4f\n", "RMSE", rmse_ridge_train, rmse_ridge_test))
cat(sprintf("%-20s %12.4f %12.4f\n", "R²", r2_ridge_train, r2_ridge_test))
cat("────────────────────────────────────────────────────────────────\n\n")

# ==============================================================================
# PASO 3: REGRESIÓN LASSO (L1 Regularization)
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 3: Regresión Lasso (Regularización L1)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔍 ¿Qué es Lasso?\n")
cat("   Lasso añade una penalización L1 a los coeficientes: λ * Σ|β|\n")
cat("   → Fuerza algunos coeficientes a ser EXACTAMENTE 0\n")
cat("   → Realiza SELECCIÓN DE VARIABLES automáticamente\n")
cat("   → Produce modelos más simples e interpretables\n\n")

cat("📈 Ajustando modelo Lasso con validación cruzada...\n\n")

# Cross-validation para encontrar mejor lambda
set.seed(42)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)

cat("✓ Validación cruzada completada\n\n")

cat("🎯 MEJOR VALOR DE LAMBDA (Lasso):\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("   • Lambda óptimo: %.6f\n", cv_lasso$lambda.min))
cat(sprintf("   • Lambda 1-SE:   %.6f (más parsimonioso)\n", cv_lasso$lambda.1se))
cat(sprintf("   • MSE mínimo:    %.6f\n\n", min(cv_lasso$cvm)))

# Ajustar modelo final con lambda óptimo
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)

# Coeficientes Lasso
coef_lasso <- as.matrix(coef(lasso_model))

cat("📋 COEFICIENTES LASSO:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-30s %15s %10s\n", "Variable", "Coeficiente", "Selec?"))
cat("────────────────────────────────────────────────────────────────\n")
for (i in 1:nrow(coef_lasso)) {
  var_name <- rownames(coef_lasso)[i]
  coef_val <- coef_lasso[i, 1]
  seleccionada <- if (abs(coef_val) > 1e-10) "✓" else "✗"
  cat(sprintf("%-30s %+15.6f      %s\n", var_name, coef_val, seleccionada))
}
cat("\n")

# Contar variables seleccionadas
n_selected <- sum(abs(coef_lasso[-1, 1]) > 1e-10)
cat(sprintf("💡 Lasso ha seleccionado %d de %d variables\n", n_selected, ncol(X)))
cat(sprintf("   → Eliminó %d variables (coeficiente = 0)\n\n", ncol(X) - n_selected))

# Predicciones Lasso
pred_lasso_train <- predict(lasso_model, X_train)
pred_lasso_test <- predict(lasso_model, X_test)

# Métricas Lasso
rmse_lasso_train <- sqrt(mean((y_train - pred_lasso_train)^2))
rmse_lasso_test <- sqrt(mean((y_test - pred_lasso_test)^2))
r2_lasso_train <- 1 - sum((y_train - pred_lasso_train)^2) / sum((y_train - mean(y_train))^2)
r2_lasso_test <- 1 - sum((y_test - pred_lasso_test)^2) / sum((y_test - mean(y_test))^2)

cat("📊 RENDIMIENTO LASSO:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12s %12s\n", "", "Train", "Test"))
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12.4f %12.4f\n", "RMSE", rmse_lasso_train, rmse_lasso_test))
cat(sprintf("%-20s %12.4f %12.4f\n", "R²", r2_lasso_train, r2_lasso_test))
cat("────────────────────────────────────────────────────────────────\n\n")

# ==============================================================================
# PASO 4: REGRESIÓN LINEAL CLÁSICA (OLS) PARA COMPARACIÓN
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 4: Regresión Lineal Clásica (OLS) - Comparación\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Crear dataframes para train y test
df_train <- data.frame(quality_of_sleep = y_train, X_train)
df_test <- data.frame(quality_of_sleep = y_test, X_test)

# Ajustar modelo OLS
ols_model <- lm(quality_of_sleep ~ ., data = df_train)

# Predicciones OLS
pred_ols_train <- predict(ols_model, df_train)
pred_ols_test <- predict(ols_model, df_test)

# Métricas OLS
rmse_ols_train <- sqrt(mean((y_train - pred_ols_train)^2))
rmse_ols_test <- sqrt(mean((y_test - pred_ols_test)^2))
r2_ols_train <- summary(ols_model)$r.squared
r2_ols_test <- 1 - sum((y_test - pred_ols_test)^2) / sum((y_test - mean(y_test))^2)

cat("📊 RENDIMIENTO OLS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12s %12s\n", "", "Train", "Test"))
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %12.4f %12.4f\n", "RMSE", rmse_ols_train, rmse_ols_test))
cat(sprintf("%-20s %12.4f %12.4f\n", "R²", r2_ols_train, r2_ols_test))
cat("────────────────────────────────────────────────────────────────\n\n")

# ==============================================================================
# PASO 5: COMPARACIÓN DE MODELOS
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 5: Comparación de los tres modelos\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 TABLA COMPARATIVA COMPLETA:\n")
cat("════════════════════════════════════════════════════════════════\n")
cat(sprintf("%-20s %12s %12s %12s\n", "Métrica", "OLS", "Ridge", "Lasso"))
cat("════════════════════════════════════════════════════════════════\n")
cat(sprintf("%-20s %12.4f %12.4f %12.4f\n", "RMSE Train", rmse_ols_train, rmse_ridge_train, rmse_lasso_train))
cat(sprintf("%-20s %12.4f %12.4f %12.4f\n", "RMSE Test", rmse_ols_test, rmse_ridge_test, rmse_lasso_test))
cat(sprintf("%-20s %12.4f %12.4f %12.4f\n", "R² Train", r2_ols_train, r2_ridge_train, r2_lasso_train))
cat(sprintf("%-20s %12.4f %12.4f %12.4f\n", "R² Test", r2_ols_test, r2_ridge_test, r2_lasso_test))
cat(sprintf("%-20s %12d %12d %12d\n", "Num. Variables", ncol(X), ncol(X), n_selected))
cat("════════════════════════════════════════════════════════════════\n\n")

# Determinar mejor modelo
mejor_modelo <- which.min(c(rmse_ols_test, rmse_ridge_test, rmse_lasso_test))
nombres_modelos <- c("OLS", "Ridge", "Lasso")

cat("🏆 MEJOR MODELO (según RMSE en test):\n")
cat(sprintf("   → %s con RMSE = %.4f\n\n", nombres_modelos[mejor_modelo], 
            min(rmse_ols_test, rmse_ridge_test, rmse_lasso_test)))

# Calcular diferencia de overfitting
overfit_ols <- rmse_ols_train - rmse_ols_test
overfit_ridge <- rmse_ridge_train - rmse_ridge_test
overfit_lasso <- rmse_lasso_train - rmse_lasso_test

cat("📉 ANÁLISIS DE SOBREAJUSTE (Train RMSE - Test RMSE):\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("   • OLS:   %.4f %s\n", overfit_ols, 
            ifelse(overfit_ols < 0, "✓ (generaliza bien)", "⚠️ (overfitting)")))
cat(sprintf("   • Ridge: %.4f %s\n", overfit_ridge,
            ifelse(overfit_ridge < 0, "✓ (generaliza bien)", "⚠️ (overfitting)")))
cat(sprintf("   • Lasso: %.4f %s\n\n", overfit_lasso,
            ifelse(overfit_lasso < 0, "✓ (generaliza bien)", "⚠️ (overfitting)")))

cat("💡 Nota: Un valor negativo indica que el modelo generaliza mejor\n")
cat("   en test que en train (puede ocurrir con muestras pequeñas)\n\n")

# ==============================================================================
# PASO 6: VISUALIZACIÓN - CURVAS DE LAMBDA
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 6: Visualización - Curvas de validación cruzada\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Generando gráficos de validación cruzada...\n\n")

# Gráfico combinado de CV para Ridge y Lasso
output_path_cv <- file.path(output_folder, "cv_ridge_lasso.png")
png(output_path_cv, width = 1400, height = 600, res = 120)

par(mfrow = c(1, 2))

# Ridge CV plot
plot(cv_ridge, main = "Validación Cruzada - Ridge (α=0)",
     xlab = "Log(λ)", ylab = "MSE")
abline(v = log(cv_ridge$lambda.min), col = "red", lty = 2, lwd = 2)
abline(v = log(cv_ridge$lambda.1se), col = "blue", lty = 2, lwd = 2)
legend("topleft", legend = c("λ min", "λ 1-SE"), 
       col = c("red", "blue"), lty = 2, lwd = 2)

# Lasso CV plot
plot(cv_lasso, main = "Validación Cruzada - Lasso (α=1)",
     xlab = "Log(λ)", ylab = "MSE")
abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2, lwd = 2)
abline(v = log(cv_lasso$lambda.1se), col = "blue", lty = 2, lwd = 2)
legend("topleft", legend = c("λ min", "λ 1-SE"), 
       col = c("red", "blue"), lty = 2, lwd = 2)

dev.off()

cat("✓ Gráfico guardado en:", output_path_cv, "\n\n")

# ==============================================================================
# PASO 7: VISUALIZACIÓN - EVOLUCIÓN DE COEFICIENTES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 7: Evolución de coeficientes con lambda\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Generando gráficos de trayectorias de coeficientes...\n\n")

# Ajustar modelos con secuencia de lambdas
ridge_path <- glmnet(X_train, y_train, alpha = 0)
lasso_path <- glmnet(X_train, y_train, alpha = 1)

output_path_coef <- file.path(output_folder, "coef_paths_ridge_lasso.png")
png(output_path_coef, width = 1400, height = 600, res = 120)

par(mfrow = c(1, 2))

# Ridge coefficient paths
plot(ridge_path, xvar = "lambda", main = "Trayectorias Ridge (α=0)",
     xlab = "Log(λ)", ylab = "Coeficientes", lwd = 2)
abline(v = log(cv_ridge$lambda.min), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "λ óptimo", col = "red", lty = 2, lwd = 2)

# Lasso coefficient paths
plot(lasso_path, xvar = "lambda", main = "Trayectorias Lasso (α=1)",
     xlab = "Log(λ)", ylab = "Coeficientes", lwd = 2)
abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "λ óptimo", col = "red", lty = 2, lwd = 2)

dev.off()

cat("✓ Gráfico guardado en:", output_path_coef, "\n\n")

# ==============================================================================
# PASO 8: COMPARACIÓN VISUAL DE COEFICIENTES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 8: Comparación visual de coeficientes\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Generando gráfico comparativo de coeficientes...\n\n")

# Preparar datos para comparación
coef_ols <- coef(ols_model)[-1]  # Excluir intercepto
coef_comparison <- data.frame(
  Variable = names(coef_ols),
  OLS = coef_ols,
  Ridge = as.vector(coef_ridge[-1, 1]),
  Lasso = as.vector(coef_lasso[-1, 1])
) %>%
  pivot_longer(cols = c(OLS, Ridge, Lasso), 
               names_to = "Modelo", 
               values_to = "Coeficiente") %>%
  mutate(Variable_limpia = case_when(
    Variable == "sleep_duration" ~ "Duración sueño",
    Variable == "physical_activity_level" ~ "Actividad física",
    Variable == "stress_level" ~ "Nivel estrés",
    Variable == "heart_rate" ~ "Frecuencia cardíaca",
    Variable == "daily_steps" ~ "Pasos diarios",
    Variable == "gender_male" ~ "Género (M)",
    Variable == "bmi_numeric" ~ "BMI",
    Variable == "has_disorder" ~ "Trastorno",
    TRUE ~ Variable
  ))

# Crear gráfico
p <- ggplot(coef_comparison, aes(x = Variable_limpia, y = Coeficiente, 
                                  fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("OLS" = "#3498db", "Ridge" = "#e74c3c", 
                                "Lasso" = "#2ecc71")) +
  coord_flip() +
  labs(
    title = "Comparación de Coeficientes: OLS vs Ridge vs Lasso",
    subtitle = "Regularización reduce magnitud de coeficientes",
    x = "",
    y = "Valor del coeficiente",
    fill = "Modelo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

output_path_comp <- file.path(output_folder, "comparacion_coeficientes.png")
ggsave(output_path_comp, p, width = 12, height = 6, dpi = 300)

cat("✓ Gráfico guardado en:", output_path_comp, "\n\n")

# ==============================================================================
# PASO 9: CONCLUSIONES FINALES
# ==============================================================================
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    CONCLUSIONES FINALES                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("✅ RESUMEN DEL ANÁLISIS:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("🔍 DIFERENCIAS CLAVE:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("1. OLS (Regresión Lineal Clásica):\n")
cat("   • No aplica regularización\n")
cat("   • Mantiene todas las variables\n")
cat(sprintf("   • RMSE Test: %.4f\n", rmse_ols_test))
cat(sprintf("   • R² Test: %.4f\n\n", r2_ols_test))

cat("2. Ridge (Regularización L2):\n")
cat("   • Penaliza la suma de cuadrados de coeficientes\n")
cat("   • Reduce magnitud pero NO elimina variables\n")
cat(sprintf("   • Lambda óptimo: %.6f\n", cv_ridge$lambda.min))
cat(sprintf("   • RMSE Test: %.4f\n", rmse_ridge_test))
cat(sprintf("   • R² Test: %.4f\n\n", r2_ridge_test))

cat("3. Lasso (Regularización L1):\n")
cat("   • Penaliza la suma de valores absolutos\n")
cat(sprintf("   • Elimina %d variables (coeficiente = 0)\n", ncol(X) - n_selected))
cat(sprintf("   • Selecciona %d variables importantes\n", n_selected))
cat(sprintf("   • Lambda óptimo: %.6f\n", cv_lasso$lambda.min))
cat(sprintf("   • RMSE Test: %.4f\n", rmse_lasso_test))
cat(sprintf("   • R² Test: %.4f\n\n", r2_lasso_test))

cat("🏆 RECOMENDACIÓN:\n")
cat("────────────────────────────────────────────────────────────────\n")

if (mejor_modelo == 1) {
  cat("   → OLS es suficiente para este problema\n")
  cat("   → Los datos no presentan multicolinealidad severa\n")
  cat("   → No hay beneficio claro en regularización\n")
} else if (mejor_modelo == 2) {
  cat("   → Ridge es el mejor modelo\n")
  cat("   → Ayuda con la multicolinealidad\n")
  cat("   → Mejora la capacidad de generalización\n")
} else {
  cat("   → Lasso es el mejor modelo\n")
  cat("   → Proporciona selección automática de variables\n")
  cat("   → Modelo más simple e interpretable\n")
  cat(sprintf("   → Solo necesitas %d variables en lugar de %d\n", n_selected, ncol(X)))
}

cat("\n💡 VARIABLES SELECCIONADAS POR LASSO:\n")
cat("────────────────────────────────────────────────────────────────\n")
vars_lasso <- rownames(coef_lasso)[-1][abs(coef_lasso[-1, 1]) > 1e-10]
vars_lasso_limpio <- case_when(
  vars_lasso == "sleep_duration" ~ "Duración del sueño",
  vars_lasso == "physical_activity_level" ~ "Actividad física",
  vars_lasso == "stress_level" ~ "Nivel de estrés",
  vars_lasso == "heart_rate" ~ "Frecuencia cardíaca",
  vars_lasso == "daily_steps" ~ "Pasos diarios",
  vars_lasso == "gender_male" ~ "Género (Masculino)",
  vars_lasso == "bmi_numeric" ~ "Categoría BMI",
  vars_lasso == "has_disorder" ~ "Trastorno de sueño",
  TRUE ~ vars_lasso
)

for (i in seq_along(vars_lasso_limpio)) {
  cat(sprintf("   %d. %s\n", i, vars_lasso_limpio[i]))
}

cat("\n📁 ARCHIVOS GENERADOS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   1. cv_ridge_lasso.png - Curvas de validación cruzada\n")
cat("   2. coef_paths_ridge_lasso.png - Trayectorias de coeficientes\n")
cat("   3. comparacion_coeficientes.png - Comparación visual\n\n")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    ANÁLISIS 6 COMPLETADO                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("🎯 Regularización completada. ¿Otro análisis?\n\n")
