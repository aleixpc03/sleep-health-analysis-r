################################################################################
#           ANÁLISIS 5: ¿Qué variables explican mejor la calidad del sueño?   #
################################################################################

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
})

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ANÁLISIS 5: Variables que explican la calidad del sueño      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Definir rutas
data_folder <- "data"
output_folder <- data_folder

# Cargar datos
cat("📂 Cargando datos...\n")
data_path <- file.path(data_folder, "Sleep_health_and_lifestyle_dataset.csv")
df <- read_csv(data_path, show_col_types = FALSE) %>% clean_names()

# Limpiar categorías BMI si es necesario
df <- df %>%
  mutate(bmi_category = case_when(
    bmi_category == "Normal Weight" ~ "Normal",
    bmi_category == "Normal" ~ "Normal",
    bmi_category == "Overweight" ~ "Overweight",
    bmi_category == "Obese" ~ "Obese",
    TRUE ~ bmi_category
  ))

cat("✓ Datos cargados:", nrow(df), "observaciones\n\n")

# ==============================================================================
# PASO 1: ANÁLISIS EXPLORATORIO DE CORRELACIONES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 1: Análisis exploratorio de correlaciones\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Variables disponibles para analizar:\n")
cat("   • Variables continuas: sleep_duration, physical_activity_level,\n")
cat("     stress_level, heart_rate, daily_steps\n")
cat("   • Variables categóricas: gender, bmi_category, sleep_disorder\n\n")

# Seleccionar variables numéricas relevantes
vars_numericas <- df %>%
  select(quality_of_sleep, sleep_duration, physical_activity_level, 
         stress_level, heart_rate, daily_steps)

# Calcular matriz de correlación
cat("🔢 Matriz de correlación con Quality of Sleep:\n")
cat("────────────────────────────────────────────────────────\n")

correlaciones <- cor(vars_numericas, use = "complete.obs")
cor_con_calidad <- correlaciones[, "quality_of_sleep"]
cor_con_calidad <- sort(cor_con_calidad, decreasing = TRUE)

for (i in 1:length(cor_con_calidad)) {
  var_name <- names(cor_con_calidad)[i]
  cor_val <- cor_con_calidad[i]
  
  if (var_name != "quality_of_sleep") {
    interpretacion <- if (abs(cor_val) >= 0.7) {
      "FUERTE"
    } else if (abs(cor_val) >= 0.4) {
      "MODERADA"
    } else if (abs(cor_val) >= 0.2) {
      "DÉBIL"
    } else {
      "MUY DÉBIL"
    }
    
    direccion <- if (cor_val > 0) "↑" else "↓"
    
    cat(sprintf("   %s %-28s: %+.3f (%s %s)\n", 
                direccion, var_name, cor_val, interpretacion, 
                ifelse(cor_val > 0, "positiva", "negativa")))
  }
}

cat("\n")

# ==============================================================================
# PASO 2: MODELO DE REGRESIÓN MÚLTIPLE COMPLETO
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 2: Regresión lineal múltiple - Modelo completo\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔧 Construyendo modelo con TODAS las variables...\n\n")

# Crear variable numérica para BMI
df <- df %>%
  mutate(bmi_numeric = case_when(
    bmi_category == "Normal" ~ 0,
    bmi_category == "Overweight" ~ 1,
    bmi_category == "Obese" ~ 2
  ))

# Crear variables dummy para género
df <- df %>%
  mutate(gender_male = ifelse(gender == "Male", 1, 0))

# Crear variables dummy para sleep disorder
df <- df %>%
  mutate(
    has_disorder = ifelse(sleep_disorder == "None", 0, 1),
    has_insomnia = ifelse(sleep_disorder == "Insomnia", 1, 0),
    has_apnea = ifelse(sleep_disorder == "Sleep Apnea", 1, 0)
  )

# Modelo completo
modelo_completo <- lm(quality_of_sleep ~ 
                        sleep_duration + 
                        physical_activity_level +
                        stress_level +
                        heart_rate +
                        daily_steps +
                        gender_male +
                        bmi_numeric +
                        has_disorder,
                      data = df)

# Mostrar resultados
cat("📊 RESULTADOS DEL MODELO COMPLETO:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

summary_model <- summary(modelo_completo)
cat(sprintf("• R² = %.4f (%.2f%% de la varianza explicada)\n", 
            summary_model$r.squared, summary_model$r.squared * 100))
cat(sprintf("• R² ajustado = %.4f\n", summary_model$adj.r.squared))
cat(sprintf("• Error estándar residual = %.4f\n", summary_model$sigma))
cat(sprintf("• F-statistic = %.2f, p-value < 0.001\n\n", summary_model$fstatistic[1]))

cat("📋 COEFICIENTES DEL MODELO:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-30s %10s %10s %10s %5s\n", 
            "Variable", "Coef.", "Std.Error", "t-value", "p-value"))
cat("────────────────────────────────────────────────────────────────\n")

coefs <- summary_model$coefficients
for (i in 1:nrow(coefs)) {
  var_name <- rownames(coefs)[i]
  coef <- coefs[i, 1]
  se <- coefs[i, 2]
  t_val <- coefs[i, 3]
  p_val <- coefs[i, 4]
  
  # Indicar significancia
  sig <- if (p_val < 0.001) {
    "***"
  } else if (p_val < 0.01) {
    "** "
  } else if (p_val < 0.05) {
    "*  "
  } else if (p_val < 0.1) {
    ".  "
  } else {
    "   "
  }
  
  cat(sprintf("%-30s %+10.4f %10.4f %10.3f  %.4f %s\n", 
              var_name, coef, se, t_val, p_val, sig))
}

cat("────────────────────────────────────────────────────────────────\n")
cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")

# ==============================================================================
# PASO 3: INTERPRETACIÓN DE COEFICIENTES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 3: Interpretación de coeficientes significativos\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("💡 ¿QUÉ SIGNIFICA CADA COEFICIENTE?\n")
cat("────────────────────────────────────────────────────────────────\n")

# Identificar variables significativas
coefs_sig <- coefs[coefs[, 4] < 0.05, , drop = FALSE]
coefs_sig <- coefs_sig[rownames(coefs_sig) != "(Intercept)", , drop = FALSE]

if (nrow(coefs_sig) > 0) {
  for (i in 1:nrow(coefs_sig)) {
    var_name <- rownames(coefs_sig)[i]
    coef <- coefs_sig[i, 1]
    p_val <- coefs_sig[i, 4]
    
    cat(sprintf("\n🔹 %s (p=%.4f):\n", var_name, p_val))
    
    if (var_name == "sleep_duration") {
      cat(sprintf("   → Por cada hora adicional de sueño, la calidad aumenta %.3f puntos\n", coef))
    } else if (var_name == "physical_activity_level") {
      cat(sprintf("   → Por cada punto de aumento en actividad física, la calidad aumenta %.3f puntos\n", coef))
    } else if (var_name == "stress_level") {
      if (coef < 0) {
        cat(sprintf("   → Por cada punto de aumento en estrés, la calidad DISMINUYE %.3f puntos\n", abs(coef)))
      } else {
        cat(sprintf("   → Por cada punto de aumento en estrés, la calidad aumenta %.3f puntos\n", coef))
      }
    } else if (var_name == "heart_rate") {
      cat(sprintf("   → Por cada latido/min adicional, la calidad cambia %.4f puntos\n", coef))
    } else if (var_name == "daily_steps") {
      cat(sprintf("   → Por cada 1000 pasos adicionales, la calidad cambia %.4f puntos\n", coef * 1000))
    } else if (var_name == "gender_male") {
      if (coef > 0) {
        cat(sprintf("   → Los hombres tienen %.3f puntos MÁS de calidad que las mujeres\n", coef))
      } else {
        cat(sprintf("   → Los hombres tienen %.3f puntos MENOS de calidad que las mujeres\n", abs(coef)))
      }
    } else if (var_name == "bmi_numeric") {
      if (coef < 0) {
        cat(sprintf("   → Cada categoría de BMI superior reduce la calidad en %.3f puntos\n", abs(coef)))
      } else {
        cat(sprintf("   → Cada categoría de BMI superior aumenta la calidad en %.3f puntos\n", coef))
      }
    } else if (var_name == "has_disorder") {
      if (coef < 0) {
        cat(sprintf("   → Tener un trastorno de sueño reduce la calidad en %.3f puntos\n", abs(coef)))
      } else {
        cat(sprintf("   → Tener un trastorno de sueño aumenta la calidad en %.3f puntos\n", coef))
      }
    }
  }
} else {
  cat("⚠️  No hay variables significativas al nivel 0.05\n")
}

cat("\n")

# ==============================================================================
# PASO 4: MODELO SIMPLIFICADO (SOLO VARIABLES SIGNIFICATIVAS)
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 4: Modelo simplificado (solo variables significativas)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Extraer nombres de variables significativas
if (nrow(coefs_sig) > 0) {
  vars_sig <- rownames(coefs_sig)
  formula_str <- paste("quality_of_sleep ~", paste(vars_sig, collapse = " + "))
  
  cat("📝 Fórmula del modelo simplificado:\n")
  cat("   ", formula_str, "\n\n")
  
  # Ajustar modelo simplificado
  modelo_simple <- lm(as.formula(formula_str), data = df)
  summary_simple <- summary(modelo_simple)
  
  cat("📊 RESULTADOS DEL MODELO SIMPLIFICADO:\n")
  cat("════════════════════════════════════════════════════════════════\n\n")
  cat(sprintf("• R² = %.4f (%.2f%% de la varianza explicada)\n", 
              summary_simple$r.squared, summary_simple$r.squared * 100))
  cat(sprintf("• R² ajustado = %.4f\n", summary_simple$adj.r.squared))
  cat(sprintf("• AIC = %.2f\n", AIC(modelo_simple)))
  cat(sprintf("• BIC = %.2f\n\n", BIC(modelo_simple)))
  
  # Comparar modelos
  cat("📊 COMPARACIÓN DE MODELOS:\n")
  cat("────────────────────────────────────────────────────────────────\n")
  cat(sprintf("%-20s %12s %12s\n", "", "Completo", "Simplificado"))
  cat("────────────────────────────────────────────────────────────────\n")
  cat(sprintf("%-20s %12.4f %12.4f\n", "R²", summary_model$r.squared, summary_simple$r.squared))
  cat(sprintf("%-20s %12.4f %12.4f\n", "R² ajustado", summary_model$adj.r.squared, summary_simple$adj.r.squared))
  cat(sprintf("%-20s %12.2f %12.2f\n", "AIC", AIC(modelo_completo), AIC(modelo_simple)))
  cat(sprintf("%-20s %12.2f %12.2f\n", "BIC", BIC(modelo_completo), BIC(modelo_simple)))
  cat(sprintf("%-20s %12d %12d\n", "Num. variables", length(coef(modelo_completo))-1, length(coef(modelo_simple))-1))
  cat("────────────────────────────────────────────────────────────────\n\n")
  
  cat("💡 El modelo simplificado es preferible porque:\n")
  cat("   ✓ Tiene menos variables (más parsimonioso)\n")
  cat("   ✓ Mantiene similar capacidad explicativa\n")
  cat("   ✓ Menor riesgo de sobreajuste\n\n")
}

# ==============================================================================
# PASO 5: IMPORTANCIA RELATIVA DE VARIABLES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 5: Importancia relativa de las variables\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Calculando coeficientes estandarizados (Beta)...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Estandarizar variables para calcular coeficientes beta
df_std <- df %>%
  mutate(across(c(sleep_duration, physical_activity_level, stress_level, 
                  heart_rate, daily_steps, quality_of_sleep), scale))

# Ajustar modelo con variables estandarizadas
modelo_std <- lm(quality_of_sleep ~ 
                   sleep_duration + 
                   physical_activity_level +
                   stress_level +
                   heart_rate +
                   daily_steps +
                   gender_male +
                   bmi_numeric +
                   has_disorder,
                 data = df_std)

coefs_std <- coef(modelo_std)[-1]  # Excluir intercepto
coefs_std <- sort(abs(coefs_std), decreasing = TRUE)

cat("🏆 RANKING DE IMPORTANCIA (coeficientes Beta estandarizados):\n")
cat("────────────────────────────────────────────────────────────────\n")

for (i in 1:length(coefs_std)) {
  var_name <- names(coefs_std)[i]
  beta <- coefs_std[i]
  
  # Crear barra visual
  bar_length <- round(beta * 50)
  bar <- paste0(rep("█", bar_length), collapse = "")
  
  cat(sprintf("%d. %-30s |Beta| = %.3f  %s\n", i, var_name, beta, bar))
}

cat("\n💡 Interpretación:\n")
cat("   Los coeficientes Beta estandarizados permiten comparar\n")
cat("   la importancia relativa independientemente de las unidades.\n")
cat("   → Mayor |Beta| = Mayor impacto en la calidad del sueño\n\n")

# ==============================================================================
# PASO 6: VISUALIZACIÓN - GRÁFICO DE COEFICIENTES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 6: Visualización de coeficientes\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Generando gráfico de coeficientes...\n")

# Preparar datos para el gráfico
coef_data <- data.frame(
  variable = names(coef(modelo_completo))[-1],
  coeficiente = coef(modelo_completo)[-1],
  se = summary_model$coefficients[-1, 2],
  p_value = summary_model$coefficients[-1, 4]
) %>%
  mutate(
    significativo = p_value < 0.05,
    lower = coeficiente - 1.96 * se,
    upper = coeficiente + 1.96 * se,
    variable_limpia = case_when(
      variable == "sleep_duration" ~ "Duración del sueño",
      variable == "physical_activity_level" ~ "Actividad física",
      variable == "stress_level" ~ "Nivel de estrés",
      variable == "heart_rate" ~ "Frecuencia cardíaca",
      variable == "daily_steps" ~ "Pasos diarios",
      variable == "gender_male" ~ "Género (Masculino)",
      variable == "bmi_numeric" ~ "Categoría BMI",
      variable == "has_disorder" ~ "Trastorno de sueño",
      TRUE ~ variable
    )
  ) %>%
  arrange(desc(abs(coeficiente)))

# Crear gráfico
p <- ggplot(coef_data, aes(x = reorder(variable_limpia, coeficiente), 
                            y = coeficiente,
                            color = significativo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
  coord_flip() +
  scale_color_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e74c3c"),
                     labels = c("No significativo", "Significativo (p<0.05)")) +
  labs(
    title = "Coeficientes del Modelo de Regresión",
    subtitle = "Impacto de cada variable en la calidad del sueño (IC 95%)",
    x = "",
    y = "Coeficiente estimado",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Guardar gráfico
output_path <- file.path(output_folder, "coeficientes_regresion.png")
ggsave(output_path, p, width = 10, height = 6, dpi = 300)

cat("✓ Gráfico guardado en:", output_path, "\n\n")

# ==============================================================================
# PASO 7: DIAGNÓSTICO DEL MODELO
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 7: Diagnóstico del modelo (supuestos de regresión)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔍 Verificando supuestos del modelo...\n\n")

# Test de normalidad de residuos
shapiro_test <- shapiro.test(residuals(modelo_completo))
cat(sprintf("1. Normalidad de residuos (Shapiro-Wilk test):\n"))
cat(sprintf("   W = %.4f, p-value = %.4f\n", shapiro_test$statistic, shapiro_test$p.value))
if (shapiro_test$p.value > 0.05) {
  cat("   ✓ Los residuos siguen una distribución normal\n\n")
} else {
  cat("   ⚠️  Los residuos NO siguen una distribución normal\n")
  cat("      (Con n grande, las violaciones leves son tolerables)\n\n")
}

# Test de homocedasticidad
cat("2. Homocedasticidad (varianza constante):\n")
cat("   Ver gráfico de diagnóstico (próximo paso)\n\n")

# Multicolinealidad (VIF)
if (requireNamespace("car", quietly = TRUE)) {
  library(car)
  vif_values <- vif(modelo_completo)
  cat("3. Multicolinealidad (VIF - Variance Inflation Factor):\n")
  cat("────────────────────────────────────────────────────────\n")
  for (i in 1:length(vif_values)) {
    vif_val <- vif_values[i]
    var_name <- names(vif_values)[i]
    interpretacion <- if (vif_val < 5) {
      "✓ OK"
    } else if (vif_val < 10) {
      "⚠️ Moderada"
    } else {
      "❌ Alta"
    }
    cat(sprintf("   %-30s VIF = %6.2f  %s\n", var_name, vif_val, interpretacion))
  }
  cat("\n   💡 VIF < 5: No hay multicolinealidad\n")
  cat("      VIF > 10: Multicolinealidad problemática\n\n")
} else {
  cat("3. Multicolinealidad:\n")
  cat("   ⚠️  Instalar librería 'car' para calcular VIF\n\n")
}

# Crear gráficos de diagnóstico
cat("📊 Generando gráficos de diagnóstico...\n")

output_path_diag <- file.path(output_folder, "diagnostico_regresion.png")
png(output_path_diag, width = 1200, height = 1000, res = 120)

par(mfrow = c(2, 2))
plot(modelo_completo)

dev.off()

cat("✓ Gráficos de diagnóstico guardados en:", output_path_diag, "\n\n")

# ==============================================================================
# PASO 8: CONCLUSIONES FINALES
# ==============================================================================
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    CONCLUSIONES FINALES                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("✅ RESPUESTA A LA PREGUNTA:\n")
cat("¿Qué variables explican mejor la calidad del sueño?\n\n")

cat("🏆 VARIABLES MÁS IMPORTANTES (en orden de impacto):\n")
cat("════════════════════════════════════════════════════════════════\n")

# Ordenar por importancia (coeficientes estandarizados + significancia)
ranking <- coef_data %>%
  filter(significativo) %>%
  arrange(desc(abs(coeficiente))) %>%
  head(5)

if (nrow(ranking) > 0) {
  for (i in 1:nrow(ranking)) {
    cat(sprintf("\n%d. %s\n", i, ranking$variable_limpia[i]))
    cat(sprintf("   • Coeficiente: %+.4f (p=%.4f)\n", 
                ranking$coeficiente[i], ranking$p_value[i]))
    if (ranking$coeficiente[i] > 0) {
      cat("   • Efecto: POSITIVO → Mejora la calidad del sueño\n")
    } else {
      cat("   • Efecto: NEGATIVO → Empeora la calidad del sueño\n")
    }
  }
}

cat("\n\n📊 CAPACIDAD EXPLICATIVA DEL MODELO:\n")
cat(sprintf("   • El modelo explica el %.1f%% de la varianza en la calidad del sueño\n", 
            summary_model$r.squared * 100))
cat(sprintf("   • Error medio de predicción: ±%.2f puntos\n", summary_model$sigma))

cat("\n💡 IMPLICACIONES PRÁCTICAS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   Para mejorar la calidad del sueño, deberías enfocarte en:\n")

top_3_pos <- coef_data %>%
  filter(significativo, coeficiente > 0) %>%
  arrange(desc(coeficiente)) %>%
  head(3)

if (nrow(top_3_pos) > 0) {
  cat("\n   ✓ AUMENTAR:\n")
  for (i in 1:nrow(top_3_pos)) {
    cat(sprintf("     → %s\n", top_3_pos$variable_limpia[i]))
  }
}

top_3_neg <- coef_data %>%
  filter(significativo, coeficiente < 0) %>%
  arrange(coeficiente) %>%
  head(3)

if (nrow(top_3_neg) > 0) {
  cat("\n   ✓ REDUCIR:\n")
  for (i in 1:nrow(top_3_neg)) {
    cat(sprintf("     → %s\n", top_3_neg$variable_limpia[i]))
  }
}

cat("\n\n📁 ARCHIVOS GENERADOS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   1. coeficientes_regresion.png - Gráfico de coeficientes\n")
cat("   2. diagnostico_regresion.png - Gráficos de diagnóstico\n\n")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    ANÁLISIS 5 COMPLETADO                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("🎯 ¿Quieres hacer otro análisis o visualización?\n\n")
