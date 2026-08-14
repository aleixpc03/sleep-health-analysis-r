################################################################################
#  ANÁLISIS 7: Patrones latentes - Clustering de perfiles de sueño            #
################################################################################

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(cluster)      # Para clustering
  library(factoextra)   # Para visualización de clusters
  library(gridExtra)    # Para múltiples gráficos
})

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ANÁLISIS 7: Clustering - Perfiles de sueño y estilo de vida ║\n")
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

# Crear variables numéricas
df <- df %>%
  mutate(
    bmi_numeric = case_when(
      bmi_category == "Normal" ~ 0,
      bmi_category == "Overweight" ~ 1,
      bmi_category == "Obese" ~ 2
    ),
    gender_numeric = ifelse(gender == "Male", 1, 0),
    has_disorder = ifelse(sleep_disorder == "None", 0, 1)
  )

cat("✓ Datos cargados:", nrow(df), "observaciones\n\n")

# ==============================================================================
# PASO 1: SELECCIÓN Y PREPARACIÓN DE VARIABLES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 1: Selección de variables para clustering\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔍 Variables seleccionadas para identificar perfiles:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   1. quality_of_sleep      - Calidad del sueño (1-10)\n")
cat("   2. sleep_duration         - Duración del sueño (horas)\n")
cat("   3. physical_activity_level - Actividad física (0-100)\n")
cat("   4. stress_level           - Nivel de estrés (1-10)\n")
cat("   5. heart_rate             - Frecuencia cardíaca (bpm)\n")
cat("   6. daily_steps            - Pasos diarios\n")
cat("   7. bmi_numeric            - Categoría de IMC (0-2)\n\n")

# Seleccionar variables para clustering
vars_cluster <- c("quality_of_sleep", "sleep_duration", "physical_activity_level",
                  "stress_level", "heart_rate", "daily_steps", "bmi_numeric")

df_cluster <- df %>%
  select(all_of(vars_cluster)) %>%
  na.omit()

cat(sprintf("📊 Datos para clustering: %d observaciones\n", nrow(df_cluster)))
cat(sprintf("   • Variables utilizadas: %d\n\n", ncol(df_cluster)))

# Estandarizar variables (importante para clustering)
df_scaled <- scale(df_cluster)

cat("✓ Variables estandarizadas (media=0, sd=1)\n")
cat("   Esto es crucial para que todas las variables tengan\n")
cat("   el mismo peso en la distancia euclidiana.\n\n")

# ==============================================================================
# PASO 2: DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 2: Determinando número óptimo de clusters\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📊 Calculando métricas para k = 2 a 10 clusters...\n\n")

# Método del codo (Within Sum of Squares)
set.seed(42)
wss <- sapply(2:10, function(k) {
  kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss
})

cat("🔹 Método del Codo (Within-cluster Sum of Squares):\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   k    WSS\n")
cat("────────────────────────────────────────────────────────────────\n")
for (i in 1:length(wss)) {
  cat(sprintf("   %d    %.2f\n", i+1, wss[i]))
}
cat("\n")

# Método de la silueta
set.seed(42)
silhouette_scores <- sapply(2:10, function(k) {
  km <- kmeans(df_scaled, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(df_scaled))
  mean(sil[, 3])
})

cat("🔹 Método de la Silueta (Silhouette Score):\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   k    Silueta   Interpretación\n")
cat("────────────────────────────────────────────────────────────────\n")
for (i in 1:length(silhouette_scores)) {
  score <- silhouette_scores[i]
  interpretacion <- if (score > 0.7) {
    "EXCELENTE"
  } else if (score > 0.5) {
    "BUENA"
  } else if (score > 0.25) {
    "DÉBIL"
  } else {
    "POBRE"
  }
  cat(sprintf("   %d    %.4f    %s\n", i+1, score, interpretacion))
}
cat("\n")

# Determinar k óptimo
k_optimo_sil <- which.max(silhouette_scores) + 1
cat(sprintf("🎯 NÚMERO ÓPTIMO DE CLUSTERS: k = %d\n", k_optimo_sil))
cat(sprintf("   • Basado en máximo Silhouette Score (%.4f)\n", max(silhouette_scores)))
cat(sprintf("   • Interpretación: Estructura de clusters %s\n\n", 
            ifelse(max(silhouette_scores) > 0.5, "BUENA", "DÉBIL")))

# ==============================================================================
# PASO 3: K-MEANS CLUSTERING
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 3: K-Means Clustering\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat(sprintf("🔧 Aplicando K-Means con k = %d...\n\n", k_optimo_sil))

# Aplicar K-means con k óptimo
set.seed(42)
kmeans_result <- kmeans(df_scaled, centers = k_optimo_sil, nstart = 25, iter.max = 100)

# Agregar clusters al dataframe original
df$cluster <- as.factor(kmeans_result$cluster)

cat("✓ Clustering completado\n\n")

cat("📊 DISTRIBUCIÓN DE CLUSTERS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cluster_counts <- table(df$cluster)
for (i in 1:k_optimo_sil) {
  pct <- 100 * cluster_counts[i] / sum(cluster_counts)
  cat(sprintf("   Cluster %d: %3d personas (%.1f%%)\n", i, cluster_counts[i], pct))
}
cat("\n")

# Calcular silueta final
sil_final <- silhouette(kmeans_result$cluster, dist(df_scaled))
cat(sprintf("📈 Calidad del clustering (Silueta media): %.4f\n\n", mean(sil_final[, 3])))

# ==============================================================================
# PASO 4: CARACTERIZACIÓN DE CLUSTERS
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 4: Caracterización de los perfiles identificados\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Calcular medias por cluster
cluster_profiles <- df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    quality_of_sleep = mean(quality_of_sleep, na.rm = TRUE),
    sleep_duration = mean(sleep_duration, na.rm = TRUE),
    physical_activity = mean(physical_activity_level, na.rm = TRUE),
    stress_level = mean(stress_level, na.rm = TRUE),
    heart_rate = mean(heart_rate, na.rm = TRUE),
    daily_steps = mean(daily_steps, na.rm = TRUE),
    bmi_avg = mean(bmi_numeric, na.rm = TRUE),
    pct_disorder = 100 * mean(has_disorder, na.rm = TRUE),
    pct_male = 100 * mean(gender_numeric, na.rm = TRUE)
  )

cat("📋 PERFILES DE LOS CLUSTERS:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

for (i in 1:k_optimo_sil) {
  perfil <- cluster_profiles[i, ]
  
  cat(sprintf("🔷 CLUSTER %d (%d personas, %.1f%%):\n", 
              i, perfil$n, 100 * perfil$n / nrow(df)))
  cat("────────────────────────────────────────────────────────────────\n")
  cat(sprintf("   • Calidad del sueño:    %.2f / 10\n", perfil$quality_of_sleep))
  cat(sprintf("   • Duración del sueño:   %.2f horas\n", perfil$sleep_duration))
  cat(sprintf("   • Actividad física:     %.1f / 100\n", perfil$physical_activity))
  cat(sprintf("   • Nivel de estrés:      %.2f / 10\n", perfil$stress_level))
  cat(sprintf("   • Frecuencia cardíaca:  %.1f bpm\n", perfil$heart_rate))
  cat(sprintf("   • Pasos diarios:        %.0f pasos\n", perfil$daily_steps))
  cat(sprintf("   • Con trastorno:        %.1f%%\n", perfil$pct_disorder))
  cat(sprintf("   • Porcentaje hombres:   %.1f%%\n\n", perfil$pct_male))
}

# Identificar características distintivas de cada cluster
cat("🏷️  ETIQUETAS DESCRIPTIVAS DE LOS PERFILES:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

# Calcular valores medios globales para comparar
medias_globales <- df %>%
  summarise(
    quality = mean(quality_of_sleep, na.rm = TRUE),
    duration = mean(sleep_duration, na.rm = TRUE),
    stress = mean(stress_level, na.rm = TRUE),
    activity = mean(physical_activity_level, na.rm = TRUE),
    disorder = mean(has_disorder, na.rm = TRUE)
  )

for (i in 1:k_optimo_sil) {
  perfil <- cluster_profiles[i, ]
  
  cat(sprintf("🔷 CLUSTER %d - ", i))
  
  # Crear etiqueta descriptiva basada en características
  etiqueta <- character()
  
  if (perfil$quality_of_sleep > medias_globales$quality + 0.5) {
    etiqueta <- c(etiqueta, "Sueño Óptimo")
  } else if (perfil$quality_of_sleep < medias_globales$quality - 0.5) {
    etiqueta <- c(etiqueta, "Sueño Deficiente")
  }
  
  if (perfil$stress_level > medias_globales$stress + 1) {
    etiqueta <- c(etiqueta, "Alto Estrés")
  } else if (perfil$stress_level < medias_globales$stress - 1) {
    etiqueta <- c(etiqueta, "Bajo Estrés")
  }
  
  if (perfil$physical_activity > medias_globales$activity + 10) {
    etiqueta <- c(etiqueta, "Muy Activo")
  } else if (perfil$physical_activity < medias_globales$activity - 10) {
    etiqueta <- c(etiqueta, "Sedentario")
  }
  
  if (perfil$pct_disorder > 50) {
    etiqueta <- c(etiqueta, "Con Trastornos")
  }
  
  if (length(etiqueta) > 0) {
    cat(paste(etiqueta, collapse = " + "))
  } else {
    cat("Perfil Promedio")
  }
  cat("\n")
  
  # Características clave
  cat("   Características clave:\n")
  
  if (perfil$quality_of_sleep > medias_globales$quality + 0.5) {
    cat("   ✓ Calidad de sueño superior al promedio\n")
  } else if (perfil$quality_of_sleep < medias_globales$quality - 0.5) {
    cat("   ⚠️  Calidad de sueño inferior al promedio\n")
  }
  
  if (perfil$stress_level > medias_globales$stress + 1) {
    cat("   ⚠️  Nivel de estrés significativamente elevado\n")
  } else if (perfil$stress_level < medias_globales$stress - 1) {
    cat("   ✓ Nivel de estrés reducido\n")
  }
  
  if (perfil$physical_activity > medias_globales$activity + 10) {
    cat("   ✓ Nivel de actividad física alto\n")
  } else if (perfil$physical_activity < medias_globales$activity - 10) {
    cat("   ⚠️  Nivel de actividad física bajo\n")
  }
  
  if (perfil$pct_disorder > 50) {
    cat(sprintf("   ⚠️  Alta prevalencia de trastornos (%.0f%%)\n", perfil$pct_disorder))
  }
  
  cat("\n")
}

# ==============================================================================
# PASO 5: CLUSTERING JERÁRQUICO (VALIDACIÓN)
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 5: Clustering Jerárquico (validación alternativa)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("🔧 Calculando clustering jerárquico (método Ward)...\n")

# Para eficiencia, usar muestra si dataset es grande
if (nrow(df_scaled) > 500) {
  set.seed(42)
  sample_idx <- sample(1:nrow(df_scaled), 500)
  df_scaled_sample <- df_scaled[sample_idx, ]
  cat("   (usando muestra de 500 observaciones para eficiencia)\n\n")
} else {
  df_scaled_sample <- df_scaled
  cat("\n")
}

# Calcular distancias
dist_matrix <- dist(df_scaled_sample, method = "euclidean")

# Clustering jerárquico
hc <- hclust(dist_matrix, method = "ward.D2")

cat("✓ Clustering jerárquico completado\n\n")

# Cortar dendrograma en k_optimo clusters
hc_clusters <- cutree(hc, k = k_optimo_sil)

cat(sprintf("📊 Distribución de clusters (jerárquico, k=%d):\n", k_optimo_sil))
cat("────────────────────────────────────────────────────────────────\n")
hc_counts <- table(hc_clusters)
for (i in 1:k_optimo_sil) {
  pct <- 100 * hc_counts[i] / sum(hc_counts)
  cat(sprintf("   Cluster %d: %3d personas (%.1f%%)\n", i, hc_counts[i], pct))
}
cat("\n")

# ==============================================================================
# PASO 6: VISUALIZACIONES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 6: Generando visualizaciones\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# 6.1 - Método del codo y silueta
cat("📊 1. Gráfico de métodos de selección de k...\n")

output_path_elbow <- file.path(output_folder, "cluster_seleccion_k.png")
png(output_path_elbow, width = 1400, height = 600, res = 120)

par(mfrow = c(1, 2))

# Método del codo
plot(2:10, wss, type = "b", pch = 19, col = "#3498db", lwd = 2,
     xlab = "Número de clusters (k)", ylab = "WSS (Within-cluster Sum of Squares)",
     main = "Método del Codo", cex.main = 1.3, cex.lab = 1.1)
grid()

# Método de la silueta
plot(2:10, silhouette_scores, type = "b", pch = 19, col = "#e74c3c", lwd = 2,
     xlab = "Número de clusters (k)", ylab = "Silhouette Score",
     main = "Método de la Silueta", cex.main = 1.3, cex.lab = 1.1)
abline(h = max(silhouette_scores), lty = 2, col = "gray50")
points(k_optimo_sil, max(silhouette_scores), pch = 19, col = "#2ecc71", cex = 2)
text(k_optimo_sil, max(silhouette_scores), 
     sprintf("  k=%d (óptimo)", k_optimo_sil), pos = 4, col = "#2ecc71", cex = 1.2)
grid()

dev.off()

cat("   ✓ Guardado:", basename(output_path_elbow), "\n")

# 6.2 - Gráfico de silueta detallado
cat("📊 2. Gráfico de silueta detallado...\n")

output_path_sil <- file.path(output_folder, "cluster_silhouette.png")
png(output_path_sil, width = 1000, height = 800, res = 120)

plot(sil_final, col = 2:(k_optimo_sil+1), border = NA, 
     main = sprintf("Análisis de Silueta - K-Means (k=%d)", k_optimo_sil),
     cex.main = 1.4)
abline(v = mean(sil_final[, 3]), lty = 2, lwd = 2, col = "red")
text(mean(sil_final[, 3]), 0, 
     sprintf("  Media = %.3f", mean(sil_final[, 3])), 
     pos = 4, col = "red", cex = 1.2)

dev.off()

cat("   ✓ Guardado:", basename(output_path_sil), "\n")

# 6.3 - PCA para visualización en 2D
cat("📊 3. Proyección PCA de clusters...\n")

# Realizar PCA
pca_result <- prcomp(df_scaled, center = FALSE, scale. = FALSE)
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Cluster = df$cluster
)

# Calcular varianza explicada
var_explained <- summary(pca_result)$importance[2, 1:2] * 100

p_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.15, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Visualización de Clusters mediante PCA",
    subtitle = sprintf("PC1 explica %.1f%% | PC2 explica %.1f%% de la varianza", 
                       var_explained[1], var_explained[2]),
    x = sprintf("PC1 (%.1f%% varianza)", var_explained[1]),
    y = sprintf("PC2 (%.1f%% varianza)", var_explained[2])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "right"
  )

output_path_pca <- file.path(output_folder, "cluster_pca.png")
ggsave(output_path_pca, p_pca, width = 10, height = 6, dpi = 300)

cat("   ✓ Guardado:", basename(output_path_pca), "\n")

# 6.4 - Dendrograma
cat("📊 4. Dendrograma del clustering jerárquico...\n")

output_path_dend <- file.path(output_folder, "cluster_dendrograma.png")
png(output_path_dend, width = 1200, height = 800, res = 120)

plot(hc, hang = -1, cex = 0.6, 
     main = sprintf("Dendrograma - Clustering Jerárquico (método Ward)\nCortado en k=%d clusters", 
                    k_optimo_sil),
     xlab = "Observaciones", ylab = "Altura", sub = "")
rect.hclust(hc, k = k_optimo_sil, border = 2:(k_optimo_sil+1), lwd = 2)

dev.off()

cat("   ✓ Guardado:", basename(output_path_dend), "\n")

# 6.5 - Perfiles de clusters (heatmap)
cat("📊 5. Heatmap de perfiles de clusters...\n")

# Preparar datos para heatmap
cluster_profiles_scaled <- cluster_profiles %>%
  select(-n, -pct_disorder, -pct_male, -bmi_avg) %>%
  column_to_rownames("cluster") %>%
  as.matrix()

# Normalizar cada variable entre 0 y 1 para el heatmap
cluster_profiles_norm <- apply(cluster_profiles_scaled, 2, function(x) {
  (x - min(x)) / (max(x) - min(x))
})

# Convertir a formato largo para ggplot
heatmap_data <- as.data.frame(cluster_profiles_norm) %>%
  rownames_to_column("Cluster") %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Valor") %>%
  mutate(
    Variable = case_when(
      Variable == "quality_of_sleep" ~ "Calidad sueño",
      Variable == "sleep_duration" ~ "Duración sueño",
      Variable == "physical_activity" ~ "Actividad física",
      Variable == "stress_level" ~ "Nivel estrés",
      Variable == "heart_rate" ~ "Frec. cardíaca",
      Variable == "daily_steps" ~ "Pasos diarios",
      TRUE ~ Variable
    )
  )

p_heatmap <- ggplot(heatmap_data, aes(x = Variable, y = Cluster, fill = Valor)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Valor)), color = "black", size = 4) +
  scale_fill_gradient2(low = "#3498db", mid = "#f39c12", high = "#e74c3c", 
                       midpoint = 0.5, name = "Valor\nnormalizado") +
  labs(
    title = "Perfiles de Clusters - Heatmap",
    subtitle = "Valores normalizados entre 0 (mínimo) y 1 (máximo)",
    x = "",
    y = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

output_path_heat <- file.path(output_folder, "cluster_heatmap.png")
ggsave(output_path_heat, p_heatmap, width = 10, height = 6, dpi = 300)

cat("   ✓ Guardado:", basename(output_path_heat), "\n\n")

# 6.6 - Comparación de variables por cluster (boxplots)
cat("📊 6. Boxplots comparativos por cluster...\n")

# Preparar datos largos
df_long <- df %>%
  select(cluster, quality_of_sleep, sleep_duration, physical_activity_level, 
         stress_level, heart_rate, daily_steps) %>%
  pivot_longer(cols = -cluster, names_to = "Variable", values_to = "Valor") %>%
  mutate(
    Variable = case_when(
      Variable == "quality_of_sleep" ~ "Calidad\nsueño",
      Variable == "sleep_duration" ~ "Duración\nsueño (h)",
      Variable == "physical_activity_level" ~ "Actividad\nfísica",
      Variable == "stress_level" ~ "Nivel\nestrés",
      Variable == "heart_rate" ~ "Frec.\ncardíaca",
      Variable == "daily_steps" ~ "Pasos\ndiarios",
      TRUE ~ Variable
    )
  )

p_boxplot <- ggplot(df_long, aes(x = cluster, y = Valor, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  labs(
    title = "Comparación de Variables por Cluster",
    subtitle = "Distribución de características en cada perfil identificado",
    x = "Cluster",
    y = "Valor"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

output_path_box <- file.path(output_folder, "cluster_boxplots.png")
ggsave(output_path_box, p_boxplot, width = 12, height = 8, dpi = 300)

cat("   ✓ Guardado:", basename(output_path_box), "\n\n")

# ==============================================================================
# PASO 7: ANÁLISIS DE ASOCIACIONES
# ==============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PASO 7: Asociaciones con variables categóricas\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Género por cluster
cat("🔹 DISTRIBUCIÓN DE GÉNERO POR CLUSTER:\n")
cat("────────────────────────────────────────────────────────────────\n")
gender_cluster <- table(df$cluster, df$gender)
print(addmargins(gender_cluster))
cat("\n")

# Test chi-cuadrado
chi_gender <- chisq.test(gender_cluster)
cat(sprintf("Chi-cuadrado test: X² = %.2f, p-value = %.4f\n", 
            chi_gender$statistic, chi_gender$p.value))
if (chi_gender$p.value < 0.05) {
  cat("→ HAY asociación significativa entre cluster y género ✓\n\n")
} else {
  cat("→ NO hay asociación significativa entre cluster y género\n\n")
}

# Trastorno de sueño por cluster
cat("🔹 DISTRIBUCIÓN DE TRASTORNOS POR CLUSTER:\n")
cat("────────────────────────────────────────────────────────────────\n")
disorder_cluster <- table(df$cluster, df$sleep_disorder)
print(addmargins(disorder_cluster))
cat("\n")

# Test chi-cuadrado
chi_disorder <- chisq.test(disorder_cluster)
cat(sprintf("Chi-cuadrado test: X² = %.2f, p-value = %.4f\n", 
            chi_disorder$statistic, chi_disorder$p.value))
if (chi_disorder$p.value < 0.05) {
  cat("→ HAY asociación significativa entre cluster y trastornos ✓\n\n")
} else {
  cat("→ NO hay asociación significativa entre cluster y trastornos\n\n")
}

# BMI por cluster
cat("🔹 DISTRIBUCIÓN DE CATEGORÍA BMI POR CLUSTER:\n")
cat("────────────────────────────────────────────────────────────────\n")
bmi_cluster <- table(df$cluster, df$bmi_category)
print(addmargins(bmi_cluster))
cat("\n")

# Test chi-cuadrado
chi_bmi <- chisq.test(bmi_cluster)
cat(sprintf("Chi-cuadrado test: X² = %.2f, p-value = %.4f\n", 
            chi_bmi$statistic, chi_bmi$p.value))
if (chi_bmi$p.value < 0.05) {
  cat("→ HAY asociación significativa entre cluster y BMI ✓\n\n")
} else {
  cat("→ NO hay asociación significativa entre cluster y BMI\n\n")
}

# ==============================================================================
# PASO 8: CONCLUSIONES FINALES
# ==============================================================================
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    CONCLUSIONES FINALES                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("✅ RESPUESTA A LA PREGUNTA:\n")
cat("¿Existen patrones latentes que agrupan a los individuos en perfiles?\n\n")

cat(sprintf("🎯 SÍ, se identificaron %d PERFILES DISTINTOS de sueño y estilo de vida\n\n", 
            k_optimo_sil))

cat("📊 RESUMEN DE PERFILES ENCONTRADOS:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

for (i in 1:k_optimo_sil) {
  perfil <- cluster_profiles[i, ]
  cat(sprintf("🔷 PERFIL %d (%d personas):\n", i, perfil$n))
  
  # Características destacadas
  if (perfil$quality_of_sleep > medias_globales$quality + 0.5) {
    cat("   ✓ BUENA calidad de sueño\n")
  } else if (perfil$quality_of_sleep < medias_globales$quality - 0.5) {
    cat("   ⚠️  MALA calidad de sueño\n")
  }
  
  if (perfil$stress_level > medias_globales$stress + 1) {
    cat("   ⚠️  ALTO nivel de estrés\n")
  } else if (perfil$stress_level < medias_globales$stress - 1) {
    cat("   ✓ BAJO nivel de estrés\n")
  }
  
  if (perfil$physical_activity > medias_globales$activity + 10) {
    cat("   ✓ MUY activo físicamente\n")
  } else if (perfil$physical_activity < medias_globales$activity - 10) {
    cat("   ⚠️  POCO activo físicamente\n")
  }
  
  if (perfil$pct_disorder > 50) {
    cat(sprintf("   ⚠️  ALTA prevalencia de trastornos (%.0f%%)\n", perfil$pct_disorder))
  } else if (perfil$pct_disorder < 20) {
    cat(sprintf("   ✓ BAJA prevalencia de trastornos (%.0f%%)\n", perfil$pct_disorder))
  }
  
  cat("\n")
}

cat("💡 CALIDAD DEL CLUSTERING:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat(sprintf("   • Silhouette Score: %.4f ", mean(sil_final[, 3])))
if (mean(sil_final[, 3]) > 0.5) {
  cat("(BUENA estructura de clusters) ✓\n")
} else if (mean(sil_final[, 3]) > 0.25) {
  cat("(Estructura DÉBIL pero presente)\n")
} else {
  cat("(Estructura POBRE)\n")
}
cat(sprintf("   • Método: K-Means con k=%d clusters\n", k_optimo_sil))
cat("   • Variables: Calidad sueño, duración, actividad, estrés, etc.\n\n")

cat("🔍 ASOCIACIONES ENCONTRADAS:\n")
cat("────────────────────────────────────────────────────────────────\n")
if (chi_gender$p.value < 0.05) {
  cat("   ✓ Los clusters se asocian con GÉNERO\n")
}
if (chi_disorder$p.value < 0.05) {
  cat("   ✓ Los clusters se asocian con TRASTORNOS DE SUEÑO\n")
}
if (chi_bmi$p.value < 0.05) {
  cat("   ✓ Los clusters se asocian con CATEGORÍA DE BMI\n")
}
cat("\n")

cat("📁 ARCHIVOS GENERADOS:\n")
cat("────────────────────────────────────────────────────────────────\n")
cat("   1. cluster_seleccion_k.png       - Métodos para seleccionar k\n")
cat("   2. cluster_silhouette.png        - Análisis de silueta\n")
cat("   3. cluster_pca.png               - Visualización 2D (PCA)\n")
cat("   4. cluster_dendrograma.png       - Dendrograma jerárquico\n")
cat("   5. cluster_heatmap.png           - Perfiles en heatmap\n")
cat("   6. cluster_boxplots.png          - Comparación de variables\n\n")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    ANÁLISIS 7 COMPLETADO                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("🎯 Clustering completado. ¿Otro análisis?\n\n")
