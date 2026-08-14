################################################################################
#                  SCRIPT MASTER - CARREGA TOT EL TREBALL                      #
#                                                                               #
#  Aquest script carrega el dataset net i configura l'entorn per continuar     #
#  amb l'anàlisi. Executa aquest fitxer per recuperar tot el treball.         #
#                                                                               #
#  Autor: GitHub Copilot Assistant                                             #
#  Data: Octubre 2025                                                          #
################################################################################

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║          SCRIPT MASTER - Carregant treball anterior           ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CONFIGURACIÓ INICIAL
# ==============================================================================

# Definir carpeta de treball
carpeta_treball <- getwd()
# El proyecto usa rutas relativas: ejecutar desde la raiz del repositorio.
cat("📁 Carpeta de treball:", getwd(), "\n\n")

# ==============================================================================
# CARREGAR LLIBRERIES
# ==============================================================================

cat("📦 Carregant llibreries necessàries...\n")

suppressPackageStartupMessages({
  library(tidyverse)   # Manipulació de dades
  library(janitor)     # Neteja de noms
  library(glmnet)      # Lasso/Ridge
  library(pROC)        # Corbes ROC
  library(cluster)     # Clustering
  library(factoextra)  # Visualització
  library(car)         # VIF
})

cat("✓ Llibreries carregades correctament\n\n")

# ==============================================================================
# CARREGAR DADES
# ==============================================================================

cat("📊 Carregant dataset...\n")

# Dataset original
df_original <- read_csv("Sleep_health_and_lifestyle_dataset.csv", 
                        show_col_types = FALSE) %>% 
  clean_names()

# Dataset net (si existeix)
if (file.exists("Sleep_health_and_lifestyle_dataset_clean.csv")) {
  df <- read_csv("Sleep_health_and_lifestyle_dataset_clean.csv", 
                 show_col_types = FALSE)
  cat("✓ Dataset net carregat\n")
} else {
  # Si no existeix, crear-lo
  df <- df_original %>%
    mutate(
      bmi_category = case_when(
        bmi_category == "Normal Weight" ~ "Normal",
        bmi_category == "Normal" ~ "Normal",
        bmi_category == "Overweight" ~ "Overweight",
        bmi_category == "Obese" ~ "Obese",
        TRUE ~ bmi_category
      ),
      bmi_numeric = case_when(
        bmi_category == "Normal" ~ 0,
        bmi_category == "Overweight" ~ 1,
        bmi_category == "Obese" ~ 2
      ),
      gender_numeric = ifelse(gender == "Male", 1, 0),
      has_disorder = ifelse(sleep_disorder == "None", 0, 1),
      has_insomnia = ifelse(sleep_disorder == "Insomnia", 1, 0),
      has_apnea = ifelse(sleep_disorder == "Sleep Apnea", 1, 0)
    )
  
  write_csv(df, "Sleep_health_and_lifestyle_dataset_clean.csv")
  cat("✓ Dataset net creat i guardat\n")
}

cat(sprintf("   • Observacions: %d\n", nrow(df)))
cat(sprintf("   • Variables: %d\n\n", ncol(df)))

# ==============================================================================
# ESTADÍSTIQUES DESCRIPTIVES RÀPIDES
# ==============================================================================

cat("📈 RESUM ESTADÍSTIC:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("🔹 Qualitat del son:\n")
cat(sprintf("   Mitjana: %.2f (±%.2f)\n", 
            mean(df$quality_of_sleep), sd(df$quality_of_sleep)))

cat("\n🔹 Duració del son:\n")
cat(sprintf("   Mitjana: %.2f hores (±%.2f)\n", 
            mean(df$sleep_duration), sd(df$sleep_duration)))

cat("\n🔹 Nivell d'estrès:\n")
cat(sprintf("   Mitjana: %.2f (±%.2f)\n", 
            mean(df$stress_level), sd(df$stress_level)))

cat("\n🔹 Distribució de trastorns:\n")
print(table(df$sleep_disorder))

cat("\n🔹 Distribució per gènere:\n")
print(table(df$gender))

cat("\n🔹 Distribució per BMI:\n")
print(table(df$bmi_category))

cat("\n")

# ==============================================================================
# MODELS GUARDATS (per si vols recarregar-los)
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("  MODELS DISPONIBLES PER EXECUTAR\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📝 Scripts disponibles:\n")
scripts <- list.files(pattern = "^analisis_.*\\.R$")
if (length(scripts) > 0) {
  for (i in seq_along(scripts)) {
    cat(sprintf("   %d. %s\n", i, scripts[i]))
  }
} else {
  cat("   (No hi ha scripts d'anàlisi en aquesta carpeta)\n")
}

cat("\n📊 Outputs generats:\n")
outputs <- list.files(pattern = "\\.(png|csv)$")
if (length(outputs) > 0) {
  outputs_sorted <- outputs[order(outputs)]
  for (output in outputs_sorted) {
    size <- file.size(output) / 1024
    cat(sprintf("   • %s (%.1f KB)\n", output, size))
  }
} else {
  cat("   (No hi ha outputs generats)\n")
}

cat("\n")

# ==============================================================================
# FUNCIONS ÚTILS PER CONTINUAR
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("  FUNCIONS ÚTILS CARREGADES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Funció per veure estructura
ver_estructura <- function() {
  cat("ESTRUCTURA DEL DATASET:\n")
  cat("════════════════════════════════════════════════════════════════\n")
  str(df)
}

# Funció per estadístiques ràpides d'una variable
estadistiques <- function(variable) {
  cat(sprintf("\nESTADÍSTIQUES DE %s:\n", deparse(substitute(variable))))
  cat("────────────────────────────────────────────────────────────────\n")
  if (is.numeric(variable)) {
    cat(sprintf("   Mitjana:    %.3f\n", mean(variable, na.rm = TRUE)))
    cat(sprintf("   Mediana:    %.3f\n", median(variable, na.rm = TRUE)))
    cat(sprintf("   Desv. Est.: %.3f\n", sd(variable, na.rm = TRUE)))
    cat(sprintf("   Mínim:      %.3f\n", min(variable, na.rm = TRUE)))
    cat(sprintf("   Màxim:      %.3f\n", max(variable, na.rm = TRUE)))
    cat(sprintf("   NA's:       %d\n", sum(is.na(variable))))
  } else {
    print(table(variable, useNA = "ifany"))
  }
}

# Funció per correlacions ràpides
correlacions <- function() {
  vars_num <- df %>% 
    select(where(is.numeric)) %>%
    select(-person_id, -age, -bmi_numeric, -gender_numeric)
  
  cat("\nMATRIU DE CORRELACIONS:\n")
  cat("════════════════════════════════════════════════════════════════\n")
  cor_matrix <- cor(vars_num, use = "complete.obs")
  print(round(cor_matrix, 3))
}

# Funció per executar un script d'anàlisi
executar_analisi <- function(numero) {
  scripts <- list.files(pattern = "^analisis_.*\\.R$")
  if (numero > 0 && numero <= length(scripts)) {
    script <- scripts[numero]
    cat(sprintf("\n🚀 Executant %s...\n\n", script))
    source(script)
  } else {
    cat("❌ Número d'anàlisi invàlid\n")
  }
}

cat("Funcions disponibles:\n")
cat("   • ver_estructura()        - Veure estructura del dataset\n")
cat("   • estadistiques(variable) - Estadístiques d'una variable\n")
cat("   • correlacions()          - Matriu de correlacions\n")
cat("   • executar_analisi(n)     - Executar script número n\n")

cat("\n")

# ==============================================================================
# EXEMPLES D'ÚS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("  EXEMPLES D'ÚS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("# Veure estructura de les dades:\n")
cat("ver_estructura()\n\n")

cat("# Estadístiques d'una variable:\n")
cat("estadistiques(df$quality_of_sleep)\n\n")

cat("# Matriu de correlacions:\n")
cat("correlacions()\n\n")

cat("# Executar un anàlisi específic:\n")
cat("executar_analisi(5)  # Executa analisis_5_calidad_sueno.R\n\n")

cat("# Crear un nou gràfic:\n")
cat("ggplot(df, aes(x = stress_level, y = quality_of_sleep)) +\n")
cat("  geom_point() +\n")
cat("  geom_smooth(method = 'lm') +\n")
cat("  theme_minimal()\n\n")

cat("# Filtrar dades:\n")
cat("df_homes <- df %>% filter(gender == 'Male')\n")
cat("df_trastorns <- df %>% filter(sleep_disorder != 'None')\n\n")

# ==============================================================================
# READY!
# ==============================================================================

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                   TOT LLEST PER CONTINUAR!                    ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("✅ Dataset 'df' carregat i llest per usar\n")
cat("✅ Totes les llibreries carregades\n")
cat("✅ Funcions útils disponibles\n\n")

cat("💡 Per començar, prova:\n")
cat("   glimpse(df)\n")
cat("   head(df)\n")
cat("   correlacions()\n\n")

cat("📖 Per més informació, consulta: RESUM_ANALISIS_COMPLETO.md\n\n")
