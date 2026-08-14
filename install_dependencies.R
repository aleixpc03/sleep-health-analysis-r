# Instala los paquetes necesarios para reproducir el análisis.
#
#   Rscript install_dependencies.R

paquetes <- c(
  "tidyverse",   # dplyr, ggplot2, tidyr, readr
  "janitor",     # limpieza de nombres de columna
  "skimr",       # resúmenes estructurados
  "psych",       # estadísticos descriptivos
  "naniar",      # análisis de valores ausentes
  "corrplot",    # visualización de correlaciones
  "GGally",      # matrices de gráficos
  "gridExtra",   # composición de gráficos
  "patchwork",   # composición de gráficos
  "scales",      # formateo de escalas
  "viridis",     # paletas
  "car",         # diagnóstico de regresión
  "FSA",         # pruebas no paramétricas post-hoc
  "glmnet",      # Lasso y Ridge
  "caret",       # partición y validación
  "pROC",        # curvas ROC
  "FactoMineR",  # PCA
  "factoextra",  # visualización de PCA y clustering
  "cluster",     # métricas de clustering
  "rmarkdown"    # renderizado del informe
)

faltan <- paquetes[!paquetes %in% rownames(installed.packages())]

if (length(faltan) == 0) {
  message("Todas las dependencias están instaladas.")
} else {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
}
