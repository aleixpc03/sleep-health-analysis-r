# Datos

## `Sleep_health_and_lifestyle_dataset.csv`

Incluido en el repositorio. Es el *Sleep Health and Lifestyle Dataset* publicado
en Kaggle por **Laksika Tharmalingam**:

https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset

- 400 filas, 13 columnas.
- Variables: identificador, género, edad, profesión, duración del sueño,
  calidad del sueño, nivel de actividad física, nivel de estrés, categoría de
  IMC, presión arterial, frecuencia cardiaca, pasos diarios y trastorno del
  sueño.
- No contiene datos personales identificables.

Si vas a reutilizarlo, consulta las condiciones vigentes en la página de Kaggle.

## Archivos derivados

`R/00_run_all.R` genera `Sleep_health_and_lifestyle_dataset_clean.csv` a partir
del anterior. No se versiona: se reconstruye ejecutando el script.
