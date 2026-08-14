# Salud del sueño y estilo de vida: análisis estadístico en R

Estudio completo sobre los factores que determinan la calidad del sueño, combinando estadística inferencial clásica, regularización y aprendizaje no supervisado. Escrito íntegramente en R.

Trabajo de la asignatura de Data Science del Máster en Análisis de Datos en Ingeniería (Tecnun – Universidad de Navarra).

---

## Preguntas de investigación

El análisis se organiza alrededor de seis preguntas concretas, cada una con su método:

| # | Pregunta | Método |
|---|---|---|
| 1 | ¿Difiere la duración del sueño entre hombres y mujeres? | Test de normalidad → Wilcoxon |
| 2 | ¿La calidad del sueño depende de la categoría de IMC? | ANOVA / Kruskal-Wallis + post-hoc |
| 3 | ¿Qué variables explican mejor la calidad del sueño? | Regresión lineal múltiple + Lasso y Ridge |
| 4 | ¿Influyen los trastornos del sueño en su calidad? | Contraste de hipótesis |
| 5 | ¿Se puede predecir insomnio o apnea? | Regresión logística + curva ROC |
| 6 | ¿Existen perfiles latentes de individuos? | PCA + k-means + clustering jerárquico |

Plantear el trabajo como preguntas —y no como un recorrido por técnicas— obliga a justificar cada método por lo que se quiere averiguar.

## Datos

*Sleep Health and Lifestyle Dataset* (Kaggle): 400 registros, 13 variables sobre hábitos de sueño y estilo de vida —duración y calidad del sueño, nivel de estrés, actividad física, categoría de IMC, presión arterial, frecuencia cardiaca, pasos diarios y diagnóstico de trastorno del sueño.

El dataset se incluye en `data/`, con atribución a su autor original (Laksika Tharmalingam, Kaggle). No contiene datos personales identificables. El repositorio es ejecutable sin descargas previas.

## Metodología

```
Datos crudos
     │
     ├─► Limpieza y EDA ─────────► dataset limpio
     │   (nulos, tipos, outliers,
     │    descriptivos, correlaciones)
     │
     ├─► Contrastes de hipótesis ─► preguntas 1, 2, 4
     │   (normalidad → paramétrico
     │    o no paramétrico)
     │
     ├─► Modelos lineales ────────► pregunta 3
     │   (OLS + diagnóstico,
     │    Lasso, Ridge, CV)
     │
     ├─► Regresión logística ─────► pregunta 5
     │   (ROC, sensibilidad)
     │
     └─► PCA + clustering ────────► pregunta 6
         (k óptimo por silueta,
          k-means y jerárquico)
```

Detalles de método que sostienen las conclusiones:

- **Cada contraste va precedido de una comprobación de normalidad**, y la elección entre prueba paramétrica y no paramétrica se justifica a partir de ella en lugar de asumirse.
- **La regresión lineal incluye diagnóstico de residuos** (`diagnostico_regresion.png`), no solo el R².
- **Lasso y Ridge se ajustan con validación cruzada** para elegir lambda, y se comparan los caminos de coeficientes de ambos frente a OLS.
- **El número de clústeres se elige con el método del codo y el coeficiente de silueta**, no a ojo, y se contrasta k-means contra clustering jerárquico.

## Resultados

**Estrés y calidad del sueño están fuertemente asociados negativamente: correlación de −0.89.** Es la relación más marcada del conjunto de datos y ninguna otra se le acerca.

**El nivel de estrés es la variable más influyente** sobre la calidad del sueño en los modelos de regresión, por delante de la duración del sueño. Pasos diarios, frecuencia cardiaca y edad tienen un peso mucho menor. La lectura práctica es que dormir bien no depende solo de cuántas horas se duerme.

**El IMC importa, pero no linealmente.** La diferencia relevante aparece al pasar de peso normal a sobrepeso; entre sobrepeso y obesidad la calidad del sueño apenas cambia. El salto crítico es el primero.

**El modelo de detección de trastornos del sueño alcanza AUC = 0.88 con 93 % de sensibilidad.** En un problema de cribado, priorizar sensibilidad es la decisión correcta: importa más no dejar escapar casos que evitar algún falso positivo.

**El clustering identifica tres perfiles** bien diferenciados:

1. Alto estrés y mal sueño.
2. Buen sueño, actividad física regular, hábitos saludables.
3. Poca actividad física y estrés intermedio.

La variable que más separa los grupos es la calidad del sueño, por encima de la edad o el género.

Las figuras están en `figures/` y el informe completo —69 páginas con el código, los contrastes y su interpretación— en [`report/sleep_health_analysis.pdf`](report/sleep_health_analysis.pdf).

## Tecnologías

**R** · `tidyverse` · `glmnet` (Lasso/Ridge) · `caret` · `FactoMineR` + `factoextra` (PCA) · `cluster` · `pROC` · `car` · `FSA` · `corrplot` · `GGally` · `janitor` · `skimr` · `R Markdown`

## Instalación y ejecución

```bash
git clone https://github.com/<usuario>/sleep-health-analysis-r.git
cd sleep-health-analysis-r

# 1. Dependencias
Rscript install_dependencies.R

# 2. Informe completo
Rscript -e 'rmarkdown::render("analysis/sleep_health_analysis.Rmd", output_dir = "report")'
```

Los análisis también se pueden lanzar por separado desde la raíz del repositorio:

```bash
Rscript R/00_run_all.R              # limpieza y EDA
Rscript R/05_sleep_quality_models.R # modelos de calidad del sueño
Rscript R/06_lasso_ridge.R          # regularización
Rscript R/07_clustering.R           # PCA y clustering
```

## Estructura

```
├── analysis/
│   └── sleep_health_analysis.Rmd   # informe completo (2.200 líneas)
├── R/
│   ├── 00_run_all.R                # limpieza y EDA
│   ├── 05_sleep_quality_models.R
│   ├── 06_lasso_ridge.R
│   └── 07_clustering.R
├── figures/                        # 11 gráficos del análisis
├── report/
│   └── sleep_health_analysis.pdf   # informe renderizado (69 páginas)
├── data/                           # dataset de origen (ver data/README.md)
└── install_dependencies.R
```

## Limitaciones y mejoras posibles

- **Es un estudio observacional y transversal.** Todas las relaciones descritas son asociaciones, no causalidad: la correlación de −0.89 entre estrés y sueño es compatible con que el estrés estropee el sueño, con que dormir mal genere estrés, o con ambas cosas a la vez.
- **El dataset procede de Kaggle y es de origen no clínico**, con indicios de haber sido generado artificialmente. Las correlaciones son sospechosamente limpias para datos de salud reales, y eso conviene tenerlo presente antes de extrapolar nada.
- **400 registros** bastan para los contrastes planteados, pero no para modelos más complejos ni para segmentar por subgrupos.
- **La regresión logística se evalúa sin validación cruzada anidada**, así que el AUC de 0.88 es probablemente algo optimista.
- El clustering se ejecuta sobre variables estandarizadas pero **sin estudiar la estabilidad** de los grupos ante remuestreo.

## Autoría

Trabajo conjunto de **Santiago López** y **Aleix Pagès Coromina** para la asignatura de Data Science del Máster en Análisis de Datos en Ingeniería, Tecnun – Universidad de Navarra. Publicado con el consentimiento de ambos autores.

Parte del código del trabajo se desarrolló con apoyo de herramientas de inteligencia artificial, tal y como se declaró en la entrega original.

El dataset es obra de Laksika Tharmalingam y se distribuye desde Kaggle; se incluye aquí con atribución. Consulta las condiciones vigentes en su página antes de reutilizarlo.
