# Informe renderizado

El PDF del informe no se incluye todavía: la versión existente se generó antes de
limpiar las rutas locales y contenía una ruta del equipo de origen en una celda
de código.

El `.Rmd` de `analysis/` ya está limpio, así que basta con volver a tejerlo:

```bash
Rscript -e 'rmarkdown::render("analysis/sleep_health_analysis.Rmd", output_dir = "report")'
```

Mientras tanto, las figuras del análisis están en `figures/`.
