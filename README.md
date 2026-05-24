# Análisis predictivo de la rentabilidad de los microestablecimientos en Bogotá

Proyecto de modelación estadística y análisis predictivo aplicado a microestablecimientos de Bogotá utilizando datos de la Encuesta de Microestablecimientos (MICRO-2016) del DANE.

## Objetivo

Analizar los factores asociados a la rentabilidad de los microestablecimientos y evaluar el desempeño de diferentes enfoques estadísticos y de aprendizaje automático para la predicción de rentabilidad.

## Metodología

El proyecto integra:

- Estadística descriptiva
- Análisis exploratorio de datos (EDA)
- Análisis de asociación
- Validación fuera de muestra
- Interpretabilidad de modelos mediante valores SHAP

## Modelos implementados

- Regresión Ridge
- Regresión LASSO
- Regresión robusta LAD
- XGBoost
- CatBoost

## Métricas de evaluación

- RMSE
- MAE
- R² predictivo

## Principales hallazgos

Los resultados muestran que, en presencia de alta variabilidad y observaciones influyentes, los modelos regularizados y robustos pueden alcanzar desempeños comparables a modelos más complejos de aprendizaje automático.

Además, variables asociadas con:
- formalización,
- antigüedad del negocio,
- características productivas,

emergen como factores relevantes para explicar la rentabilidad de los microestablecimientos en Bogotá.

## Herramientas utilizadas

- R
- RStudio
- RMarkdown
- Machine Learning
- SHAP Values

## Contenido del repositorio

- `analisis_predictivo.Rmd`
- `analisis_predictivo.pdf`
- `data/`
- `figures/`

## Autor

Lubin Abaunza  
Estadístico — Universidad Nacional de Colombia

GitHub: https://github.com/Lubin-Abaunza
