######## 0) Preparar el entorno de trabajo #########################

rm(list = ls())   # limpiar entorno (opcional)
setwd("C:/Users/labau/OneDrive/Escritorio/UN/Modelos lineales/Proyecto")

library(readxl)
library(rsample)
library(skimr)
library(dplyr)
library(Matrix)
library(patchwork)


# Manejo eficiente de datos
library(data.table)
setDTthreads(percent = 70)  # PARA PORTÁTIL: usar solo 70% de núcleos

# Pipe y manejo de strings
library(magrittr)
library(stringr)

# Entrenar XGBoost
library(xgboost)

# Preprocesamiento (dummies, partición, etc.)
library(caret)

# Tuning 
library(mlr)


# Visualización
library(ggplot2)


######## 1) Cargar una base de datos y prepararla #########################  
Bogota <- read_excel("Bogota.xlsx")
sapply(Bogota, class)

var_cat <- c("tiene_rut","registro_camara_comercio","naturaleza_registro", "registro_renovado",
             "lleva_contabilidad", "tiempo_funcionamiento", "razon_no_dispositivo",
             "tiene_sitio_web", "tiene_redes_sociales", "tiene_servicio_internet", "razon_no_uso_internet",
             "tipo_conexion_internet", "velocidad_conexion","solicitud_prestamo_ultimo_anio", 
             "razon_no_solicitud_prestamo", "prestamo_solicitado_quien",
             "prestamo_aprobado", "ahorro_dinero", "donde_ahorro", "sector_economico")

Bogota[var_cat] <- lapply(Bogota[var_cat], as.factor)

for(v in var_cat){
  Bogota[[v]] <- as.character(Bogota[[v]])
  Bogota[[v]][is.na(Bogota[[v]])] <- "No_aplica"
  Bogota[[v]] <- as.factor(Bogota[[v]])
}

cor(Bogota$total_ingresos_mes, Bogota$contabilidad_ingresos_anio)
Bogota <- Bogota %>% select(-total_ingresos_mes)
Bogota <- Bogota %>% select(-gasto_mercancia_vendida_mes)
Bogota <- Bogota %>% select(-personas_prestaciones_sociales)


# ------------------------
# Calcular los percentiles 5 y 95
percentil_10 <- quantile(Bogota$contabilidad_ingresos_anio, probs = 0.1, na.rm = TRUE)
percentil_90 <- quantile(Bogota$contabilidad_ingresos_anio, probs = 0.9, na.rm = TRUE)

Bogota <- Bogota %>% 
  filter(contabilidad_ingresos_anio >= percentil_10 & 
           contabilidad_ingresos_anio <= percentil_90)

# ----------------------------
# Análisis descriptivo

# ANÁLISIS DESCRIPTIVO DE LA VARIABLE DE INTERÉS
var_y <- "contabilidad_ingresos_anio"

# Estadísticas descriptivas
desc_y <- summary(Bogota[[var_y]])
desc_y

quantile(Bogota[[var_y]], probs = seq(0, 1, 0.1), na.rm = TRUE)


# Histograma
p1 <- ggplot(Bogota, aes(x = .data[[var_y]])) +
  geom_histogram(
    bins = 50,
    fill = "#4C72B0",   # azul académico
    color = "black",
    alpha = 0.85
  ) +
  labs(
    title = "Distribución del ingreso anual",
    x = "Ingreso anual",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Boxplot
p2 <- ggplot(Bogota, aes(y = .data[[var_y]])) +
  geom_boxplot(
    fill = "#55A868",   # verde azulado sobrio
    color = "black",
    alpha = 0.85,
    outlier.color = "black",
    outlier.size = 1
  ) +
  labs(
    title = "Boxplot del ingreso anual",
    y = "Ingreso anual"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Panel conjunto
(p1 + p2) +
  plot_annotation(tag_levels = "a")



# 2) ANÁLISIS DESCRIPTIVO DE TODAS LAS COVARIABLES

# Separar numéricas y categóricas
vars_numericas <- names(Bogota)[sapply(Bogota, is.numeric)]
vars_numericas <- setdiff(vars_numericas, var_y)

vars_categoricas <- names(Bogota)[sapply(Bogota, is.factor)]

# ---- Resumen numéricas
desc_num <- psych::describe(Bogota[, vars_numericas])
desc_num

# ---- Resumen categóricas
for (v in vars_categoricas) {
  cat("\n=============================\n")
  cat("Variable categórica:", v, "\n")
  print(table(Bogota[[v]], useNA = "ifany"))
}


# Analisis de correlacion de las covaribles numericas y la varible de interes

cor_pearson <- sapply(vars_numericas, function(v) {
  cor(Bogota[[var_y]], Bogota[[v]], method = "pearson")
})

cor_spearman <- sapply(vars_numericas, function(v) {
  cor(Bogota[[var_y]], Bogota[[v]], method = "spearman")
})

cor_kendall <- sapply(vars_numericas, function(v) {
  cor(Bogota[[var_y]], Bogota[[v]], method = "kendall")
})


library(XICOR)
cor_xi <- sapply(vars_numericas, function(v) {
  xicor(Bogota[[var_y]], Bogota[[v]])
})


cor_all <- data.frame(
  variable = vars_numericas,
  pearson = cor_pearson,
  spearman = cor_spearman,
  kendall = cor_kendall,
  xi = cor_xi
)

cor_all <- cor_all[order(abs(cor_all$pearson), decreasing = TRUE), ]
cor_all

# Gráfico de barras ordenado
library(tidyr)

cor_long <- pivot_longer(
  cor_all,
  cols = c("pearson", "spearman", "kendall", "xi"),
  names_to = "metodo",
  values_to = "correlacion"
)


ggplot(
  cor_long,
  aes(
    x = reorder(variable, correlacion),
    y = correlacion,
    color = metodo,
    group = metodo
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Asociación entre las covariables y el ingreso anual",
    subtitle = "Comparación de diferentes medidas de correlación",
    x = "Covariable",
    y = "Coeficiente de asociación",
    color = "Método"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom"
  )


# Analisis de asociacion de las covaribles categoricas y la varible de interes

kw_results <- lapply(vars_categoricas, function(v) {
  test <- kruskal.test(Bogota[[var_y]] ~ Bogota[[v]])
  data.frame(
    variable = v,
    p_value = test$p.value
  )
})

kw_results_df <- do.call(rbind, kw_results)
kw_results_df <- kw_results_df[order(kw_results_df$p_value), ]
kw_results_df






#-----------------------------------------
# PARTICIÓN ENTRENAMIENTO–PRUEBA (80%-20%) 
# Estratificado en contabilidad_ingresos_anio (mejor estabilidad)
set.seed(123)

train_index <- createDataPartition(Bogota$contabilidad_ingresos_anio, p = 0.8, list = FALSE)

muestra1 <- Bogota[train_index, ]   # entrenamiento
muestra2 <- Bogota[-train_index, ]  # prueba

# IDENTIFICAR VARIABLES
y_var  <- "contabilidad_ingresos_anio"
x_vars <- setdiff(names(muestra1), y_var)

# MATRIZ DISEÑO PARA ENTRENAMIENTO
XF_train <- Matrix::sparse.model.matrix(
  as.formula(paste0(y_var, "~ .")),
  data = muestra1[, c(x_vars, y_var), with = FALSE]
)[, -1]

# MATRIZ DISEÑO PARA PRUEBA
XF_test <- Matrix::sparse.model.matrix(
  as.formula(paste0(y_var, "~ .")),
  data = muestra2[, c(x_vars, y_var), with = FALSE]
)[, -1]

# VECTORES OBJETIVO
y_train <- muestra1[[y_var]]
y_test  <- muestra2[[y_var]]



# ----------------------------
# XGBoost
# ----------------------------


######## 3) XGBOOST #########################
######## 3.1) Parámetros iniciales para el ajuste de hiperparámetros #########################

#=====================================================================
##Propósito de esta sección
#1.Definir cuántas combinaciones de hiperparámetros se van a probar.
#2.Definir cómo será la validación cruzada.
#3.Definir límites para nrounds (cantidad de boosting rounds).
#4.Definir “early stopping” para evitar sobreajuste.
#5.Identificar la variable objetivo y los predictores.
#======================================================================

######## 3.1) Parámetros iniciales para XGBoost #########################

# =============================================================
# OBJETIVO DE ESTA SECCIÓN
# 1. Cuántas combinaciones de hiperparámetros se probarán.
# 2. Qué esquema de validación cruzada se usará.
# 3. Límites para nrounds.
# 4. Early stopping para evitar sobreajuste.
# 5. Definir variable objetivo y predictores.
# =============================================================

# 1) Número máximo de combinaciones que probará el tuning
max.iter.par.tun <- 40L  

# 2) Validación cruzada dentro del tuning
folds.par.tun <- 5L       # no 10 (muy pesado)
reps.par.tun  <- 2L

# 3) Rondas máximas que intentará cada XGBoost antes del early stopping
nRound.w <- 1000L          

# 4) Early stopping
EarlyStop.nR <- 30L       # permitir 20 rondas sin mejora

# 5) VARIABLE OBJETIVO Y VARIABLES PREDICTORAS
.y.n  <- y_var   # "contabilidad_ingresos_anio"
.Xs.n <- x_vars  # todos los demás nombres

# 6) La base activa será la muestra de entrenamiento
dt1 <- muestra1


######### 3.2.1) Espacio de hiperparámetros para XGBoost (Regresión) #########

xgb_params <- makeParamSet(
  
  # Tasa de aprendizaje (cuánto aporta cada árbol al modelo)
  # Valores bajos = aprendizaje lento pero modelos más estables
  makeNumericParam("eta", lower = 0.01, upper = 0.2),
  
  # Profundidad de los árboles.
  # Recomendado para tabulares: 3 a 10
  makeIntegerParam("max_depth", lower = 3, upper = 10),
  
  # Regularización L2
  makeNumericParam("lambda", lower = 0, upper = 20),
  
  # Regularización que controla creación de nodos
  makeNumericParam("gamma", lower = 0, upper = 10),
  
  # Submuestreo de columnas por nivel
  makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
  
  # Submuestreo de columnas por árbol (muy importante en datos con muchos predictores)
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  
  # Submuestreo de filas para cada árbol (evita sobreajuste)
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  
  # Peso mínimo requerido para crear un nodo hijo
  makeIntegerParam("min_child_weight", lower = 1, upper = 25),
  
  # Paso máximo permitido para cada hoja
  makeIntegerParam("max_delta_step", lower = 0, upper = 10)
)


######### 3.2.2) Hyperparameter Tuning configuration (Regresión) #########

#===========================================================================
# Objetivo:
# 1. Definir cómo buscar hiperparámetros.
# 2. Definir cómo evaluar cada combinación.
# 3. Especificar métricas correctas para REGRESIÓN (RMSE, MAE).
#===========================================================================

##### 3.2.2.1) Tipo de búsqueda y número de iteraciones ######
# Búsqueda aleatoria (ideal para portátil)
control <- makeTuneControlRandom(
  maxit = max.iter.par.tun,
  tune.threshold = FALSE
)


##### 3.2.2.2) Validación cruzada ######
# K-fold cross validation con repetición
resample_desc <- makeResampleDesc(
  method = "RepCV",
  folds = folds.par.tun,
  reps  = reps.par.tun,
  stratify = FALSE
)


##### 3.2.2.3) Métrica(s) de evaluación para REGRESIÓN ######

# Puedes usar cualquiera de estas:
# rmse, mse, mae, rsq

measure_rmse <- mlr::rmse     # raíz del error cuadrático medio
measure_mae  <- mlr::mae      # error absoluto medio
measure_mse  <- mlr::mse      # error cuadrático medio

# Para tuning usaremos RMSE (más estándar para modelos de ingresos)
xgb_measure <- measure_rmse


##### 3.2.3) Definir el learner de XGBoost para REGRESIÓN #######

xgb_learner <- makeLearner(
  "regr.xgboost",
  predict.type = "response",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  nrounds = nRound.w,
  early_stopping_rounds = EarlyStop.nR,
  maximize = FALSE
)


### Verificar configuración del learner
getLearnerProperties(xgb_learner)
getLearnerType(xgb_learner)
getHyperPars(xgb_learner)


##### 3.2.4) Preparar los datos para XGBoost y mlr  #####

# 1. Crear DMatrix para XGBoost (formato eficiente)
dtrain <- xgb.DMatrix(
  data  = XF_train,
  label = y_train
)

dtest <- xgb.DMatrix(
  data  = XF_test,
  label = y_test
)

# 2. Crear Task para mlr (necesario para tuneParams)
# mlr exige un data.frame tradicional
dt_train_mlr <- data.frame(
  as.matrix(XF_train),
  y = y_train
)

trainTask <- makeRegrTask(
  data   = dt_train_mlr,
  target = "y"
)

##### 3.2.5) ¡Tuning de hiperparámetros! #####
gc()
set.seed(1234)

# Detectar núcleos del procesador
total_cores <- parallel::detectCores()

# --- IMPORTANTE ---
# En portátiles NO usar todos los núcleos → se sobrecalienta.
# Usamos aprox. la mitad:
safe_cores <- max(1, floor(total_cores * 0.5))
safe_cores
# ------------------

# Iniciar paralelización
parallelMap::parallelStart(
  mode  = "socket",
  cpu   = safe_cores,
  level = "mlr.tuneParams"
)

# Cargar librerías necesarias en cada worker
parallelMap::parallelLibrary(packages = c("data.table", "magrittr"))


# Medir tiempo
t0 <- Sys.time()

tuned_params <- tuneParams(
  learner    = xgb_learner,
  task       = trainTask,
  resampling = resample_desc,
  par.set    = xgb_params,
  control    = control,
  measures   = xgb_measure  
)

parallelMap::parallelStop()

gc()

t1 <- Sys.time()
t1 - t0


##### 3.3) Selección del número óptimo de iteraciones #####

# Recuperar mejores hiperparámetros del tuning
best.tuned_params <- tuned_params$x

# Para seguridad, forzamos el objetivo a regresión (a veces mlr guarda de más)
best.tuned_params$objective <- "reg:squarederror"

# Número de rondas máximo (ya definido)
# nRound.w  # 1000 típicamente

# Early stopping (ya definido)
# EarlyStop.nR  # 20 recomendado

# Usar SOLO la parte entrenamiento
XF_cv <- XF_train
yF_cv <- y_train

set.seed(12345)
t0 <- Sys.time()

xgb_cv_res <- xgb.cv(
  data  = XF_cv,
  label = yF_cv,
  params = best.tuned_params,
  nthread = safe_cores,          # mitad de núcleos → seguro en laptop
  nrounds = nRound.w,            # máximo permitido
  nfold = folds.par.tun,         # 10 folds
  verbose = TRUE,
  early_stopping_rounds = EarlyStop.nR,
  maximize = FALSE,              # RMSE se MINIMIZA
  eval_metric = "rmse"
)

t1 <- Sys.time()
t1 - t0

best.iter <- xgb_cv_res$best_iteration
best.iter


#### 3.4) Entrenar el modelo XGBoost final (SOLO con muestra de entrenamiento) ####

# Preparar DMatrix SOLO con los datos de entrenamiento
dtrain_final <- xgb.DMatrix(
  data  = XF_train,
  label = y_train
)

# Aseguramos parámetros correctos
best.tuned_params$objective  <- "reg:squarederror"
best.tuned_params$eval_metric <- "rmse"

# Entrenar modelo final
set.seed(123456)
t0 <- Sys.time()

xgb_final <- xgboost(
  data = dtrain_final,
  params = best.tuned_params,
  nrounds = best.iter,         
  nthread = safe_cores,        
  verbose = TRUE
)

t1 <- Sys.time()
t1 - t0   # tiempo de entrenamiento

# Predicción en la misma muestra de entrenamiento
pred_train <- predict(xgb_final, dtrain_final)
summary(pred_train)


######## 4) Importancia de variables en el modelo XGBoost
model.importance <- xgb.importance(
  feature_names = colnames(XF_train),
  model = xgb_final
)

# Ordenar de mayor a menor "Gain"
model.importance <- model.importance[order(-Gain)]

# Comprobar que las contribuciones suman 1
sum(model.importance$Gain)

# Crear acumulado para inspección
model.importance$Gain_cumsum <- cumsum(model.importance$Gain)

# Dimensiones: número de "features" creados
dim(model.importance)

# Ver las 20 variables más importantes
head(model.importance, 20)



######## 5) Abilidad predictiva del modelo ##########################

pred_test <- predict(xgb_final, XF_test)

library(Metrics)

# RMSE
rmse_xgb <- rmse(y_test, pred_test)

# MAE
mae_xgb  <- mae(y_test, pred_test)

# R^2 de predicción
# Cuánto mejora tu modelo al predecir datos nuevos frente a usar solo la media.
r2_xgb   <- 1 - sum((y_test - pred_test)^2)/sum((y_test - mean(y_test))^2)

# Mostrar resultados
rmse_xgb
mae_xgb
r2_xgb


######## 6) Modelo interpretable ##########################
# Instalar si no lo tienes
# install.packages("SHAPforxgboost")

library(shapviz)

XF_train_dense <- as.matrix(XF_train)

shap_obj <- shapviz(
  xgb_final,
  X = XF_train_dense,
  X_pred = XF_train_dense
)



# Importancia SHAP
sv_importance(shap_obj)

# Explicación individual
#sv_waterfall(shap_obj, row_id = 1)







#===============================================================================
#===============================================================================

# ----------------------------
# CatBoost
# ----------------------------

######## 0). Instalar y Cargar librería  ##########################

#instalando el paquete CatBoost en tu sistema desde el archivo .tgz (fuera de CRAN).
#install.packages("remotes")
#remotes::install_url(url = "https://github.com/catboost/catboost/releases/download/v1.2.8/catboost-R-windows-x86_64-1.2.8.tgz",INSTALL_opts = c("--no-multiarch", "--no-test-load"))


# Cargar librería
library(catboost)



######## 1). Preparar datos — usar los objetos que ya se tienen  ###############

train_df <- as.data.frame(muestra1[, c(x_vars, y_var), with = FALSE])
test_df  <- as.data.frame(muestra2[, c(x_vars, y_var), with = FALSE])

# Etiquetas
y_train <- train_df[[y_var]]
y_test  <- test_df[[y_var]]

# Matriz X (sin la variable objetivo)
X_train <- train_df[, x_vars, drop = FALSE]
X_test  <- test_df[, x_vars, drop = FALSE]

# Crear Pools (si tus factores están como factor, CatBoost los detecta)
dtrain_pool <- catboost.load_pool(data = X_train, label = y_train)
dtest_pool  <- catboost.load_pool(data = X_test,  label = y_test)



######## 2). Búsqueda de hiperparámetros ##########################

set.seed(123456)
n_iter_search <- 25   

best_res <- list(rmse = Inf, params = NULL, best_iter = NULL)

for (i in seq_len(n_iter_search)) {
  
  # Hiperparámetros mejorados
  params_try <- list(
    loss_function = "RMSE",
    iterations = 2000L,
    learning_rate = runif(1, 0.01, 0.15),
    depth = sample(4:10, 1),
    l2_leaf_reg = runif(1, 3, 25),
    bagging_temperature = runif(1, 0.0, 1.0),
    random_strength = runif(1, 0.5, 2.0),
    rsm = runif(1, 0.6, 1.0),
    border_count = sample(c(32, 64, 128), 1),
    od_type = "Iter",
    od_wait = 50,
    random_seed = sample(1:9999, 1),
    logging_level = "Silent"   # <<— AQUÍ SÍ ES VÁLIDO
  )
  
  # cv con parámetros nuevos
  cv_res <- catboost.cv(
    params = params_try,
    pool = dtrain_pool,
    fold_count = 5,
    partition_random_seed = params_try$random_seed,
    shuffle = TRUE
  )
  
  # extraer mejor RMSE
  rmse_col <- grep("test.*RMSE.*mean", colnames(cv_res), ignore.case = TRUE, value = TRUE)[1]
  rmse_vec <- cv_res[[rmse_col]]
  
  rmse_min <- min(rmse_vec)
  best_it <- which.min(rmse_vec)
  
  if (rmse_min < best_res$rmse) {
    best_res$rmse <- rmse_min
    best_res$params <- params_try
    best_res$best_iter <- best_it
  }
  
  message(sprintf(
    "[%02d/%02d] RMSE=%.3f | iter=%d | LR=%.4f depth=%d L2=%.2f",
    i, n_iter_search, rmse_min, best_it,
    params_try$learning_rate,
    params_try$depth,
    params_try$l2_leaf_reg
  ))
}


best_res


######## 3). Determinar best.iter con catboost.cv ##########################

params_final <- best_res$params

cv_final <- catboost.cv(
  params = params_final,
  pool = dtrain_pool,
  fold_count = 5,
  partition_random_seed = params_final$random_seed,
  shuffle = TRUE
)

# detectar columna RMSE
rmse_col <- grep("test.*RMSE.*mean", colnames(cv_final), ignore.case = TRUE, value = TRUE)[1]

best.iter <- which.min(cv_final[[rmse_col]])
best.iter




######## 4). Entrenar el modelo con la muestra de entrenamiento ################

# Copiar mejores parámetros
params_train <- best_res$params

# Sobrescribir iterations por el óptimo encontrado
params_train$iterations <- best.iter

# Entrenar modelo final
set.seed(123)
catboost_model <- catboost.train(
  learn_pool = dtrain_pool,
  params = params_train
)


######## 5) Predecir en la muestra de prueba (muestra2) y calcular métricas ################

pred_test2 <- catboost.predict(catboost_model, dtest_pool, prediction_type = "RawFormulaVal")
# RawFormulaVal devuelve la predicción numérica para regresión

# RMSE
rmse_cat <- rmse(y_test, pred_test2)

# MAE
mae_cat  <- mae(y_test, pred_test2)

# R^2 de predicción
r2_cat   <- 1 - sum((y_test - pred_test2)^2)/sum((y_test - mean(y_test))^2)

# Mostrar resultados
rmse_cat
mae_cat
r2_cat



######## 6) Importancia de variables  ################

safe_cores <- max(1, parallel::detectCores() - 1)

fi <- catboost.get_feature_importance(
  model = catboost_model,
  pool = dtrain_pool,
  type = "FeatureImportance",
  thread_count = safe_cores
)


fi_df <- data.frame(
  feature = colnames(X_train),
  importance = fi
)
fi_df <- fi_df[order(-fi_df$importance), ]
head(fi_df, 30)


# Gráfico simple
library(ggplot2)
ggplot(head(fi_df, 20), aes(x = reorder(feature, importance), y = importance)) +
  geom_col() + coord_flip() + labs(title = "CatBoost - Feature Importance (FeatureImportance)", y = "Importance")



######## 7) SHAP (explicaciones locales/globales) con CatBoost ################

# Obtener SHAP values
shap_mat <- catboost.get_feature_importance(
  model = catboost_model,
  pool = dtrain_pool,
  type = "ShapValues",
  thread_count = safe_cores
)

# shap_mat: filas = n_obs, columnas = n_features + 1 (última es expected value)
nfeat <- ncol(shap_mat) - 1
shap_vals <- shap_mat[, 1:nfeat, drop = FALSE]


# importancia SHAP promedio (mean absolute)
shap_mean_abs <- colMeans(abs(shap_vals))
shap_df <- data.frame(
  feature = colnames(X_train),
  mean_abs_shap = shap_mean_abs
)
shap_df <- shap_df[order(-shap_df$mean_abs_shap), ]
head(shap_df, 20)


# Si quieres un plot tipo beeswarm / summary, puedes usar 'shapviz' o 'fastshap' (algunas adaptaciones necesarias)
# Ejemplo: barras SHAP
ggplot(head(shap_df, 20),
       aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
  geom_col() + coord_flip() +
  labs(title = "CatBoost - SHAP mean(|value|)")




#===============================================================================
#===============================================================================

# ----------------------------
# LASSO
# ----------------------------

library(glmnet)

#transforma las variables cualitativas en dummy porque 
#LASSO solo aceptan var. cuantitativas

x_train <- model.matrix(contabilidad_ingresos_anio ~ ., data = muestra1)[,-1]
y_train <- muestra1$contabilidad_ingresos_anio

x_test <- model.matrix(contabilidad_ingresos_anio ~ ., data = muestra2)[,-1]
y_test <- muestra2$contabilidad_ingresos_anio

# Validación cruzada para escoger lambda
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

# Predicciones
pred_lasso <- predict(cv_lasso, s = best_lambda_lasso, newx = x_test)

# RMSE
rmse_lasso <- rmse(y_test, pred_lasso)

# MAE
mae_lasso  <- mae(y_test, pred_lasso)

# R^2 de predicción
r2_lasso   <- 1 - sum((y_test - pred_lasso)^2)/sum((y_test - mean(y_test))^2)

# Mostrar resultados
rmse_lasso
mae_lasso
r2_lasso


# Varibles consideradas
coef_lasso <- coef(cv_lasso, s = "lambda.min")

coef_vec   <- as.numeric(coef_lasso)
coef_names <- rownames(coef_lasso)

names(coef_vec) <- coef_names


# variables con coeficiente distinto de cero
coef_vec <- coef_vec[names(coef_vec) != "(Intercept)"]
coef_vec <- coef_vec[coef_vec != 0]

# Variables seleccionadas por LASSO
lasso_selected_vars <- names(coef_vec)
lasso_selected_vars



# ----------------------------
# RIDGE
# ----------------------------

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min

pred_ridge <- predict(cv_ridge, s = best_lambda_ridge, newx = x_test)

# Predicciones
pred_ridge <- predict(cv_lasso, s = best_lambda_ridge, newx = x_test)

# RMSE
rmse_ridge <- rmse(y_test, pred_ridge)

# MAE
mae_ridge  <- mae(y_test, pred_ridge)

# R^2 de predicción
r2_ridge   <- 1 - sum((y_test - pred_ridge)^2)/sum((y_test - mean(y_test))^2)

# Mostrar resultados
rmse_ridge
mae_ridge
r2_ridge


# ----------------------------
# lad
# ----------------------------

library(quantreg)
library(caret)

# Matrices completas
x_train_full <- model.matrix(contabilidad_ingresos_anio ~ ., data = muestra1)[,-1]
x_test_full  <- model.matrix(contabilidad_ingresos_anio ~ ., data = muestra2)[,-1]

y_train <- muestra1$contabilidad_ingresos_anio
y_test  <- muestra2$contabilidad_ingresos_anio

# Variables seleccionadas por LASSO
x_train_lad <- x_train_full[, lasso_selected_vars, drop = FALSE]
x_test_lad  <- x_test_full[,  lasso_selected_vars, drop = FALSE]

# Eliminar alta colinealidad
cor_mat <- cor(x_train_lad, use = "pairwise.complete.obs")
high_corr <- findCorrelation(cor_mat, cutoff = 0.95)

if (length(high_corr) > 0) {
  x_train_lad2 <- x_train_lad[, -high_corr, drop = FALSE]
  x_test_lad2  <- x_test_lad[,  -high_corr, drop = FALSE]
} else {
  x_train_lad2 <- x_train_lad
  x_test_lad2  <- x_test_lad
}

# Estandarizar
x_train_lad2 <- scale(x_train_lad2)
x_test_lad2  <- scale(
  x_test_lad2,
  center = attr(x_train_lad2, "scaled:center"),
  scale  = attr(x_train_lad2, "scaled:scale")
)

# Agregar intercepto
x_train_lad2 <- cbind(Intercept = 1, x_train_lad2)
x_test_lad2  <- cbind(Intercept = 1, x_test_lad2)

# Ajustar modelo LAD
lad_model <- rq.fit(
  x = x_train_lad2,
  y = y_train,
  tau = 0.5,
  method = "fn"
)

# Predicción
pred_lad <- as.vector(x_test_lad2 %*% lad_model$coefficients)

# Métricas
rmse_lad <- rmse(y_test, pred_lad)
mae_lad  <- mae(y_test, pred_lad)
r2_lad   <- 1 - sum((y_test - pred_lad)^2) /
  sum((y_test - mean(y_test))^2)

rmse_lad
mae_lad
r2_lad
