# Optimizacion Bayesiana para canaritos asesinos
# Busca ortimizar los parametros ratio y desvio


# Defino los limites para estos parametros
ratio_min <- 0.1
ratio_max <- 1.0
desvios_min <- 0.1
desvios_max <- 4.0


# defino una funcion de evaluacion
evaluate_model <- function(canaritos_ratio, canaritos_desvios) {
  # Llama a la función CanaritosAsesinos con los parámetros proporcionados
  CanaritosAsesinos(
    canaritos_ratio = canaritos_ratio,
    canaritos_desvios = canaritos_desvios,
    canaritos_semilla = 18
  )
  
  # Entrena el modelo LightGBM y calcula la ganancia
  # Retorna la métrica de ganancia
  ganancia <- #ACA HAY QUE LLENAR CON LA FUNCION DE GANANCIA !!!!!!
    
    return(ganancia)
}


# inicializo muestras aleatorias en el espacio de busqueda
set.seed(123)
initial_samples <- data.frame(
  canaritos_ratio = runif(5, min = ratio_min, max = ratio_max),
  canaritos_desvios = runif(5, min = desvios_min, max = desvios_max)
)

results <- data.frame(
  canaritos_ratio = initial_samples$canaritos_ratio,
  canaritos_desvios = initial_samples$canaritos_desvios,
  ganancia = sapply(1:nrow(initial_samples), function(i) {
    evaluate_model(initial_samples$canaritos_ratio[i], initial_samples$canaritos_desvios[i])
  })
)



# funcion que utiliza los resultados para ajustar una regresion que predice la ganancia con esos hiperparametros
update_surrogate <- function(results) {
  # Usa un modelo de regresión simple (por ejemplo, linear model o loess) para ajustar la superficie de respuesta
  surrogate_model <- loess(ganancia ~ canaritos_ratio + canaritos_desvios, data = results)
  return(surrogate_model)
}



# funcion para definir el proximo conjunto de hiperparametros
acquisition_function <- function(surrogate_model, n_samples = 100) {
  new_samples <- data.frame(
    canaritos_ratio = runif(n_samples, min = ratio_min, max = ratio_max),
    canaritos_desvios = runif(n_samples, min = desvios_min, max = desvios_max)
  )
  
  predictions <- predict(surrogate_model, new_samples)
  
  best_sample <- new_samples[which.max(predictions),]
  return(best_sample)
}



# *******************************************
# realizo la optimizacion bayesiana mediante un for

n_iterations <- 20

for (i in 1:n_iterations) {
  surrogate_model <- update_surrogate(results)
  new_sample <- acquisition_function(surrogate_model)
  
  new_ganancia <- evaluate_model(new_sample$canaritos_ratio, new_sample$canaritos_desvios)
  
  results <- rbind(results, data.frame(
    canaritos_ratio = new_sample$canaritos_ratio,
    canaritos_desvios = new_sample$canaritos_desvios,
    ganancia = new_ganancia
  ))
}

# Los mejores parámetros encontrados
best_result <- results[which.max(results$ganancia),]
print(best_result)
