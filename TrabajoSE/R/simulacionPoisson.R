### ### ### --------------------------------------------------------------
### ### ### Lectura de paquetes importantes
### ### ### --------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(nortest)
library(moments)

### ### ### --------------------------------------------------------------
### ### ### Idea
### ### ### --------------------------------------------------------------

# Se van a simular cinco muestras aleatorias de una distribución Poisson
# con medias muy pequeñas por cada valor de tamaño de muestra, y en cada
# caso se va a calcular la simetría, el coeficiente de asimetría y la
# curtosis, así como la realización de un test de Shapiro-Wilks, que
# tiene mayor potencia para valores de n >= 5.

tamanos <- seq(25, 5000, 25)
lambdas <- seq(0.5, 50, 0.5)

tamanos <- rep(tamanos, 1000)
lambdas <- sort(rep(lambdas, 2000))

### ### ### --------------------------------------------------------------
### ### ### Simulación
### ### ### --------------------------------------------------------------

simulaciones = data.frame(matrix(ncol = 10, nrow = 200000))
colnames(simulaciones) <- c("simulacion", "n", "lambda", "semilla", "asimetria",
                            "curtosis", "s_w", "s_p", "lill_d", "lill_p")

n <- 200000

for (i in 1:n) {
  # Generación de la simulación
  simulaciones$simulacion[i] <- i
  simulaciones$n[i] <- tamanos[i]
  simulaciones$lambda[i] <- lambdas[i]
  simulaciones$semilla[i] <- 2022+2*i
  
  # Simulación
  muestra <- rpois(n = tamanos[i], lambda = lambdas[i])
  
  # Momentos
  simulaciones$asimetria[i] <- skewness(muestra)
  simulaciones$curtosis[i] <- kurtosis(muestra)
  
  # Tests de normalidad
  test_shap <- shapiro.test(muestra)
  test_lill <- lillie.test(muestra)
  
  # Últimos parámetros 
  simulaciones$s_w[i] <- as.numeric(test_shap$statistic)
  simulaciones$s_p[i] <- test_shap$p.value
  simulaciones$lill_d[i] <- as.numeric(test_lill$statistic)
  simulaciones$lill_p[i] <- test_lill$p.value
}

save(simulaciones, file = "simulaciones.RData")
write.csv(simulaciones, file = "simulaciones.csv")
