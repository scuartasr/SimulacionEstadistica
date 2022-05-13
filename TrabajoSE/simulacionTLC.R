### ### ### -------------------------------------------------------------------
### ### ### SIMULACIONES, TRABAJO DE SIMULACIÓN ESTADÍSTICA
### ### ### -------------------------------------------------------------------

### ### ### -------------------------------------------------------------------
### ### ### Librerías importantes
### ### ### -------------------------------------------------------------------

library(moments)
library(nortest)
library(magrittr)

# Función para la generación de las muestras aleatorias, el cálculo de
# y el cálculo de cada uno de los promedios

simulaciones_poisson <- function(m = 200, lambda, n) {
  muestras <- replicate(m, rpois(n = n, lambda = lambda))
  # asim <- apply(X = muestras,
  #               MARGIN = 2,
  #               FUN = skewness)
  # curt <- apply(X = muestras,
  #               MARGIN = 2,
  #               FUN = kurtosis)
  prom <- apply(X = muestras,
                MARGIN = 2,
                FUN = mean)
  
  normalidad <- lillie.test(prom)
  resultados <- c(n, lambda, m, normalidad$statistic, normalidad$p.value)
  write(x = resultados,
        file = "resultados.txt",
        ncolumns = 5,
        append = T)
}

# Función para la replicación de r veces de la función que obtiene los
# promedios

replicacion_n <- function(r = 250, m = 200, lambda) {
  tamanos <- seq(30, 500, 5)
  long <- length(tamanos)
  lapply(1:long, function(i) {
    cat(i, " ")
    replicate(r, simulaciones_poisson(lambda = 0.1, n = tamanos[i] ))
  })
  
  
}

# Repetición para diferentes lambdas

lambdas <- seq(0.10, 5, 0.05)
n_lambdas <- length(lambdas)

lapply(1:n_lambdas, function(i) {
  cat("Para lambda: ", lambdas[i])
  replicacion_n(lambda = lambdas[i])
})
