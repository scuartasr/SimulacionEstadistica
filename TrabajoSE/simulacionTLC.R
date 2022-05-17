
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
  asim <- apply(X = muestras,
                MARGIN = 2,
                FUN = skewness)
  asim <- median(asim)
  curt <- apply(X = muestras,
                MARGIN = 2,
                FUN = kurtosis)
  curt <- median(curt)
  prom <- apply(X = muestras,
                MARGIN = 2,
                FUN = mean)
  
  normalidad <- lillie.test(prom)
  resultados <- c(n, lambda, m, asim, curt,
                  normalidad$statistic, normalidad$p.value)
  write(x = resultados,
        file = "resultados.txt",
        ncolumns = 7,
        append = T)
}

# Función para la replicación de r veces de la función que obtiene los
# promedios

# tamanos <- seq(5, 50, 5)

replicacion_n <- function(r = 500, m = 200, lambda) {
  tamanos <- seq(30, 1000, 5)
  long <- length(tamanos)
  lapply(1:long, function(i) {
    cat(i, " ")
    replicate(r, simulaciones_poisson(lambda = lambda, n = tamanos[i] ))
  })
  
  
}

# Repetición para diferentes lambdas

lambdas <- seq(0.20, 15, 0.2)
n_lambdas <- length(lambdas)

lapply(1:n_lambdas, function(i) {
  cat("\nPara lambda: ", lambdas[i], " ")
  replicacion_n(lambda = lambdas[i])
})