# Ejercicio de análisis de riesgo

# Generador de números aleatorios para el precio de venta (p)

precio_venta <- function(u) {
  ifelse(u < 0.3, 4,
         ifelse(u < 0.8, 5, 6))
}

costo_variable <- function(u) {
  ifelse(u < 0.1, 2,
         ifelse(u < 0.7, 3, 4))
}

ventas <- function(u) {
  ifelse(u < 0.2, 3,
         ifelse(u < 0.6, 4, 5))
}

# Generador de la tabla de análisis de riesgo con
# simulación Monte Carlo

analisis_riesgo <- function(n) {
  tabla <- data.frame()
  prueba <-  1
  
  for (i in 1:n) {
    aleatorios <- runif(n = 3, min = 0, max = 1)
    valores <- c(precio_venta(aleatorios[1]),
                 costo_variable(aleatorios[2]),
                 ventas(aleatorios[3]))
    utilidad <- (valores[1] - valores[2]) * valores[3] - 5
    observacion <- c(prueba,
                     aleatorios[1], valores[1],
                     aleatorios[2], valores[2],
                     aleatorios[3], valores[3],
                     utilidad)
    tabla <- rbind(tabla, observacion)
    prueba <- prueba + 1
  }
  colnames(tabla) <- c("prueba", "aleat_precio_venta", "precio_venta",
                       "aleat_costo", "costo", "aleat_ventas", "ventas",
                       "utilidad")
  return(tabla)
}

# Análisis de la simulación haciendo n = 25,000 repeticiones

simulacion <- analisis_riesgo(25000)
utilidad_total <- sum(simulacion$utilidad); utilidad_total
utilidad_promedio <- utilidad_total/25000; utilidad_promedio
prop.table(table(simulacion$utilidad))
