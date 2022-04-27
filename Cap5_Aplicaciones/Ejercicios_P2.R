### ### ### ------------------------------------------------------- ### ### ###
### ### ### ------------------------------------------------------- ### ### ###
### ### ### --------------- EJERCICIOS DE APLICACIÓN -------------- ### ### ###
### ### ### ------------------------------------------------------- ### ### ###
### ### ### ------------------------------------------------------- ### ### ###

### ### ---------------------------------------------------
### ### Librerías
### ### ---------------------------------------------------

library(tidyverse)

### ### ---------------------------------------------------
### ### Ejercicio uno. Valor de una acción en la bolsa
### ### ---------------------------------------------------

rm(list = ls())

accion_bolsa <- function(x) {
  ifelse(x < 0.25, 0,
         ifelse(x < 0.45, 1,
                ifelse(x < 0.65, 2,
                       ifelse(x < 0.75, 3,
                              ifelse(x < 0.85, 4,
                                     ifelse(x < 0.95, -1, -2))))))
}

simulacion_accion <- function(n, inicial) {
  # Se solicita ingresar el número n de simulaciones a realizar y el precio
  # inicial de la acción
  
  annos <- data.frame()
  
  for (i in 1:n) {
    x <- inicial
    accion <- c()                     # Vector de valores de la acción
    for (i in 1:12) {
      nuevo_valor <- accion_bolsa(runif(1))  # Cambio en el valor de la acción
      x <- x + nuevo_valor       # Nuevo valor de la acción
      accion <- append(accion, x)      # Agregar valor nuevo al vector
    }
    annos <- rbind(annos, accion)
  }
  
  colnames(annos) <- paste0("mes", 1:12)
  
  return(annos)
}

acciones <- simulacion_accion(15000, 39)
paste("El valor medio de la acción es: $", mean(acciones$mes12),
      " y su varianza es: $", var(acciones$mes12))

### XXXX. No entiendo la pregunta

### ### ---------------------------------------------------
### ### Ejercicio dos. Tiempo para finalizar cuatro actividades
### ### ---------------------------------------------------

rm(list = ls())

actividad_A <- function(x) {
  ifelse(x < 0.35, 6,
         ifelse(x < 0.60, 5,
                ifelse(x < 0.85, 7, 8)))
}

actividad_B <- function(x) {
  ifelse(x < 0.55, 5,
         ifelse(x < 0.8, 7, 3))
}

actividad_C <- function(x) {
  ifelse(x < 0.4, 14,
         ifelse(x < 0.65, 12,
                ifelse(x < 0.85, 16,
                       ifelse(x < 0.95, 10, 18))))
}

actividad_D <- function(x) {
  ifelse(x < 0.6, 8, 10)
}

tiempo_actividades <- function(n) {
  simulaciones <- data.frame()
  
  for (i in 1:n) {
    # Simulación de las actividades
    aleatorios <- runif(4)
    a <- actividad_A(aleatorios[1])
    b <- actividad_B(aleatorios[2])
    c <- actividad_C(aleatorios[3])
    d <- actividad_D(aleatorios[4])
    
    # Tiempo total
    fila <- c(a, b, c, d, (a+b+c+d))
    
    # Adición al marco de datos
    
    simulaciones <- rbind(simulaciones, fila)
  }
  
  colnames(simulaciones) <- c("A", "B", "C", "D", "Suma")
  return(simulaciones)
}

n = 35000

simulacion_actividades <- tiempo_actividades(n)

simulacion_35 <- simulacion_actividades %>% 
  dplyr::filter(Suma <= 35)

rta <- nrow(simulacion_35) * 100 / n

paste("La probabilidad de terminar el proyecto en 35 semanas o menos es del", 
      round(rta, 3), " %.")


### ### ---------------------------------------------------
### ### Ejercicio tres. Producto nuevo
### ### ---------------------------------------------------

rm(list = ls())

costo_compra <- function(x) {
  ifelse(x < 0.45, 11,
         ifelse(0.75, 12, 10))
}

costo_mano <- function(x) {
  ifelse(x < 0.35, 24,
         ifelse(x < 0.65, 25,
                ifelse(x < 0.9, 22, 20)))
}

costo_transporte <- function(x) {
  ifelse(x < 0.75, 3, 5)
}

utilidad <- function(n, precio) {
  casos <- data.frame()
  
  for (i in 1:n) {
    valores <- runif(3)
    compra <- costo_compra(valores[1])
    mano <- costo_mano(valores[2])
    transporte <- costo_transporte(valores[3])
    total <- compra + mano + transporte
    utilidad <- precio - total
    
    
    fila <- c(compra, mano, transporte, total, utilidad)
    
    casos <- rbind(casos, fila)
  }
  
  colnames(casos) <- c("compra", "mano", "transporte", "total", "utilidad")
  
  return(casos)
}

n = 40000
precio = 45

simulacion_utilidad <- utilidad(n, precio)

rta <- nrow(simulacion_utilidad[simulacion_utilidad$utilidad < 5, ]) * 100 / n

paste("La probabilidad de que la utilidad sea menor a $5 es de",
      round(rta, 4), " %.")

# Realizado con n = 25000 y n = 40000. Siempre da 12.2


### ### ---------------------------------------------------
### ### Ejercicio cuatro. Utilidades de Madeira Manufacturing
### ### ---------------------------------------------------
rm(list = ls())

costo_fijo <- 30000

simulacion_utilidad <- function(n, costo_fijo, precio) {
  utilidades <- c()
  
  for (i in 1:n) {
    costo_variable <- runif(1, 16, 24)
    costo_total <- costo_fijo + costo_variable
    demanda <- rnorm(1, mean = 1200, sd = 300)
    ventas <- demanda * 50
    utilidad <- ventas - costo_total
    utilidades <- c(utilidades, utilidad)
  }
  
  
  return(utilidades)
}

x <- simulacion_utilidad(10000, 30000, 50)

# Literal A. Utilidad media
paste("La utilidad media es de ", mean(x))
























