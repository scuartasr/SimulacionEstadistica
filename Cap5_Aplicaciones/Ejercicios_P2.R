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
    costo_variable <- runif(1, 16, 24) * 60000
    demanda <- rnorm(1, mean = 1200, sd = 300)
    ventas <- demanda * precio
    utilidad <- ventas - costo_variable - costo_fijo
    utilidades <- c(utilidades, utilidad)
  }
  
  
  return(utilidades)
}

x <- simulacion_utilidad(10000, costo_fijo = 30000, precio = 50)

# Literal A. Utilidad media
paste("La utilidad media es de ", mean(x))

# Literal B. Distribución empírica de la utilidad
density(x)
plot(density(x))

# Literal C.
perdida <- x[x < 0]
prob_perdida <- length(perdida) / length(x)


### ### ---------------------------------------------------
### ### Ejercicio cinco. Ventas del muñeco Bobby
### ### ---------------------------------------------------

rm(list = ls())

simulacion_juguete <- function(n, fijo, variable, venta,
                               produccion, salvamento) {
  # Creación de la tabla
  tbl <- data.frame(matrix(ncol = 8, nrow = n))
  colnames(tbl) <- c("simulacion", "demanda", "costo_var", "costo_tot",
                     "navidad", "salvamento", "total", "utilidad")
  
  # Simulación
  
  tbl$simulacion <- seq(1, n)
  tbl$demanda <- round(rnorm(n, mean = 60000, sd = 15000))
  tbl$costo_var <- produccion * variable
  tbl$costo_tot <- tbl$costo_var + fijo
  tbl$navidad <- ifelse(tbl$demanda > produccion,
                        produccion * venta,
                        tbl$demanda * venta)
  tbl$salvamento <- ifelse(tbl$demanda >= produccion,
                           0,
                           10 * (produccion - tbl$demanda))
  tbl$total <- tbl$navidad + tbl$salvamento
  tbl$utilidad <- tbl$total - tbl$costo_tot

  # Retorno
  retorno <- list(mean(tbl$utilidad), tbl)
  names(retorno) <- c("media", "datos")
  return(retorno)
  
}

simulacion_60K <- simulacion_juguete(10000, 100000, 34, 42, 60000, 10)
simulacion_50K <- simulacion_juguete(10000, 100000, 34, 42, 50000, 10)
simulacion_70K <- simulacion_juguete(10000, 100000, 34, 42, 70000, 10)

resultados <- as.numeric(c(paste0(5:7,"0000")))
utilidades <- c(simulacion_50K$media, simulacion_60K$media,
                simulacion_70K$media)
resumen <- data.frame(resultados, utilidades)
barplot(utilidades~resultados)
title("Utilidades según la producción")

### ### ---------------------------------------------------
### ### Ejercicio seis. Borrachito.
### ### ---------------------------------------------------

rm(list = ls())

c1 <- rep(1, 7)
c2 <- rep(2, 7)
c3 <- rep(3, 7)
c4 <- rep(4, 7)
c5 <- rep(5, 7)

habitacion <- rbind(c1, c2, c3, c4, c5)
# 
# borrachito <- function() {
#   fila <- 1
#   contador <- 
#   
#   while(f)
# }




























