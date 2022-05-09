# PARCIAL DE SIMULACIÓN

### -------------------------------------------------------
### Punto uno
### -------------------------------------------------------

costo_compra <- function(u) {
  ifelse(u < 0.5, 12,
         ifelse(u < 0.88, 11, 10))
}

costo_humano <- function(u) {
  ifelse(u < 0.42, 20,
         ifelse(u < 0.75, 24,
                ifelse(u < 0.92, 25, 22)))
}

costo_transporte <- function(u) {
  ifelse(u < 0.83, 5, 3)
}

simulacion_brinkley <- function(n, alfa, gama) {
  tbl <- data.frame(matrix(ncol = 5, nrow = n))
  colnames(tbl) <- c("compra", "mano_obra", "transporte", "precio", "utilidad")
  
  for (i in 1:n) {
    tbl$compra[i] <- costo_compra(runif(1))
    tbl$mano_obra[i] <- costo_humano(runif(1))
    tbl$transporte[i] <- costo_transporte(runif(1))
    tbl$precio[i] <- rgamma(1, shape = alfa, scale = gama)
    tbl$utilidad[i] <- tbl$precio[i] - (tbl$compra[i] + tbl$mano_obra[i] + tbl$transporte[i])
  }
  
  return(tbl)
}

simulacion_pto1 <- simulacion_brinkley(50000, alfa = 320, gama = 0.125)

perdidas <- nrow(simulacion_pto1[simulacion_pto1$utilidad <= 5, ])  

probabilidad_1 <- perdidas / 50000

### -------------------------------------------------------
### Punto dos
### -------------------------------------------------------

rm(list = ls())

tiro_moneda <- function() {
  lanzamientos <- 0
  caras <- 0
  cruces <- 0
  while(abs(caras - cruces) < 7) {
    lanzamientos <- lanzamientos + 1
    ifelse(sample.int(2, size = 1) == 1,
           caras <- caras + 1,
           cruces <- cruces + 1)
  }
  return(10 - lanzamientos)
}

varios <- replicate(n = 250000, expr = tiro_moneda())
mean(varios)
varios_1 <- replicate(n = 10000, tiro_moneda())
mean(varios_1)

### -------------------------------------------------------
### Punto tres
### -------------------------------------------------------

rm(list = ls())

demanda <- function(u) {
  ifelse(u < 0.24, 55,
         ifelse(u < 0.45, 70,
                ifelse(u < 0.6, 40,
                       ifelse(u < 0.75, 85,
                              ifelse(u < 0.85, 25,
                                     ifelse(u < 0.93, 10, 100))))))
}

simulacion_periodo <- function(n, q=50, compra=720, venta=1350,
                               salvamento=40) {
  tbl <- data.frame(matrix(ncol = 10, nrow = n))
  colnames(tbl) <- c("dia", "rand", "deman", "q_comp", "per_ven", "per_no_ven",
                     "costo_compr", "ingr_ven", "ingr_salv", "utilidad")
  for(i in 1:n) {
    tbl$dia[i] <- i
    tbl$rand[i] <- runif(1)
    tbl$deman[i] <- demanda(tbl$rand[i])
    tbl$q_comp[i] <- q
    tbl$per_ven[i] <- min(q, tbl$deman[i])
    tbl$per_no_ven[i] <- q - tbl$deman[i]
    tbl$costo_compr[i] <- q * compra
    tbl$ingr_ven[i] <- venta * tbl$per_ven[i]
    tbl$ingr_salv[i] <- salvamento * tbl$per_no_ven[i]
    tbl$utilidad[i] <- tbl$ingr_ven[i] + tbl$ingr_salv[i] - tbl$costo_compr[i]
  }
  return(mean(tbl$utilidad))
}

simulacion_periodo(n = 20000)
