demanda <- function(u) {
  ifelse(u < 0.10, 1,
         ifelse(u < 0.5, 2,
                ifelse(u < 0.8, 3, 4)))
}

inventario <- function(u) {
  ifelse(u < 0.2, 2,
         ifelse(u < 0.8, 3, 4))
}

simulacion_inventario <- function(n, Q, R) {
  tbl <- data.frame(matrix(NA, nrow = (1.02*n), ncol = 10))
  colnames(tbl) <- c("semana", "unds_recib", "inv_inicial", "dem_aleat",
                       "demanda", "inv_final", "ventas_perdidas", "pedido",
                       "ped_aleat", "t_entrega")
  # Llenado de valores criticos en toda la tbl
  tbl$semana <- c(rep("X", 0.02*n), seq(1, n))
  tbl$unds_recib <- rep(0, 1.02*n)
  tbl$dem_aleat <- runif(1.02*n)
  tbl$ped_aleat <- runif(1.02*n)
  
  
  # Llenado de la primera fila
  tbl$inv_inicial[1] <- 7
  tbl$demanda[1] <- demanda(tbl$dem_aleat[1])
  tbl$inv_final[1] <- ifelse(tbl$inv_inicial[1] > tbl$demanda[1],
                             tbl$inv_inicial[1] - tbl$demanda[1], 0)
  tbl$ventas_perdidas[1] <- ifelse(tbl$inv_inicial[1] < tbl$demanda[1],
                                  tbl$demanda[1] - tbl$inv_inicial[1], 0)
  tbl$pedido[1] <- ifelse(tbl$inv_final[1] < (R+1), "Si", "No")
  aux1 <- inventario(tbl$ped_aleat[1])
  tbl$t_entrega[1] <- ifelse(tbl$pedido[1] == "Si",
                             aux1, "---")
  tbl$unds_recib[1 + aux1] <- ifelse(tbl$pedido[1] == "Si", 10, 0)
  
  # Para el resto de filas
  for (i in 2:nrow(tbl)) {
    tbl$inv_inicial[i] <- tbl$inv_final[i - 1] + tbl$unds_recib[i]
    tbl$demanda[i] <- demanda(tbl$dem_aleat[i])
    tbl$inv_final[i] <- ifelse(tbl$inv_inicial[i] > tbl$demanda[i],
                               tbl$inv_inicial[i] - tbl$demanda[i], 0)
    tbl$ventas_perdidas[i] <- ifelse(tbl$inv_inicial[i] < tbl$demanda[i],
                                     tbl$demanda[i] - tbl$inv_inicial[i], 0)
    tbl$pedido[i] <- ifelse(tbl$t_entrega[i-1] > 1, "No",
                            ifelse(tbl$inv_final[i] < (R+1), "Si", "No"))
    tbl$t_entrega[i] <- ifelse(tbl$pedido[i] == "Si",
                               inventario(tbl$ped_aleat),
                               (tbl$t_entrega[i-1] - 1))
    if ((i+aux1) <= nrow(tbl)) {
      tbl$unds_recib[i + aux1] <- ifelse(tbl$pedido[i] == "Si", 10, 0)
    }
  }
  
  return(tbl[1:(1.02*n), ])
}

costo_semanal <- function(tbl) {
  inv_promedio <- mean(tbl$inv_inicial)
  vent_perd_promedio <- mean(tbl$ventas_perdidas)
  return(5 * inv_promedio + vent_perd_promedio)
}

costo1 <- costo_semanal(simulacion_inventario(1000, 10, 5))
costo2 <- costo_semanal(simulacion_inventario(1000, 15, 10))