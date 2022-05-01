# Simulación de una línea de espera con un solo servidor

simulacion_mm1 <- function(n, clientes_hora = 10, tiempo_corte = 4) {
  atencion <- 60 / tiempo_corte
  tbl <- data.frame(matrix(ncol = 10, nrow = n))
  colnames(tbl) <- c("cliente", "llegada_a", "t_llegada", "llegada_acum",
                     "servicio_a", "t_servicio", "inicio", "fin", "espera",
                     "t_sistema")
  tbl$cliente <- seq(1, n)
  tbl$llegada_a <- runif(n)
  tbl$servicio_a <- runif(n)
  
  # Primera fila
  
  tbl$t_llegada[1] <- -log(tbl$llegada_a[1]) * 60 / clientes_hora
  tbl$llegada_acum[1] <- tbl$t_llegada[1]
  tbl$t_servicio[1] <- -log(tbl$servicio_a[1]) * 60 / atencion
  tbl$inicio[1] <- tbl$t_llegada[1]
  tbl$fin[1] <- tbl$inicio[1] + tbl$t_servicio[1]
  tbl$espera[1] <- 0
  tbl$t_sistema[1] <- tbl$fin[1] - tbl$llegada_acum[1]
  
  # Resto de filas
  
  for (i in 2:n) {
    tbl$t_llegada[i] <- -log(tbl$llegada_a[i]) * 60 / clientes_hora
    tbl$llegada_acum[i] <- tbl$t_llegada[i] + tbl$llegada_acum[i-1]
    tbl$t_servicio[i] <- -log(tbl$servicio_a[i]) * 60 / atencion
    tbl$inicio[i] <- max(tbl$llegada_acum[i], tbl$fin[i-1])
    tbl$fin[i] <- tbl$inicio[i] + tbl$t_servicio[i]
    tbl$espera[i] <- ifelse(tbl$llegada_acum[i] - tbl$fin[i-1] < 0,
                            tbl$fin[i-1] - tbl$llegada_acum[i], 0)
    tbl$t_sistema[i] <- tbl$t_servicio[i] + tbl$espera[i]
  }
  
  total_espera <- sum(tbl$espera)
  media_espera <- mean(tbl$espera)
  total_sistema <- sum(tbl$t_sistema)
  media_sistema <- mean(tbl$t_sistema)
  
  retorno <- list(total_espera, total_sistema, media_espera, media_sistema,
                  tbl)
  names(retorno) <- c("total_espera", "total_sistema", "media_espera",
                      "media_sistema", "tbl")
  return(retorno)
}

simulacion <- simulacion_mm1(1000)


