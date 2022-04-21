# Juego de la escalera

# Casillas donde comienza o termina una escalera o serpiente
inicios <- c(1, 4, 9, 21, 28, 36, 51, 71, 80,
             98, 95, 93, 87, 64, 62, 56, 49, 47)
# Casillas donde comienza o termina una escalera o serpiente
finales <- c(38, 14, 31, 42, 84, 44, 67, 91, 100,
             78, 75, 73, 24, 60, 19, 53, 11, 26)


# Juegi
juego.escalera <- function() {
  posicion <- 0
  ruta <- c()
  
  base <- data.frame() #XXXX
  
  while(posicion < 100) {
    dado <- sample.int(n = 6, size = 1)
    
    # ¿Puede moverse dentro de una de las cien casillas?
    if (posicion + dado <= 100) {
      posicion <- posicion + dado
      
      # ¿El movimiento lo llevó a una escalera o a una serpiente?
      if (posicion %in% inicios) {
        indice <- which(inicios == posicion)
        posicion <- finales[indice]
      }
    }
    ruta <- append(ruta, posicion)
    # base <- rbind(base, c(dado, posicion))
  }
  retorno <- list(length(ruta), ruta)
  names(retorno) <- c("Pasos", "Ruta")
  return(retorno)
  #return(base) 
}

juego.escalera()
