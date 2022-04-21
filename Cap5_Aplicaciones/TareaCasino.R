## Juego del casino

juego.casino <- function() {
  intento <- 1
  dinero <- 30
  apuesta <- 10
  disponible <- 30
  control <- TRUE
  
  moneda <- c("Cara", "Cruz")
  
  respuesta <- data.frame()
  
  while(control) {
    eleccion <- sample(moneda, size = 1)
    lanzamiento <- sample(moneda, size = 1)
    
    if (eleccion == lanzamiento) {
      ganar = "Sí"
      disponible <- dinero + apuesta
    
      }
    if (eleccion != lanzamiento) {
      ganar = "No"
      disponible <- dinero - apuesta
    }
    
    if(disponible > 0 & disponible < 50) {
      termina <- "No"
      juego <- data.frame(intento, dinero, apuesta, eleccion,
                          lanzamiento, ganar, disponible, termina)
      dinero <- disponible
      intento <- intento + 1
    }
    
    if (disponible <= 0 | disponible >= 50) {
      termina <- "Sí"
      juego <- data.frame(intento, dinero, apuesta, eleccion,
                          lanzamiento, ganar, disponible, termina)
      control <- FALSE
    }
    
    respuesta <- rbind(respuesta, juego)
    
    if (ganar == "No") {
      if (apuesta * 2 < disponible) {
        apuesta <- apuesta * 2
      }
      else {
        apuesta <- disponible
      }
    }
    
  }
  
  return(respuesta)
}

simulacion <- data.frame()

for (i in 1:10000) {
  juego <- juego.casino()
  indice <- rep.int(i, nrow(juego))
  simulacion <- rbind(simulacion, cbind(indice, juego))
}

library(magrittr)

final <- simulacion %>% 
  dplyr::filter(termina == "Sí")


final$disponible %>% table()

final.50 <- final %>% 
  dplyr::filter(disponible == 50)


final.50$intento %>% table() %>% prop.table()


mean(final.50$intento)
