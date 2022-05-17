# Análisis de resultados

library(tidyverse)

datos <- read.csv('resultados.txt',
                  sep = "",
                  dec = ".")

head(datos)
tail(datos)
class(datos)
colnames(datos) <- c("tamano", "lambda", "repeticiones", "asimetria",
                     "curtosis", "estadistico", "valor_p")

datos <- datos %>% 
  mutate(val_crit =  round(0.886/sqrt(tamano), 4)) %>% 
  mutate(rechazo = ifelse(estadistico < val_crit, "Rechazo", "Normal"))

# Rechazo con el estadístico calculado

fun1 <- function(x) 0.886/sqrt(x)

win.graph()
plot(x = datos$tamano, y = datos$estadistico)
plot(fun1, 30, 1000, col = 'red', add = TRUE)

# Rechazo con el valor p

win.graph()
plot(x = datos$tamano, y = datos$valor_p)
abline(a = 0.05, b = 0, col = 'red')

save(datos, file = 'datos.RData')
