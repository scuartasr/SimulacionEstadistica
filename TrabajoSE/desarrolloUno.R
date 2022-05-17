library(tidyverse)
library(fastDummies)


load('datos.RData')

head(datos)
tail(datos)

#### Para todos los datos

datos <- datos %>% 
  dummy_cols(select_columns = 'rechazo') %>% 
  select(-c(rechazo))

resumen <- datos %>% 
  group_by(tamano, lambda) %>% 
  summarise(aceptaciones = sum(rechazo_Normal),
            rechazos = sum(rechazo_Rechazo),
            asimetria = median(asimetria),
            curtosis = median(curtosis))

win.graph()
plot(x = resumen$tamano, y = resumen$asimetria)

win.graph()
plot(x = resumen$tamano, y = resumen$curtosis)
abline(a = 3, b = 0, col = 'red')
  