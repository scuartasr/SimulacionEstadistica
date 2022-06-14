## ____________________________________________________________________________
############################ ESTUDIO DE SIMULACIÓN ############################
## ____________________________________________________________________________

# _____________________________________________________________________________
# 1. Alistamiento =============================================================
# _____________________________________________________________________________

# Limpieza de la memoria

rm(list = ls())

# Cargado de paquetes importantes

paquetes <- c('tidyverse', 'plotly', 'magrittr')
lapply(paquetes, library, character.only = TRUE)

# Cargado de los datos

## Primera base de datos

direccion1 <- './Datos/resultados3.txt'
tlc1 <- read.csv(direccion1, sep = '', dec = '.', header = FALSE)

## 1.1. Revisión de los datos =================================================

dim(tlc1)
head(tlc1)
tail(tlc1)

# Nótese que la base de datos no tiene bien los nombres, por lo que se procede
# a corregirlos.

colnames(tlc1) <- c('n', 'lambda', 'rep', 'asim', 'curt',
                    'estcalc', 'vp')
head(tlc1)

# La base de datos está bien leída.

# _____________________________________________________________________________
# 2. Rechazo de la hipótesis nula =============================================
# _____________________________________________________________________________

# Rechazo según el estadístico de prueba.
# Recordar que para tamaños de muestra mayores a 30, y con una significancia de
# 0.5, se rechaza si el estadístico de prueba es mayor a 0.875897/sqrt(n)

tlc1 <- tlc1 %>% 
  mutate(norm = ifelse(vp < 0.05, 0, 1))
# _____________________________________________________________________________
# 3.. Revisiones ==============================================================
# _____________________________________________________________________________

# Se va a calcular la media truncada para la curtosis y el coeficiente de asi-
# metría, así como la tasa de aprobación de la hipótesis de normalidad (nula)
# para cada par de tamaño de muestra y media poblacional.

tlc1_sum <- tlc1 %>% 
  group_by(n, lambda) %>% 
  summarise(normalidad = sum(norm)/100,
            curtosis = mean(curt, trim = 0.05),
            asimetria = mean(asim, trim = 0.05))

# _____________________________________________________________________________
# 4. Gráficos =================================================================
# _____________________________________________________________________________

## 1.1. Gráficos de proporción de aceptación de normalidad ====================

fig1 <- plot_ly(z = ~xtabs(normalidad ~ n + lambda,
                           data = tlc1_sum)) %>%
  add_surface()
fig1 <- fig1 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig1 <- fig1 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig1

##

fig2 <- plot_ly(tlc1_sum, x = ~n, y = ~lambda, z = ~normalidad,
                color = ~normalidad,
                colors = c('#BF382A', '#0C4B8E'))
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig2 <- fig2 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig2

## 1.2. Gráficos para la curtosis =============================================

fig3 <- plot_ly(tlc1_sum, x = ~n, y = ~lambda, z = ~curtosis)
fig3 <- fig3 %>% add_markers()
fig3 <- fig3 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig3 <- fig3 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig3

# XXXX ====

# Cargado de los datos

## Primera base de datos

direccion2 <- './Datos/resultados4.txt'
tlc2 <- read.csv(direccion2, sep = '', dec = '.', header = FALSE)

## 1.1. Revisión de los datos =================================================

dim(tlc2)
head(tlc2)
tail(tlc2)

# Nótese que la base de datos no tiene bien los nombres, por lo que se procede
# a corregirlos.

colnames(tlc2) <- c('n', 'lambda', 'rep', 'asim', 'curt',
                    'estcalc', 'vp')
head(tlc2)

# La base de datos está bien leída.

# _____________________________________________________________________________
# 2. Rechazo de la hipótesis nula =============================================
# _____________________________________________________________________________

# Rechazo según el estadístico de prueba.
# Recordar que para tamaños de muestra mayores a 30, y con una significancia de
# 0.5, se rechaza si el estadístico de prueba es mayor a 0.875897/sqrt(n)

tlc2 <- tlc2 %>% 
  mutate(norm = ifelse(vp < 0.05, 0, 1))
# _____________________________________________________________________________
# 3.. Revisiones ==============================================================
# _____________________________________________________________________________

# Se va a calcular la media truncada para la curtosis y el coeficiente de asi-
# metría, así como la tasa de aprobación de la hipótesis de normalidad (nula)
# para cada par de tamaño de muestra y media poblacional.

tlc2_sum <- tlc2 %>% 
  group_by(n, lambda) %>% 
  summarise(normalidad = sum(norm)/100,
            curtosis = mean(curt, trim = 0.05),
            asimetria = mean(asim, trim = 0.05))

# _____________________________________________________________________________
# 4. Gráficos =================================================================
# _____________________________________________________________________________

## 1.1. Gráficos de proporción de aceptación de normalidad ====================

fig4 <- plot_ly(z = ~xtabs(normalidad ~ n + lambda,
                           data = tlc2_sum)) %>%
  add_surface()
fig4 <- fig4 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig4 <- fig4 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig4

##

fig5 <- plot_ly(tlc2_sum, x = ~n, y = ~lambda, z = ~normalidad,
                color = ~normalidad,
                colors = c('#BF382A', '#0C4B8E'))
fig5 <- fig5 %>% add_markers()
fig5 <- fig5 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig5 <- fig5 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig5

## 1.2. Gráficos para la curtosis =============================================

fig6 <- plot_ly(tlc2_sum, x = ~n, y = ~lambda, z = ~curtosis)
fig6 <- fig6 %>% add_markers()
fig6 <- fig6 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig6 <- fig6 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig6

# XXXX ====

# Cargado de los datos

## Primera base de datos

direccion3 <- './Datos/resultados5.txt'
tlc3 <- read.csv(direccion3, sep = '', dec = '.', header = FALSE)

## 1.1. Revisión de los datos =================================================

dim(tlc3)
head(tlc3)
tail(tlc3)

# Nótese que la base de datos no tiene bien los nombres, por lo que se procede
# a corregirlos.

colnames(tlc3) <- c('n', 'lambda', 'rep', 'asim', 'curt',
                    'estcalc', 'vp')
head(tlc3)

# La base de datos está bien leída.

# _____________________________________________________________________________
# 2. Rechazo de la hipótesis nula =============================================
# _____________________________________________________________________________

# Rechazo según el estadístico de prueba.
# Recordar que para tamaños de muestra mayores a 30, y con una significancia de
# 0.5, se rechaza si el estadístico de prueba es mayor a 0.875897/sqrt(n)

tlc3 <- tlc3 %>% 
  mutate(norm = ifelse(vp < 0.05, 0, 1))
# _____________________________________________________________________________
# 3.. Revisiones ==============================================================
# _____________________________________________________________________________

# Se va a calcular la media truncada para la curtosis y el coeficiente de asi-
# metría, así como la tasa de aprobación de la hipótesis de normalidad (nula)
# para cada par de tamaño de muestra y media poblacional.

tlc3_sum <- tlc3 %>% 
  group_by(n, lambda) %>% 
  summarise(normalidad = sum(norm)/100,
            curtosis = mean(curt, trim = 0.05),
            asimetria = mean(asim, trim = 0.05))

# _____________________________________________________________________________
# 4. Gráficos =================================================================
# _____________________________________________________________________________

## 1.1. Gráficos de proporción de aceptación de normalidad ====================

fig7 <- plot_ly(z = ~xtabs(normalidad ~ n + lambda,
                           data = tlc3_sum)) %>%
  add_surface()
fig7 <- fig7 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig7 <- fig7 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig7

##

fig8 <- plot_ly(tlc3_sum, x = ~n, y = ~lambda, z = ~normalidad,
                color = ~normalidad,
                colors = c('#BF382A', '#0C4B8E'))
fig8 <- fig8 %>% add_markers()
fig8 <- fig8 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig8 <- fig8 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig8

## 1.2. Gráficos para la curtosis =============================================

fig9 <- plot_ly(tlc9_sum, x = ~n, y = ~lambda, z = ~curtosis)
fig9 <- fig9 %>% add_markers()
fig9 <- fig9 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig9 <- fig9 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig9
