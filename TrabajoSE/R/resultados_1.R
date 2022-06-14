# ==============================================================================
# ==============================================================================
# RESULTADOS PRELIMINARES DE LA SIMULACIÓN REALIZADA
# ==============================================================================
# ==============================================================================

# ==============================================================================
# Paquetes
# ==============================================================================

library(tidyverse)
library(plotly)

# ==============================================================================
# Base de datos
# ==============================================================================

# >>> Apertura de la base de datos
direccion <- "./simulaciones.txt"
datos <- read.csv(direccion,
                  sep = "",
                  dec = ".",
                  encoding = "UTF-8")

# =====================================
# >>> Revisión de la base de datos

dim(datos)                             # 377,233 columnas y siete variables
colnames(datos) <- c("n", "lambda", "repeticiones", "asimetria",
                     "curtosis", "est_prueba", "p_valor")
colnames(datos)
head(datos)
tail(datos)

# =====================================
# >>> Eliminación de observaciones basura

datos <- datos %>% 
  filter(repeticiones != 200) %>% 
  select(-c(repeticiones))

# >>> Rechazo del estadístico calculado


# Función que demarca el punto de aprobación usando el estadístico de prueba
fun1 <- function(x) 0.886/sqrt(x)

# Gráfica de aprobación y rechazo usando el estadístico de prueba
win.graph()
plot(x = datos$n, y = datos$est_prueba)
plot(fun1, 30, 1000, col = 'red', add = TRUE)

# Gráfica de aprobación y rechazo usando el valor p
win.graph()
plot(x = datos$n, y = datos$p_valor)
abline(a = 0.05, b = 0, col = 'red')

# Proporción de aprobación
datos$normal <- ifelse(datos$est_prueba > 0.886/sqrt(datos$n), 1, 0)

aprobacion <- datos %>% 
  group_by(n, lambda) %>% 
  summarise(tasa_aprobacion = round(sum(normal) * 2))

# Tomando algunos valores de lambda para poder hacer comparaciones
aprobacion$lambda %>% unique()

lambdas <- matrix(unique(datos$lambda), nrow = 15, ncol = 5, byrow = TRUE)
lambdas <- as.vector(lambdas[, 5])

aprobacionx <- aprobacion %>% 
  filter(lambda %in% lambdas)

fig1 <- plot_ly(aprobacion, x = ~n, y = ~lambda, z = ~tasa_aprobacion,
               color = ~tasa_aprobacion,
               colors = c('#BF382A', '#0C4B8E'))
fig1 <- fig1 %>% add_markers()
fig1 <- fig1 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig1 <- fig1 %>% layout(legend=list(title=list(text='<b>Tasa de aprobación</b>')))
fig1

# Otra forma de presentar la gráfica

title <- 'aaaa'

fig2 <- plot_ly(z = ~xtabs(tasa_aprobacion ~ n + lambda,
                           data = aprobacion)) %>%
            add_surface()
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Tamaño de muestra'),
                                     yaxis = list(title = 'λ'),
                                     zaxis = list(title = 'Tasa de aprobación de H<sub>0</sub>')))
fig2 <- fig2 %>%
  layout(legend = list(title = list(text = paste('<b>', title, '</b>'))))
fig2
fig2 %>% layout(title = "Custom Hover Text", 
                legend = list(title = list(text = "<b>Cylinders</b>"))) # TITLE HERE
