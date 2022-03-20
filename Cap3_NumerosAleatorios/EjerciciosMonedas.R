# Ejercicios de la clase tres

## EJEMPLO UNO

fair_coin <- function() {
  x <- runif(n=1, min=0, max=1)
  result <- ifelse(x < 0.5, "head", "tail")
  print(result)
}

fair_coin()

##
##

## EJEMPLO DOS

unfair_coin <- function(p_head) {
  x <- runif(n=1, min=0, max=1)
  result <- ifelse(x < p_head, "head", "tail")
  print(result)
}

##
##

## EJEMPLO TRES

n_coins <- function(n, p_head) {
  if (n <= 0) stop("n no puede ser negativo")
  if (p_head > 1 | p_head < 0) stop("la probabilidad es incorrecta")
  x <- runif(n=n, min=0, max=1)
  result <- ifelse(x < p_head, "head", "tail")
  return(result)
}

##
##

## EJEMPLO CUATRO

replicate(n = 5, expr = n_coins(n = 8, p_head = 0.5))

##
##

## EJEMPLO CINCO

set.seed(20220316)
replicas = 250000
res = replicate(n = replicas,
                expr = n_coins(n = 10, p_head = 0.5))

contador.caras = function(x) {
  n.caras = sum(x == "head")
  ifelse(n.caras == 5, 1, 0)
}

resultados = apply(X = res,
                   MARGIN = 2,
                   FUN = contador.caras)

probabilidad.ej5 = sum(resultados)/replicas
paste("La probabilidad de que salgan igual número de caras y sellos es: ",
      probabilidad.ej5)
paste("Comparar el resultado anterior con el resultado real: 0.24609")
paste("El error relativo es del:",
      round(100*(abs(0.24609-probabilidad.ej5)/0.24609), 4),
      "%.")

##
##
##
##
##

# TAREA

# Se lanza una moneda justa diez veces. Encontrar la probabilidad de:

## Punto uno. Hay más caras que sellos.

set.seed(20220317)
replicas = 500000
lanzamientos = replicate(n = replicas,
                         expr = n_coins(n = 10, p_head = 0.5))

mas.caras = function(x) {
  n.caras = sum(x == "head")
  ifelse(n.caras > 5, 1, 0)
}

resultados.tarea1 = apply(X = lanzamientos,
                   MARGIN = 2,
                   FUN = mas.caras)


real.tarea1 = 193/512
probabilidad.ej5 = sum(resultados.tarea1)/replicas
paste("La probabilidad de que salgan más caras que sellos es: ",
      resultados.tarea1)
paste("Comparar el resultado anterior con el resultado real:",
      real.tarea1)
paste("El error relativo es del:",
      round(100*(abs(real.tarea1-probabilidad.ej5)/real.tarea1), 4),
      "%.")

##
##
##

## Punto dos. El iésimo lanzamiento coincide con el (11-i)-ésimo lanzamiento

set.seed(20220317)

simetria.lanzamientos = function(x) {
  x1 = x[1:5]
  x2 = rev(x[6:10])
  ifelse(identical(x1, x2), 1, 0)
}

resultados.tarea2 = apply(X = lanzamientos,
                          MARGIN = 2,
                          FUN = simetria.lanzamientos)


real.tarea2 = 1/32
resultado.tarea2 = sum(resultados.tarea2)/replicas
paste("La probabilidad de que el evento ocurra: ",
      resultado.tarea2)
paste("Comparar el resultado anterior con el resultado real:",
      real.tarea2)
paste("El error relativo es del:",
      round(100*(abs(real.tarea2-resultado.tarea2)/real.tarea2), 4),
      "%.")

##
##
##
##

## Punto tres. Se obtienen cuatro caras consecutivas

set.seed(20220317)

caras.consecutivas = function(x){
  secuencia = rle(x)
  ifelse(length(which(secuencia$values == "head" & secuencia$lengths > 3)),
         1, 0)
}

resultados.tarea3 = apply(X = lanzamientos,
                          MARGIN = 2,
                          FUN = caras.consecutivas)


real.tarea3 = 251/1024
resultado.tarea3 = sum(resultados.tarea3)/replicas
paste("La probabilidad de que el evento ocurra: ",
      resultado.tarea3)
paste("Comparar el resultado anterior con el resultado real:",
      real.tarea3)
paste("El error relativo es del:",
      round(100*(abs(real.tarea3-resultado.tarea3)/real.tarea3), 4),
      "%.")