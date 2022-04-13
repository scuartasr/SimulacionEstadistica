### ############################
### PARCIAL UNO DE SIMULACIÓN
### SIMÓN CUARTAS RENDÓN

### ----------------------------
### Punto uno
### ----------------------------

secuencia.aleatoria <- function(x1, x2, n) {
  resultado <- c(x1, x2)
  x_i1 <- x2
  x_i2 <- x1
  x_i <- (3*x_i1 + 6*x_i2) %% 100
  for (i in 3:n) {
    resultado <- c(resultado, x_i)
    x_i1 <- resultado[length(resultado)]
    x_i2 <- resultado[length(resultado)-1]
    x_i <- (3*x_i1 + 6*x_i2) %% 100
  }
  return(resultado)
}

secuencia <- secuencia.aleatoria(10, 25, 143)
secuencia[143]

### ----------------------------
### Punto dos
### ----------------------------

pandequesos.cientos <- function(x) {
  ((1/2) ^ (x + 1)) + 0.5*(2^(x-1))/(3^x)
}

defectuosos <- function(y) {
  exp(y) / (exp(1) - 1)
}

total.defectuosos <- function(x, y) {
  pandequesos.cientos(x) * defectuosos(y) + 0.6
}

### ----------------------------
### Punto tres
### ----------------------------

n_coins <- function(n, p_head) {
  if (n <= 0) stop("n no puede ser negativo")
  if (p_head > 1 | p_head < 0) stop("la probabilidad es incorrecta")
  x <- runif(n=n, min=0, max=1)
  result <- ifelse(x < p_head, 1, 0)
  return(result)
}

lanzamientos <- replicate(n = 500000, expr = n_coins(n = 24, p_head = 0.8))

lanzamientos <- as.data.frame(t(lanzamientos))

# for (i in 50000) {
#   lanzamientos[i, 26] <- sum(lanzamientos[i,])
# }

lanzamientos$total <- rowSums(lanzamientos)
lanzamientos$sello <- 24 - lanzamientos$total
mean(lanzamientos$total) - mean(lanzamientos$sello)

### ----------------------------
### Punto cuatro
### ----------------------------


vonNeumann <- function(n, semilla) {
  paste("La semilla a usar es: ", semilla)
  resultado <- vector()
  resultado <- c(resultado, semilla)
  for (i in 1:(n-1)) {
    semilla <-  semilla ^ 2             # Se eleva al cuadrado
    #semilla <- as.character(semilla)
    if (nchar(semilla) < 10) {          # Si no tiene diez dígitos...
      semilla <- sprintf("%010d", semilla) # ... se los completa
    }
    semilla <- as.numeric(substr(semilla, start = 3, stop = 7)) # 5 dís central
    resultado <- c(resultado, semilla)
  }
  return(resultado)
}

von.neumann <- vonNeumann(10, 3842)
von.neumann

### ----------------------------
### Punto cinco
### ----------------------------

m1 <- 1000
m2 <- 1000
m3 <- 500
m4 <- 500
m5 <- 200
m6 <- 200
m7 <- 200
m8 <- 100
m9 <- 100

monedas <- data.frame(m1, m2, m3, m4, m5, m6, m7, m8, m9)
monedas[rep(1, 5)]
monedas <- do.call("rbind", replicate(100000, monedas, simplify = FALSE))
opciones <- 1:9

for (i in 1:100000) {
  monedas[i, sample.int(9, 1)] <- 0
}

monedas$total <- rowSums(monedas)

monedas$chequeo <- ifelse(monedas$total >= 3500, 1, 0)

respuesta4 <- sum(monedas$chequeo) / length(monedas$chequeo)

### ----------------------------
### Punto seis
### ----------------------------

n <- 1e6
x_min <- 3
x_max <- 11
# Autos vendidos ---------------------------------------------------------------
constante <- sum((x_min:x_max)^2 + 4) # Inverso de la constante
pdf_autos <- function(x) (x^2 + 4) / constante
# Para chequar si la pdf si es una pdf
sum(pdf_autos(x_min:x_max))
# Simulando los autos
x <- sample(x=x_min:x_max, size=n, replace=TRUE, prob=pdf_autos(x_min:x_max))
# Verificando si la m.a. tiene la distribucion de interes
plot(x=x_min:x_max, y=prop.table(table(x)), type='b', bty='l', lwd=6,
     xlab='X', ylab='Proportion', las=1, col='dodgerblue3', ylim=c(0, 1))
lines(x=x_min:x_max, y=pdf_autos(x=x_min:x_max), type='b', lwd=2, col='firebrick3')
legend('topleft', c('Simulated','Theoretical'), lwd=c(6, 2), bty='n',
       col=c('dodgerblue3','firebrick3'), lty=1)
# Precio de cada auto ----------------------------------------------------------
# Simulador del precio
pdf_precio <- function(x) 2 * x * exp(-x^2)
# Densidad de la funcion de interes y de una distribucion auxiliar
curve(pdf_precio, from=0, to=10, lwd=2, col=2)
curve(dgamma(x, shape=2.2, scale=0.5),
      from=0, to=10, lwd=2, col=4, add=TRUE)
# Usando aceptacion rechazo
h <- function(x) pdf_precio(x) / dgamma(x, shape=2.2, scale=0.5)
k <- optimize(f=h, interval=c(0, 10), maximum=TRUE)$objective
simul_precio <- function(n) {
  flag <- 0
  x <- numeric(n)
  while (flag < n) {
    y <- rgamma(n=1, shape=2.2, scale=0.5) # Value from g(x)
    u <- runif(1) # Value to comparing
    if (u < pdf_precio(y) / (k * dgamma(x=y, shape=2.2, scale=0.5))) {
      flag <- flag + 1
      x[flag] <- y
    }
  }
  x
}
# Simulando los precios
y <- simul_precio(n=n)
curve(pdf_precio, from=0, to=5, lwd=6, col=2, main='Aceptacion rechazo')
lines(density(y), col='blue', lwd=2)
legend('topright', c('Simulated','Theoretical'), bty='n',
       col=c('blue','red'), lty=1)
# Calculando el ingreso bruto
ingreso <- x * y
mean(ingreso)

### ----------------------------
### Punto siete
### ----------------------------

c1 <- 8; c2 <- 6; c3 <- 4; c4 <- 6; c5 <- 4

x1 <- c(sample.int(c1, 1), sample.int(c2, 1), sample.int(c3, 1),
        sample.int(c4, 1), sample.int(c5, 1))
x2 <- c(sample.int(c1, 1), sample.int(c2, 1), sample.int(c3, 1),
        sample.int(c4, 1), sample.int(c5, 1))
lanzamiento.dados <- as.data.frame(rbind(x1, x2))

for (i in 1:99998) {
  lanzamiento.dados <- rbind(lanzamiento.dados, c(sample.int(c1, 1), 
                                                  sample.int(c2, 1), 
                                                   sample.int(c3, 1),
                                                   sample.int(c4, 1), 
                                                   sample.int(c5, 1)))
}

lanzamiento.dados$total <- rowSums(lanzamiento.dados)

### ----------------------------
### Punto ocho
### ----------------------------

# The eggs produced -------------------------------------
f1 <- function(x, param) -(1-param)^x / (x * log(param))
gen_f1 <- function(u, param) { # Using Inverse transform method
  p <- -(1-param) / log(param)
  F <- p
  i <- 1
  while (u >= F) {
    i <- i + 1
    p <- f1(x=i, param=param)
    F <- F + p
  }
  return(i)
}
gen_f1 <- Vectorize(gen_f1)
                    
##
##

# The percentage of eggs ($Y$) in poor condition --------
f <- function(x, a, b) a * b * x^(a-1) * (1-x)^(b-1)
area <- integrate(f, lower=0, upper=1, a=a, b=b)$value
f2 <- function(x, a, b) a * b * x^(a-1) * (1-x)^(b-1) / area
# ------------- Inverse transform method
# calculates the cdf by numerical integration
F <- function(x, a, b) integrate(f=f2, lower=0, upper=x, a=a, b=b)$value
F <- Vectorize(F)
# Inverting the cdf
F.inv <- function(y, a, b)
  uniroot(function(x) {F(x, a=a, b=b) - y}, interval=c(0, 1))$root
F.inv <- Vectorize(F.inv)
gen1 <- function(n, a, b) {
  x <- F.inv(runif(n), a, b)
  return(x)
}
# ------------- Acceptance rejection method
h <- function(x, a, b) f2(x, a, b) / dbeta(x, shape1=a, shape2=b)
constant <- optimize(f=h, interval=c(0, 1), a=a, b=b, maximum=TRUE)$objective
gen2 <- function(n, a, b) {
  flag <- 0
  attempt <- 0
  x <- numeric(n)
  while (flag < n) {
    u1 <- rbeta(n=1, shape1=a, shape2=b) # Value from g(x)
    u2 <- runif(1) # Value to comparing
    if (u2 < f2(u1, a, b) / (constant * dbeta(u1, shape1=a, shape2=b)) ) {
      flag <- flag + 1
      x[flag] <- u1
    }
    attempt <- attempt + 1
  }
  if (n==1) list(x=x, attempt=attempt) # attempt is useful when n=1
  else x
}

p <- 0.3
a <- 3
b <- 4
n <- 10000
x <- gen_f1(runif(n), param=p)
y <- gen2(n=n, a, b)
z <- x * y
mean(z)