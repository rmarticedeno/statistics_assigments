
ejercicio1 <- read.csv('../Data/ejercicio1.csv')
ejercicio2 <- read.csv('../Data/ejercicio2.csv')
ejercicio3 <- read.csv('../Data/ejercicio3.csv')

best_ecuation <- function(data){
  X <- sum(data$X)
  Y <- sum(data$Y)
  XY <- sum(data$X * data$Y)
  X2 <- sum(data$X * data$X)
  Y2 <- sum(data$Y * data$Y)
  
  SSxy <- XY - (X*Y/length(data$X))
  
  SSx <- X2 - (X*X)/length(data$Y)
  
  b1 <- SSxy/SSx
  
  b0 <- (Y - (b1 * X))/length(data$X)
  
  result <- function(x) {
    return(b0 + b1*x)
  }
  
  return(result)
}

#fit <- lm(Y ~ X, data=data)

#print(summary(fit))

#MC(ejercicio1)

## Section Ejercicio 1

jpeg("../Images/ejercicio1.jpeg")

plot(ejercicio1$X,ejercicio1$Y,
     xlab="x",
     ylab="f(x)",main = 'Gráfico de Dispersión con recta de mejor ajuste. (Ejercicio 1)')

fit1 <- lm(Y ~ X, data=ejercicio1)

abline(fit1)


dev.off()

ecuation1 <-best_ecuation(ejercicio1)

print(paste("El valor estimado de la función para x = 1 es: ",ecuation1(1)))

## Section Ejercicio 2

jpeg("../Images/ejercicio2.jpeg")

plot(ejercicio2$X,ejercicio2$Y,
     xlab="x",
     ylab="f(x)",main = 'Gráfico de Dispersión con recta de mejor ajuste. (Ejercicio 2)')

fit2 <- lm(Y ~ X, data=ejercicio2)

abline(fit2)

dev.off()

ecuation2 <-best_ecuation(ejercicio2)

print(paste("El valor estimado de la función para x = 0 es: ",ecuation2(0)))
print(paste("El valor estimado de la función para x = 2 es: ",ecuation2(2)))

## Section Ejercicio 3

jpeg("../Images/ejercicio3norecta.jpeg")

plot(ejercicio3$X,ejercicio3$Y,
     xlab="Velocidad (ft/s)",
     ylab="Rapidez de pasos",main = 'Gráfico de Dispersión. (Ejercicio 3)')

dev.off()

jpeg("../Images/ejercicio3.jpeg")

plot(ejercicio3$X,ejercicio3$Y,
     xlab="Velocidad (ft/s)",
     ylab="Rapidez de pasos",main = 'Gráfico de Dispersión con recta de mejor ajuste. (Ejercicio 3)')


fit3 <- lm(Y ~ X, data=ejercicio3)

abline(fit3)

dev.off()

ecuation3 <-best_ecuation(ejercicio3)

summary(fit3)

print(paste("El valor estimado de la función para una velocidad de 19 pasos por segundo es: ",ecuation3(19)))
