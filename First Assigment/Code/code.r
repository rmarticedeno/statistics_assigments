
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
    return(b0 - b1*x)
  }
  
  return(result)
}

#fit <- lm(Y ~ X, data=data)

#print(summary(fit))

#MC(ejercicio1)

plot(ejercicio1$X,ejercicio1$Y,
     xlab="x",
     ylab="f(x)",main = 'Gráfico de Dispersión')

ecuation1 <-best_ecuation(ejercicio1)

print(ecuation1(1))

