library(lmtest)
library(corrplot)

ejercicio1 <- read.csv2('../data/ejercicio1.csv', header = TRUE ,sep = ',')
ejercicio2 <- read.csv('../data/Advertising.csv', header = TRUE)

#Ejercicio 1


M <- cor(ejercicio1)
corrplot(M,method = 'number')

png(filename='../Images/Dispersion1.png')
pairs(ejercicio1)
dev.off()

e1 <- lm(Ventas~Gastos, data=ejercicio1)
summary(e1)

e2 <- lm(Ventas~Gastos+Mes, data=ejercicio1)
summary(e2)

mean(e1$residuals)
sum(e1$residuals)

mean(e2$residuals)
sum(e2$residuals)

png(filename='../Images/qqplot1.png')
qqnorm(e1$residuals)
qqline(e1$residuals)
dev.off()

png(filename='../Images/qqplot2.png')
qqnorm(e2$residuals)
qqline(e2$residuals)
dev.off()

png(filename='../Images/hist1.png')
hist(e1$residuals)
dev.off()

png(filename='../Images/hist2.png')
hist(e2$residuals)
dev.off()

dwtest(e1)
dwtest(e2)

png(filename='../Images/standard1.png')
plot(ejercicio1$Ventas, rstandard(e1), ylab = "Residuos estandarizados", xlab = "Ventas")
abline(h =0, lty = 2)
dev.off()

png(filename='../Images/standard2.png')
plot(ejercicio1$Ventas, rstandard(e2), ylab = "Residuos estandarizados", xlab = "Ventas")
abline(h =0, lty = 2)
dev.off()

#Ejercicio 2

M2 <- cor(ejercicio2)
corrplot(M2,method = 'number')

png(filename='../Images/Dispersion2.png')
pairs(ejercicio2)
dev.off()

e3 <- lm(sales~TV+radio+newspaper, data=ejercicio2)
summary(e3)

mean(e3$residuals)
sum(e3$residuals)

dwtest(e3)

png(filename='../Images/qqplot3.png')
qqnorm(e3$residuals)
qqline(e3$residuals)
dev.off()

png(filename='../Images/hist3.png')
hist(e3$residuals)
dev.off()

png(filename='../Images/standard3.png')
plot(ejercicio2$sales, rstandard(e3), ylab = "Residuos estandarizados", xlab = "sales")
abline(h =0, lty = 2)
dev.off()

e4 <- lm(sales~TV+radio, data=ejercicio2)
summary(e4)

mean(e4$residuals)
sum(e4$residuals)

dwtest(e4)

png(filename='../Images/qqplot4.png')
qqnorm(e4$residuals)
qqline(e4$residuals)
dev.off()

png(filename='../Images/hist4.png')
hist(e4$residuals)
dev.off()

png(filename='../Images/standard4.png')
plot(ejercicio2$sales, rstandard(e4), ylab = "Residuos estandarizados", xlab = "sales")
abline(h =0, lty = 2)
dev.off()

e5 <- lm(sales~TV, data=ejercicio2)
summary(e5)

mean(e5$residuals)
sum(e5$residuals)

dwtest(e5)

png(filename='../Images/qqplot5.png')
qqnorm(e5$residuals)
qqline(e5$residuals)
dev.off()

png(filename='../Images/hist5.png')
hist(e5$residuals)
dev.off()

png(filename='../Images/standard5.png')
plot(ejercicio2$sales, rstandard(e5), ylab = "Residuos estandarizados", xlab = "sales")
abline(h =0, lty = 2)
dev.off()