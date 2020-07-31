
library(lmtest)

data <- read.csv('../data/spray.csv', header = TRUE)

brand <- c(rep('S1',6), rep('S2',6), rep('S3',6))

reply <- c(data$S1,data$S2,data$S3)

#reply <- c(72, 65, 67, 75, 62, 73, 55, 59, 68, 70, 53, 50, 64, 74, 61, 58, 51, 69)

df <- data.frame(brand, reply)

png(filename='../Images/boxplot.png')
plot(reply~brand, data=df)
dev.off()

brand.anova <- aov(reply~brand, data=df)
summary(brand.anova)

res <- brand.anova$residuals

plot(res)

layout(matrix(c(1,2,3,4),2,2,byrow = T))

#layout(matrix(c(1,2,3),3,1,byrow = T))

png(filename='../Images/sup.png')
layout(matrix(c(1,2,3,4),2,2,byrow = T))
plot(brand.anova$fitted.values, rstudent(brand.anova),
     main="Anova Studentized Residuals",
     xlab="Predictions",ylab= "Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)

hist(res, main="Histograma de Residuos")
qqnorm(res)
qqline(res)
dev.off()

shapiro.test(res)

bartlett.test(res, df$brand)

dwtest(brand.anova)

