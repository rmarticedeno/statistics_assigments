library(lmtest)
library(rpart)

data <- read.csv('../data/anorectic.csv', header = TRUE ,sep = ',')

M <- cor(data)

png(filename='../Images/boxplot.png')
pairs(data)
dev.off()

symnum(M)

acp <- prcomp(data, scale=TRUE)

summary(acp)

png(filename='../Images/Pplot.png')
plot(acp)
dev.off()

biplot(acp)

acp$rotation

data.std <- scale(data)

d <- dist(data.std, method='euclidean')

fit <- hclust(d, method = 'complete')

d2 <- as.dendrogram(fit)


png(filename='../Images/Cluster1.png')
plot(fit)
rect.hclust(fit, k=5, border='red')
dev.off()

png(filename='../Images/Cluster2.png')
plot(fit)
rect.hclust(fit, k=6, border='red')
dev.off()

#kmeans

#este es el que empleamos
data.k1 <- kmeans(data.std, 6)

data.k1

#son de prueba de aca hacia abajo

data.k2 <- kmeans(data.std, 2)

data.k2

data.k4 <- kmeans(data.std, 4)

data.k4

data.k12 <- kmeans(data.std, 12)

data.k12



data.k5 <- kmeans(data.std, 5)

data.k5

plot(data.std, col = data.k1$cluster)

# decision tree

l <- length(data[,1])

sub <- sample(1:l, 2*l/3)

diag.tree <- rpart(diag~. ,data = data[sub,], cp=0.01, maxdepth=3)

summary(diag.tree)

png(filename='../Images/Destition_Tree1.png')
plot(diag.tree)
text(diag.tree, use.n = TRUE, pretty=1, xpd=TRUE)
dev.off()

plotcp(diag.tree)
printcp(diag.tree)

diag.pred <- predict(diag.tree, newdata = data[-sub,], type="vector")

tb <- table(diag.pred, data[-sub,]$diag )

error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart