
head(iris)

names(iris)


x = iris[,-5]
y = iris$Species

kc <- kmeans(x,3)


kc

table(y,kc$cluster)


plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)


