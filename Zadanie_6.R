library(tree)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)

iris <- read.table(file = 'D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_6/iris.txt',sep = ",", header = TRUE)
attach(iris)
table(iris$class)
qplot(petal.width, sepal.width, data=iris, colour=class, size=I(4))
s<-sample(150,100)
iris_train<-iris[s,]
iris_test<-iris[-s,]
dtm <- rpart(class ~ ., data=iris_train, method="class",)
dtm
fancyRpartPlot(dtm,main='Iris')
p <- table(predict(dtm,iris_test,type="class"), iris_test$class)
p
nok<-sum(p)-sum(diag(p))
ok <- ((nrow(iris_test)-nok)/nrow(iris_test))*100 
nok
ok
