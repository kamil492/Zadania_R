library(datasets)
library(class)
library(ggplot2)
library(GGally)
library(gmodels)

iris_full <- read.table(file = 'D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_7/iris.txt',sep=",", header = TRUE)
iris_mix <- iris_full[sample(nrow(iris_full)),]
head(iris_mix)
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
iris.norm<- data.frame(lapply(iris_mix[,c(1,2,3,4)],normalize))
iris_train<- iris.norm[1:100,]
iris_test<- iris.norm[101:150,]
iris_train1<- iris_mix[1:100,5]
iris_test1<- iris_mix[101:150,5]
summary(iris.norm)
iris_pred<- knn(train=iris_train, test=iris_test, cl=iris_train1, k=3)
iris_pred_2<- knn(train=iris_train, test=iris_test, cl=iris_train1, k=5)
par(mfrow=c(1,2))
plot(iris_pred, main='dla k =3', ylab='iloœæ klasyfikacji')
plot(iris_pred_2, main='dla k =5',ylab='iloœæ klasyfikacji')

p<-table(iris_test1,iris_pred)
p
nok<-sum(p)-sum(diag(p))
ok <- ((nrow(iris_test)-nok)/nrow(iris_test))*100 
nok
ok
