library(car)
library(MASS)
library(ggplot2)
library(reshape2)
library(corrplot)
#zaladowanie danych
dane <- read.table(file = "D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_3/savings.txt", header=TRUE)
summary(dane)
linearMod<-lm(Savings ~ dpi +ddpi +Pop15 +Pop75, data=dane)
#sprawdzenie korelacji
dane_set <- dane[,-1]
dane_cor <- cor(dane_set)
for (i in 1:nrow(dane_cor)){
  correlations <-  which((dane_cor[i,] > 0.85) & (dane_cor[i,] != 1))

  if(length(correlations)> 0){
    print(colnames(dane_set)[i])
    print(correlations)
  }
}
corrplot(dane_cor, method = "ellipse")
# z tabeli dane_cor wynika, iz najbardziej skorelowany z savings jest Pop15
plot(linearMod$residuals, main='wykres reszt',ylab = "Reszty", xlab = "Kraj")
abline (h=0,col="red")
summary (linearMod)
dane$Country[which(linearMod$residuals == min(linearMod$residuals))] #Chile
dane$Country[which(linearMod$residuals == max(linearMod$residuals))] #Zambia
plot(linearMod)
plot(rstudent(linearMod),main="reszty studentyzowane")  
abline(h=2,col='black')
abline(h=-2,col='black')
x <- dane$Country[which(abs(rstudent(linearMod))> 2.33)]
x
n <- nrow(dane)
k <- length(linearMod$coefficients)-1
cv <- 2*sqrt(k/n)
#DFFITS
dane$Country[which(abs(dffits(linearMod))> (cv))]
#DFBETAS
dane$Country[which(abs(dfbeta(linearMod)[,2])> (2/sqrt(n)))]
dane$Country[which(abs(dfbeta(linearMod)[,3])> (2/sqrt(n)))]
dane$Country[which(abs(dfbeta(linearMod)[,4])> (2/sqrt(n)))]
dane$Country[which(abs(dfbeta(linearMod)[,5])> (2/sqrt(n)))]
#odleglosc Cookea
dane$Country[which(cooks.distance(linearMod)>1 )] #dla >1
dane$Country[which(cooks.distance(linearMod)>4/(n-k-1) )] #dla >4/(n-k-1)
dane$Country[which(cooks.distance(linearMod)>4/n )] #dla >4/n
#duza odleglosc Cookea jest dla Japonii Zambii i Libii, wplywowa moze byc tez Irlandia(DFBETAS)
dane$Country[which(cooks.distance(linearMod) == max(cooks.distance(linearMod)))] 
#najwieksza odlegosc Cookea jest dla Libii
#model regresji z wylaczeniem Libii
dane_2 <- dane[-49,]
linearMod_2<-lm(Savings ~ dpi +ddpi +Pop15 +Pop75, data=dane_2)
summary(linearMod)  
summary(linearMod_2)
#usuwajac obserwacje 49 uzyskujemy lepszy model - blad standardowy sie zmniejszyl oraz wzrosl wsp determinacji
#odczytuje z tabeli dane_cor wielkosc korelacji pomiedzy savings a dpi - jest to 0.22 wskazuje to na slaba korelacje
#jednak dpi posiada silna korelacje z Pop15 i Pop75

