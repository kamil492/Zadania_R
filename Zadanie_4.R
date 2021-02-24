dane <- read.table(file = 'D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_4/realest.txt', header = TRUE)
#Model regresji ze wszystkimi zmiennymi objasjniajacymi
linearMod<-lm(Price ~ Bedroom+Space+Room+Lot+Tax+Bathroom+Garage+Condition, data=dane)
summary(linearMod)
#dodanie 1 do liczby sypialni
dane_2<-dane
syp<-dane[, 2] + 1
dane_2$Bedroom<-syp
#Model regresji ze wszystkimi zmiennymi objasjniajacymi i liczba sypialni zwiekszona o 1
linearMod_2<-lm(Price ~ Bedroom+Space+Room+Lot+Tax+Bathroom+Garage+Condition, data=dane_2)
summary(linearMod_2)
#Model regresji zalezny jedynie od sypialni
linearMod_3<-lm(Price ~ Bedroom, data=dane)
summary(linearMod_3)
x3<-summary(linearMod_3)
#Przewidywanie dla okreslonych wartosci
dom <- data.frame(Bedroom=3,Space=1500,Room=8, Lot=40,Tax=1000, Bathroom=5, Garage=1, Condition=0)
predict(linearMod, newdata = dom, interval = "confidence")
