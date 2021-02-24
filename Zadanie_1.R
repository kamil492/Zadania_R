#Zadanie_1

library(MASS)
data(Cars93)
head(Cars93)
#Sprawdzenie typu zmiennych 
lapply(Cars93["Min.Price"] , class)
lapply(Cars93["MPG.city"] , class)
lapply(Cars93["MPG.highway"] , class)
lapply(Cars93["Weight"] , class)
lapply(Cars93["Origin"] , class)
lapply(Cars93["Type"] , class)

write.csv(Cars93, 'Cars93_2.csv')
Cars93_2 <- Cars93
row=nrow(Cars93_2)

#obliczenie interesujacych zmiennych
paliwomiasto=c()
for (i in 1:row) { 
  paliwomiasto[i]<-(3.8*100)/(1.6*Cars93$MPG.city[i])
}
paliwoautostrada=c()
for (i in 1:row) { 
  paliwoautostrada[i]<-(3.8*100)/(1.6*Cars93$MPG.highway[i])
}
wagaKG=c()
for (i in 1:row) { 
  wagaKG[i]<- Cars93$Weight[i]*0.4536
}
cenaPLN=c()
for (i in 1:row) { 
  cenaPLN[i]<- Cars93$Min.Price[i]*3.35
}

Cars93_2$paliwomiasto <- paliwomiasto
Cars93_2$paliwoautostrada <- paliwoautostrada
Cars93_2$wagaKG <- wagaKG
Cars93_2$cenaPLN <- cenaPLN

#kwantyl .95 i podstawowe statystyki probkowe
summary(Cars93_2$Min.Price)
kw95<-quantile(Cars93_2$Min.Price,probs=c(0.95)) 
kw95
modele=Cars93_2[which(Cars93_2$Min.Price>(quantile(Cars93_2$Min.Price, probs = 0.95))),]
modele

#wykresy
Type<-table(Cars93_2$Type)
barplot(Type)
pie(Type)
#ile spotowych
sportowe<-Type["Sporty"]
sportowe
pochodzenie<-split(Cars93_2, Cars93_2$Origin)
boxplot(pochodzenie$USA$paliwomiasto, pochodzenie$USA$paliwoautostrada ,names =c("Paliwo_Miasto","Paliwo_Autostrada"),main = 'amerykañskie',ylab="l/100km")
boxplot(pochodzenie$`non-USA`$paliwomiasto, pochodzenie$`non-USA`$paliwoautostrada ,names =c("Paliwo_Miasto","Paliwo_Autostrada"),main='nieamerykañskie',ylab="l/100km")

par(mfrow=c(1,2))
plot(Cars93_2$cenaPLN, Cars93_2$paliwomiasto,xlab="Cena", ylab="Paliwo_Miasto")
plot(Cars93_2$paliwomiasto, Cars93_2$paliwoautostrada,xlab="Paliwo_Autostrada", ylab="Paliwo_Miasto")
par(mfrow=c(1,1))

hist(Cars93_2$wagaKG, main="Histogram wagi" , xlab="Waga w kg",ylab="Czêstoœæ")

