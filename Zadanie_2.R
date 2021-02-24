#zaladowanie danych
air_poluttion <- read.table(file = "D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_2/airpollution.txt", header=TRUE)
summary(air_poluttion)
plot(air_poluttion$NOx, air_poluttion$Mortality,xlab="NOx",ylab ="Mortality",  main="Mortality (NOx)")
#sprawdzenie korelacji
cor(air_poluttion$NOx, air_poluttion$Mortality)
#daleko od 1, wiec ten model jest slaby
linearMod <-   lm (Mortality ~   NOx, data= air_poluttion)
summary (linearMod)
linearMod
#wsp nachylenia prostej regresji to -0,1043
abline(linearMod)

#logNOx
plot(log(air_poluttion$NOx), air_poluttion$Mortality,xlab="log(NOx)",ylab ="Mortality", main="Mortality (log(NOx))")
#sprawdzenie korelacji 
cor(log(air_poluttion$NOx), air_poluttion$Mortality)
#nieco lepszy wynik niz dla pierwszego przypadku
linearModlog<-lm(Mortality ~log(NOx), data= air_poluttion)
summary(linearModlog)
linearModlog
#wsp nachylenia prostej regresji to 15.1
abline(linearModlog)


plot(rstudent(linearModlog), main="Residua studentyzowane")  
abline(h=2, col="blue")
abline(h=-2, col="blue")
wplywoweplus <- as.numeric(names(rstudent(linearModlog))[(rstudent(linearModlog) < (-2))]) 
wplywoweminus <- as.numeric(names(rstudent(linearModlog))[(rstudent(linearModlog) > (2))])  
wplywowe<-c(wplywoweplus,wplywoweminus)
air_poluttion_2 <- air_poluttion[-wplywowe,]

#sprawdzenie korelacji Pearsona 
plot(log(air_poluttion_2$NOx), air_poluttion_2$Mortality,xlab="log(NOx)",ylab ="Mortality", main="Mortality (log(NOx)) z pominieciem pkt")
cor(log(air_poluttion_2$NOx), air_poluttion_2$Mortality)
#lepiej niz w poprzednim przypadku 
linearModlog_2<-lm(Mortality ~log(NOx),data= air_poluttion_2)
summary(linearModlog_2)
linearModlog_2
#wsp nachylenia prostej regresji to 27.24
abline(linearModlog_2)
