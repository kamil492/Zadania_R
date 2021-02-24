library(ggfortify)
dane<-read.table(file = 'D:/MECHATRONIKA/DATA MINING/indywidualne/Zadanie_5/gala_data.txt', header = TRUE)
linearMod<-lm(Species ~ Area +Elevation +Nearest +Scruz+ Adjacent, data=dane)
summary(linearMod)
autoplot(linearMod)
#widzimy iz przy wykresie Residuals vs Fitted values mamy doczynienia z pewnym trendem po lewej stronie wykresu (linia powinna byc pozioma), widac tam duze wartosci residuów
#na wykresie Scale - Location widzimy iz wariancje rosna wraz z wartoscia dopasowanej zmiennej (linia powinna byc pozioma-zblizona do zera)
#zalozenie o liniowosci i stalej wariancji nie jest spelnione (wiec wariancja residuow zalezy od wartosci prognozowanych )
#Na wykresie Q-Q widzimy iz sa 3 punkty odstajace
dane_2<-dane
l=nrow(dane_2)
for (i in 1:l){dane_2$Species[i]<-sqrt(dane$Species[i])}
linearMod_2<-lm(Species ~ Area +Elevation +Nearest +Scruz+ Adjacent, data=dane_2)
summary(linearMod_2)
autoplot(linearMod_2)
#widzimy iz przy wykresie Residuals vs Fitted values,ze duze wartosci residuów zmala³y (z rzedu setek do rzedu jednosci)
#na wykresie Scale - Location widzimy iz wariancje najpierw maleja, a nastepnie rosna wraz z wartoscia dopasowanej zmiennej (linia powinna byc pozioma-zblizona do zera), inaczej niz w pierwszym modelu
#Na wykresie Q-Q widzimy iz sa 3 punkty odstajace
#model z pominieciem Nearest
linearMod_3<-lm(Species ~ Area +Elevation +Scruz+ Adjacent, data=dane_2)
summary(linearMod_3)
#dla modelu 1 (bez jakichkolwiek zmian) wsp R wynosi ~0.77, a R adj wynosi ~0.72
#dla modelu 3 (ze zmieniona objasniana i pominieciu zmniennej Nearest) wsp R wynosi ~0.78, a R adj wynosi ~0.75, wiec model ulegl polepszeniu
