 # odds.ratio() wylicza ilorazy szans, przedzialy ufnosci i testuje H0:\theta=1

odds.ratio <-
  function(x, pad.zeros=FALSE, conf.level=0.95) {
    if (pad.zeros) {
      if (any(x==0)) x <- x + 0.5
    }
    theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
    ASE <- sqrt(sum(1/x))
    CI <- exp(log(theta)
              + c(-1,1) * qnorm(0.5*(1+conf.level)) *ASE )
    p.value <- pnorm(abs(log(theta)/ASE), lower.tail=FALSE)
    list(estimator=theta,
         p.value=p.value,
         conf.interval=CI,
         conf.level=conf.level)
  }



#Przygotowanie danych do analizy
#Wczytanie danych
titanic<-read.table("titanic.txt", sep="\t", header=T)
attach(titanic)
titanic<-data.frame(PClass, Age, Sex, Survived)

#Usuniêcie brakuj¹cych obserwacji
titanic2<-na.omit(titanic)
attach(titanic2)
titanic2<-data.frame(PClass, Age, Sex, Survived)



#dyskretyzacja zmiennej Age: 
wiek<-cut(Age,breaks=c(0,20,50,Inf))
wiek<-factor(wiek, ordered=T)


#tworzenie tabeli z nowymi kolumnami
titanic.table<-data.frame(PClass, Sex, wiek, Survived)


print("Zbadamy zale¿noœci pomiêdzy zmienn¹ Survived oraz pozosta³ymi zmiennymi")
print("Tablica kontyngencji Survived~PClass")
ocaleni<-xtabs(Survived~PClass, data=titanic.table)
martwi<-xtabs(1-Survived~PClass, data=titanic.table)
tab.sur.pclass<-cbind(ocaleni, martwi)
frakcja<-tab.sur.pclass[,1]/(tab.sur.pclass[,1]+tab.sur.pclass[,2])
tab.sur.pclass<-cbind(ocaleni,martwi,frakcja)
#frakcja ocala³ych
(tab.sur.pclass[1,1]+tab.sur.pclass[2,1]+tab.sur.pclass[3,1])/(tab.sur.pclass[1,1]+tab.sur.pclass[2,1]+tab.sur.pclass[3,1]+tab.sur.pclass[1,2]+tab.sur.pclass[2,2]+tab.sur.pclass[3,2])
print(tab.sur.pclass)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")


print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Tablica kontyngencji Survived~Sex.")
ocaleni<-xtabs(Survived~Sex, data=titanic.table)
martwi<-xtabs(1-Survived~Sex, data=titanic.table)
tab.sur.sex<-cbind(ocaleni,martwi)
frakcja<-tab.sur.sex[,1]/(tab.sur.sex[,1]+tab.sur.sex[,2])
tab.sur.sex<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.sex)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Tablica kontyngencji Survived~wiek.")
ocaleni<-xtabs(Survived~wiek, data=titanic.table)
martwi<-xtabs(1-Survived~wiek, data=titanic.table)
tab.sur.wiek<-cbind(ocaleni,martwi)
frakcja<-tab.sur.wiek[,1]/(tab.sur.wiek[,1]+tab.sur.wiek[,2])
tab.sur.wiek<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.wiek)

readline("Proszê nacisnaæ dowolny klawisz, aby kontynuowaæ.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Analiza modeli GLM")
print("Model logitowy 1")
PClass<-factor(PClass, levels=c("3rd","2nd","1st"), ordered=T)
model.logit1<-glm(Survived~Sex+wiek+PClass, family=binomial(link=logit))
print(summary(model.logit1))
print("W modelu mamy zmiennej nieistotne. Chcemy udoskonaliæ model, poprzez usuniêcie wspó³czynników nieistotnych.")
print("Najpierw zobaczymy jak prezentuje siê model po usuniêciu zmiennych")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model bez zmiennej wiek")
model.logit1.bezwiek<-glm(Survived~Sex+PClass, family=binomial(link=logit))
print(summary(model.logit1.bezwiek))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print(anova(model.logit1.bezwiek, model.logit1, test="Chisq"))
print("Test ilorazu wiarogodnoœci odrzuca hipotezê, ¿e model zawarty w modelu pe³nym jest lepszy.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model bez zmiennej PClass")
model.logit1.bezPClass<-glm(Survived~Sex+wiek, family=binomial(link=logit))
print(summary(model.logit1.bezPClass))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print(anova(model.logit1.bezPClass,model.logit1, test="Chisq"))
print("Test ilorazu wiarogodnoœci odrzuca hipotezê, ¿e model zawarty w modelu pe³nym jest lepszy.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model jedynie ze zmienn¹ Sex")
model.logit1.Sex<-glm(Survived~Sex, family=binomial(link=logit))
print(summary(model.logit1.Sex))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print(anova(model.logit1.Sex, model.logit1, test="Chisq"))
print("Test ilorazu wiarogodnoœci odrzuca hipotezê, ¿e model zawarty w modelu pe³nym jest lepszy.")

readline("Proszê nacisnaæ dowolny klawisz, aby kontynuowaæ.")

#Wprowadzenie nowej zmiennej wiek1
wiek1<-cut(Age,breaks=c(0,20,Inf))
wiek1<-factor(wiek1, ordered=T)
titanic.table1<-data.frame(PClass, Sex, wiek1, Survived)

#Badanie niezale¿noœci w tablicy kontyngencji i za pomoc¹ testów

print("Tablica kontyngencji Survived~wiek1.")
ocaleni<-xtabs(Survived~wiek1, data=titanic.table1)
martwi<-xtabs(1-Survived~wiek1, data=titanic.table1)
tab.sur.wiek1<-cbind(ocaleni,martwi)
frakcja<-tab.sur.wiek1[,1]/(tab.sur.wiek1[,1]+tab.sur.wiek1[,2])
tab.sur.wiek1<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.wiek1)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model logitowy 2")
model.logit2<-glm(Survived~Sex+wiek1+PClass, family=binomial(link=logit))
print(summary(model.logit2))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Nadal obserwujemy nieistotnoœæ wspó³czynnika przy poziomie wartoœci zmiennej PClass dla drugiej klasy pasa¿erów.")
print("Stworzymy now¹ zmienn¹: klasa, która po³¹czy poziom 1st oraz 2nd zmiennej PClass.")

klasa<-ifelse(PClass=="3rd","gorsza", "lepsza")
klasa<-factor(klasa, levels=c("gorsza", "lepsza"), ordered=T)
titanic.table2<-data.frame(klasa,Sex,wiek1,Survived)

#Tablica kontyngencji i niezale¿noœæ

print("Tablica kontyngencji Survived~klasa.")

ocaleni<-xtabs(Survived~klasa, data=titanic.table2)
martwi<-xtabs(1-Survived~klasa, data=titanic.table2)
tab.sur.klasa<-cbind(ocaleni,martwi)
frakcja<-tab.sur.klasa[,1]/(tab.sur.klasa[,1]+tab.sur.klasa[,2])
tab.sur.klasa<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.klasa)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model logitowy 3")
model.logit3<-glm(Survived~Sex+wiek1+klasa, family=binomial(link=logit))
print(summary(model.logit3))
print("Model ten nie zawiera wspó³czynników nieistotnych wg testu istotnoœci Walda.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model zerowy.")
model0<-glm(Survived~1, family=binomial(link=logit))
print(summary(model0))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print(anova(model0, model.logit3, test="Chisq"))
print("Test ilorazu wiarogodnosci odrzuca hipotezê, ¿e model zerowy jest lepszy od modelu logitowego 3.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Model probitowy")
model.probit<-glm(Survived~Sex+wiek1+klasa, family=binomial(link=probit))
print(summary(model.probit))

readline("Naciœnij dowolny klawisz, aby kontynuowaæ.")

#Tworzymy tabelê licznoœci

licznosc<-expand.grid(Sex=levels(Sex),wiek1=levels(wiek1),klasa=levels(klasa))

licz<-ftable(Sex, wiek1, klasa, Survived)
alive<-licz[,1]
nonalive<-licz[,2]
frakcja=alive/(alive+nonalive)
licznosc$alive<-alive
licznosc$nonalive<-nonalive
licznosc$frakcja<-frakcja

#Dodanie do tabeli licznoœci wyestymowanych prawdopodobienstw
#Prawdopodobienstwa z modelu logitowego
print("Tabela licznoœci")
pred.logit<-predict(model.logit3,type="response", se=T)
wektor.logitowy<-c(0.67207609,0.13713633,0.50232297,0.07258928,0.91160317,0.44435580,0.83549324,0.28255956)
pred.probit<-predict(model.probit, type="response", se=T)
wektor.probit<-c(0.67877489,0.14599318,0.52994154,0.07452053,0.91231791,0.43530556,0.83298160,0.29046216)
print("Tabela podsumowuj¹ca prawdopodobienstwo wyestymowane oraz frakcje w ka¿dej z grup.")
licznosc$logit<-wektor.logitowy
licznosc$probit<-wektor.probit
print(licznosc)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

#Wykresy 
logse<-c(0.04915215,0.02783301,0.04896369,0.01427623,0.02111659,0.05695230,0.02474250,0.02717375)
probitse<-c(0.04683627,0.03000616,0.04608689,0.01568114,0.02311447,0.05346733,0.02497647,0.02706920)

print("Wykres dla modelu logitowego.")
x<-c(1:8)
plot(frakcja, col="blue", main="Wykres otrzymanego wyniku dla grup", pch=19, cex=1.3, xlim=c(1,8),ylim=c(0.0,1.25),xlab="Numer grupy",ylab="Prawdopodobienstwo prze¿ycia")
points(x,licznosc$logit, col="red", pch=19, cex=1.3)
lines(x,licznosc$logit, col="red", lty=1, lwd=2)
lines(x,licznosc$logit+1.96*logse, col="green", lty=2, lwd=2)
lines(x,licznosc$logit-1.96*logse, col="green", lty=2, lwd=2)
legend(x=0.5,y=1.2,legend=c("prognozy z modelu logitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=1, text.width=0.5, adj=0)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Wykres dla modelu probitowego.")

x<-c(1:8)
plot(frakcja, col="blue", main="Wykres otrzymanego wyniku dla grup", pch=19, cex=1.3, xlim=c(1,8),ylim=c(0.0,1.25),xlab="Numer grupy",ylab="Prawdopodobienstwo prze¿ycia")
points(x,licznosc$probit, col="red", pch=19, cex=1.3)
lines(x,licznosc$probit, col="red", lty=1, lwd=2)
lines(x,licznosc$probit+1.96*probitse, col="green", lty=2, lwd=2)
lines(x,licznosc$probit-1.96*probitse, col="green", lty=2, lwd=2)
legend(x=0.5,y=1.2,legend=c("prognozy z modelu probitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=1, text.width=0.5, adj=0)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Analiza jakoœci dopasowania modelu logitowego.")

par(mfrow=c(2,2))
x=c(1,2)
plot(x, licznosc$frakcja[c(2,6)], col="blue", main="Pr prze¿. mê¿czyzn poni¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,0.65),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(2,6)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(2,6)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(2,6)]+1.96*logse[c(2,6)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(2,6)]-1.96*logse[c(2,6)], col="green", lty=2, lwd=2)
legend(x=1,y=0.65,legend=c("prognozy z modelu logitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(4,8)], col="blue", main="Pr prze¿. mê¿czyzn powy¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,1.1),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(4,8)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(4,8)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(4,8)]+1.96*logse[c(4,8)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(4,8)]-1.96*logse[c(4,8)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu logitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(1,5)], col="blue", main="Pr prze¿. kobiet poni¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1.2),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(1,5)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(1,5)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(1,5)]+1.96*logse[c(1,5)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(1,5)]-1.96*logse[c(1,5)], col="green", lty=2, lwd=2)
legend(x=1,y=1.2,legend=c("prognozy z modelu logitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(3,7)], col="blue", main="Pr prze¿. kobiet powy¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(3,7)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(3,7)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(3,7)]+1.96*logse[c(3,7)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(3,7)]-1.96*logse[c(3,7)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu logitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Analiza jakoœci dopasowania modelu probitowego.")
x=c(1,2)
plot(x, licznosc$frakcja[c(2,6)], col="blue", main="Pr prze¿. mê¿czyzn poni¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,0.65),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(2,6)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(2,6)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(2,6)]+1.96*probitse[c(2,6)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(2,6)]-1.96*probitse[c(2,6)], col="green", lty=2, lwd=2)
legend(x=1,y=0.65,legend=c("prognozy z modelu probitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(4,8)], col="blue", main="Pr prze¿. mê¿czyzn powy¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,1.1),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(4,8)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(4,8)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(4,8)]+1.96*probitse[c(4,8)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(4,8)]-1.96*probitse[c(4,8)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu probitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(1,5)], col="blue", main="Pr prze¿. kobiet poni¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1.2),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(1,5)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(1,5)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(1,5)]+1.96*probitse[c(1,5)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(1,5)]-1.96*probitse[c(1,5)], col="green", lty=2, lwd=2)
legend(x=1,y=1.2,legend=c("prognozy z modelu probitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(3,7)], col="blue", main="Pr prze¿. kobiet powy¿ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1),xlab="Klasa podró¿y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(3,7)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(3,7)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(3,7)]+1.96*probitse[c(3,7)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(3,7)]-1.96*probitse[c(3,7)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu probitowego","frakcje ocala³ych","przedzia³ ufnoœci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Dopasowanie modelu po usuniêciu zmiennej Age dla danych pierwotnych.")

titanic1<-read.table("titanic.txt", sep="\t", header=T)
attach(titanic1)


print("Tablica kontyngencji Survived~Sex.")
ocaleni<-xtabs(Survived~Sex, data=titanic1)
martwi<-xtabs(1-Survived~Sex, data=titanic1)
tab.sur.sex2<-cbind(ocaleni,martwi)
frakcja<-tab.sur.sex2[,1]/(tab.sur.sex2[,1]+tab.sur.sex2[,2])
tab.sur.sex2<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.sex2)

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Badanie niezale¿noœci zmiennej Survived i Sex")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))
print("Oba testy jednoznacznie odrzucaj¹ hipotezê o niezale¿noœci zmiennych. Iloraz szans równie¿ jest znacz¹co wiêkszy od 1.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Tablica kontyngencji Survived~PClass")
ocaleni<-xtabs(Survived~PClass, data=titanic1)
martwi<-xtabs(1-Survived~PClass, data=titanic1)
tab.sur.pclass1<-cbind(ocaleni, martwi)
frakcja<-tab.sur.pclass1[,1]/(tab.sur.pclass1[,1]+tab.sur.pclass1[,2])
tab.sur.pclass1<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.pclass1)
 
readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Badanie niezale¿noœci zmniennej Survived oraz PClass.")

print("Dok³adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))
print("Oba testy jednoznacznie odrzucaj¹ hipotezê o niezale¿noœci zmiennych.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

titanic1$PClass<-factor(titanic1$PClass, levels=c("3rd","2nd","1st"), ordered=T)

print("Model logitowy.")
modelus<-glm(titanic1$Survived~titanic1$PClass+titanic1$Sex, family=binomial(link=logit))
print(summary(modelus))
print("W modelu widzimy wspó³czynnik nieistotny dla klasy œredniej zmiennej PClass.")
print(" Kryterium Akaike jest znacznie wy¿sze od wybranego modelu. Œwiadczy to, ¿e wybrany model jest lepszy.")

readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")

print("Koniec.")