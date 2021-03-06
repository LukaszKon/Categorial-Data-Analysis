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

#Usuni�cie brakuj�cych obserwacji
titanic2<-na.omit(titanic)
attach(titanic2)
titanic2<-data.frame(PClass, Age, Sex, Survived)



#dyskretyzacja zmiennej Age: 
wiek<-cut(Age,breaks=c(0,20,50,Inf))
wiek<-factor(wiek, ordered=T)


#tworzenie tabeli z nowymi kolumnami
titanic.table<-data.frame(PClass, Sex, wiek, Survived)


print("Zbadamy zale�no�ci pomi�dzy zmienn� Survived oraz pozosta�ymi zmiennymi")
print("Tablica kontyngencji Survived~PClass")
ocaleni<-xtabs(Survived~PClass, data=titanic.table)
martwi<-xtabs(1-Survived~PClass, data=titanic.table)
tab.sur.pclass<-cbind(ocaleni, martwi)
frakcja<-tab.sur.pclass[,1]/(tab.sur.pclass[,1]+tab.sur.pclass[,2])
tab.sur.pclass<-cbind(ocaleni,martwi,frakcja)
#frakcja ocala�ych
(tab.sur.pclass[1,1]+tab.sur.pclass[2,1]+tab.sur.pclass[3,1])/(tab.sur.pclass[1,1]+tab.sur.pclass[2,1]+tab.sur.pclass[3,1]+tab.sur.pclass[1,2]+tab.sur.pclass[2,2]+tab.sur.pclass[3,2])
print(tab.sur.pclass)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")


print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Tablica kontyngencji Survived~Sex.")
ocaleni<-xtabs(Survived~Sex, data=titanic.table)
martwi<-xtabs(1-Survived~Sex, data=titanic.table)
tab.sur.sex<-cbind(ocaleni,martwi)
frakcja<-tab.sur.sex[,1]/(tab.sur.sex[,1]+tab.sur.sex[,2])
tab.sur.sex<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.sex)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Tablica kontyngencji Survived~wiek.")
ocaleni<-xtabs(Survived~wiek, data=titanic.table)
martwi<-xtabs(1-Survived~wiek, data=titanic.table)
tab.sur.wiek<-cbind(ocaleni,martwi)
frakcja<-tab.sur.wiek[,1]/(tab.sur.wiek[,1]+tab.sur.wiek[,2])
tab.sur.wiek<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.wiek)

readline("Prosz� nacisna� dowolny klawisz, aby kontynuowa�.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Analiza modeli GLM")
print("Model logitowy 1")
PClass<-factor(PClass, levels=c("3rd","2nd","1st"), ordered=T)
model.logit1<-glm(Survived~Sex+wiek+PClass, family=binomial(link=logit))
print(summary(model.logit1))
print("W modelu mamy zmiennej nieistotne. Chcemy udoskonali� model, poprzez usuni�cie wsp�czynnik�w nieistotnych.")
print("Najpierw zobaczymy jak prezentuje si� model po usuni�ciu zmiennych")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model bez zmiennej wiek")
model.logit1.bezwiek<-glm(Survived~Sex+PClass, family=binomial(link=logit))
print(summary(model.logit1.bezwiek))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print(anova(model.logit1.bezwiek, model.logit1, test="Chisq"))
print("Test ilorazu wiarogodno�ci odrzuca hipotez�, �e model zawarty w modelu pe�nym jest lepszy.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model bez zmiennej PClass")
model.logit1.bezPClass<-glm(Survived~Sex+wiek, family=binomial(link=logit))
print(summary(model.logit1.bezPClass))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print(anova(model.logit1.bezPClass,model.logit1, test="Chisq"))
print("Test ilorazu wiarogodno�ci odrzuca hipotez�, �e model zawarty w modelu pe�nym jest lepszy.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model jedynie ze zmienn� Sex")
model.logit1.Sex<-glm(Survived~Sex, family=binomial(link=logit))
print(summary(model.logit1.Sex))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print(anova(model.logit1.Sex, model.logit1, test="Chisq"))
print("Test ilorazu wiarogodno�ci odrzuca hipotez�, �e model zawarty w modelu pe�nym jest lepszy.")

readline("Prosz� nacisna� dowolny klawisz, aby kontynuowa�.")

#Wprowadzenie nowej zmiennej wiek1
wiek1<-cut(Age,breaks=c(0,20,Inf))
wiek1<-factor(wiek1, ordered=T)
titanic.table1<-data.frame(PClass, Sex, wiek1, Survived)

#Badanie niezale�no�ci w tablicy kontyngencji i za pomoc� test�w

print("Tablica kontyngencji Survived~wiek1.")
ocaleni<-xtabs(Survived~wiek1, data=titanic.table1)
martwi<-xtabs(1-Survived~wiek1, data=titanic.table1)
tab.sur.wiek1<-cbind(ocaleni,martwi)
frakcja<-tab.sur.wiek1[,1]/(tab.sur.wiek1[,1]+tab.sur.wiek1[,2])
tab.sur.wiek1<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.wiek1)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model logitowy 2")
model.logit2<-glm(Survived~Sex+wiek1+PClass, family=binomial(link=logit))
print(summary(model.logit2))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Nadal obserwujemy nieistotno�� wsp�czynnika przy poziomie warto�ci zmiennej PClass dla drugiej klasy pasa�er�w.")
print("Stworzymy now� zmienn�: klasa, kt�ra po��czy poziom 1st oraz 2nd zmiennej PClass.")

klasa<-ifelse(PClass=="3rd","gorsza", "lepsza")
klasa<-factor(klasa, levels=c("gorsza", "lepsza"), ordered=T)
titanic.table2<-data.frame(klasa,Sex,wiek1,Survived)

#Tablica kontyngencji i niezale�no��

print("Tablica kontyngencji Survived~klasa.")

ocaleni<-xtabs(Survived~klasa, data=titanic.table2)
martwi<-xtabs(1-Survived~klasa, data=titanic.table2)
tab.sur.klasa<-cbind(ocaleni,martwi)
frakcja<-tab.sur.klasa[,1]/(tab.sur.klasa[,1]+tab.sur.klasa[,2])
tab.sur.klasa<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.klasa)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model logitowy 3")
model.logit3<-glm(Survived~Sex+wiek1+klasa, family=binomial(link=logit))
print(summary(model.logit3))
print("Model ten nie zawiera wsp�czynnik�w nieistotnych wg testu istotno�ci Walda.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model zerowy.")
model0<-glm(Survived~1, family=binomial(link=logit))
print(summary(model0))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print(anova(model0, model.logit3, test="Chisq"))
print("Test ilorazu wiarogodnosci odrzuca hipotez�, �e model zerowy jest lepszy od modelu logitowego 3.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Model probitowy")
model.probit<-glm(Survived~Sex+wiek1+klasa, family=binomial(link=probit))
print(summary(model.probit))

readline("Naci�nij dowolny klawisz, aby kontynuowa�.")

#Tworzymy tabel� liczno�ci

licznosc<-expand.grid(Sex=levels(Sex),wiek1=levels(wiek1),klasa=levels(klasa))

licz<-ftable(Sex, wiek1, klasa, Survived)
alive<-licz[,1]
nonalive<-licz[,2]
frakcja=alive/(alive+nonalive)
licznosc$alive<-alive
licznosc$nonalive<-nonalive
licznosc$frakcja<-frakcja

#Dodanie do tabeli liczno�ci wyestymowanych prawdopodobienstw
#Prawdopodobienstwa z modelu logitowego
print("Tabela liczno�ci")
pred.logit<-predict(model.logit3,type="response", se=T)
wektor.logitowy<-c(0.67207609,0.13713633,0.50232297,0.07258928,0.91160317,0.44435580,0.83549324,0.28255956)
pred.probit<-predict(model.probit, type="response", se=T)
wektor.probit<-c(0.67877489,0.14599318,0.52994154,0.07452053,0.91231791,0.43530556,0.83298160,0.29046216)
print("Tabela podsumowuj�ca prawdopodobienstwo wyestymowane oraz frakcje w ka�dej z grup.")
licznosc$logit<-wektor.logitowy
licznosc$probit<-wektor.probit
print(licznosc)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

#Wykresy 
logse<-c(0.04915215,0.02783301,0.04896369,0.01427623,0.02111659,0.05695230,0.02474250,0.02717375)
probitse<-c(0.04683627,0.03000616,0.04608689,0.01568114,0.02311447,0.05346733,0.02497647,0.02706920)

print("Wykres dla modelu logitowego.")
x<-c(1:8)
plot(frakcja, col="blue", main="Wykres otrzymanego wyniku dla grup", pch=19, cex=1.3, xlim=c(1,8),ylim=c(0.0,1.25),xlab="Numer grupy",ylab="Prawdopodobienstwo prze�ycia")
points(x,licznosc$logit, col="red", pch=19, cex=1.3)
lines(x,licznosc$logit, col="red", lty=1, lwd=2)
lines(x,licznosc$logit+1.96*logse, col="green", lty=2, lwd=2)
lines(x,licznosc$logit-1.96*logse, col="green", lty=2, lwd=2)
legend(x=0.5,y=1.2,legend=c("prognozy z modelu logitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=1, text.width=0.5, adj=0)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Wykres dla modelu probitowego.")

x<-c(1:8)
plot(frakcja, col="blue", main="Wykres otrzymanego wyniku dla grup", pch=19, cex=1.3, xlim=c(1,8),ylim=c(0.0,1.25),xlab="Numer grupy",ylab="Prawdopodobienstwo prze�ycia")
points(x,licznosc$probit, col="red", pch=19, cex=1.3)
lines(x,licznosc$probit, col="red", lty=1, lwd=2)
lines(x,licznosc$probit+1.96*probitse, col="green", lty=2, lwd=2)
lines(x,licznosc$probit-1.96*probitse, col="green", lty=2, lwd=2)
legend(x=0.5,y=1.2,legend=c("prognozy z modelu probitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=1, text.width=0.5, adj=0)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Analiza jako�ci dopasowania modelu logitowego.")

par(mfrow=c(2,2))
x=c(1,2)
plot(x, licznosc$frakcja[c(2,6)], col="blue", main="Pr prze�. m�czyzn poni�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,0.65),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(2,6)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(2,6)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(2,6)]+1.96*logse[c(2,6)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(2,6)]-1.96*logse[c(2,6)], col="green", lty=2, lwd=2)
legend(x=1,y=0.65,legend=c("prognozy z modelu logitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(4,8)], col="blue", main="Pr prze�. m�czyzn powy�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,1.1),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(4,8)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(4,8)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(4,8)]+1.96*logse[c(4,8)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(4,8)]-1.96*logse[c(4,8)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu logitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(1,5)], col="blue", main="Pr prze�. kobiet poni�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1.2),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(1,5)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(1,5)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(1,5)]+1.96*logse[c(1,5)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(1,5)]-1.96*logse[c(1,5)], col="green", lty=2, lwd=2)
legend(x=1,y=1.2,legend=c("prognozy z modelu logitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(3,7)], col="blue", main="Pr prze�. kobiet powy�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$logit[c(3,7)], col="red", pch=19, cex=1)
lines(x,licznosc$logit[c(3,7)], col="red", lty=1, lwd=2)
lines(x,licznosc$logit[c(3,7)]+1.96*logse[c(3,7)], col="green", lty=2, lwd=2)
lines(x,licznosc$logit[c(3,7)]-1.96*logse[c(3,7)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu logitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Analiza jako�ci dopasowania modelu probitowego.")
x=c(1,2)
plot(x, licznosc$frakcja[c(2,6)], col="blue", main="Pr prze�. m�czyzn poni�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,0.65),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(2,6)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(2,6)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(2,6)]+1.96*probitse[c(2,6)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(2,6)]-1.96*probitse[c(2,6)], col="green", lty=2, lwd=2)
legend(x=1,y=0.65,legend=c("prognozy z modelu probitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(4,8)], col="blue", main="Pr prze�. m�czyzn powy�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.0,1.1),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(4,8)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(4,8)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(4,8)]+1.96*probitse[c(4,8)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(4,8)]-1.96*probitse[c(4,8)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu probitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(1,5)], col="blue", main="Pr prze�. kobiet poni�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1.2),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(1,5)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(1,5)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(1,5)]+1.96*probitse[c(1,5)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(1,5)]-1.96*probitse[c(1,5)], col="green", lty=2, lwd=2)
legend(x=1,y=1.2,legend=c("prognozy z modelu probitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

plot(x, licznosc$frakcja[c(3,7)], col="blue", main="Pr prze�. kobiet powy�ej 20 roku", pch=19, cex=1.1, xlim=c(1,2),ylim=c(0.4,1),xlab="Klasa podr�y",ylab="Prawdopodobienstwo")
points(x,licznosc$probit[c(3,7)], col="red", pch=19, cex=1)
lines(x,licznosc$probit[c(3,7)], col="red", lty=1, lwd=2)
lines(x,licznosc$probit[c(3,7)]+1.96*probitse[c(3,7)], col="green", lty=2, lwd=2)
lines(x,licznosc$probit[c(3,7)]-1.96*probitse[c(3,7)], col="green", lty=2, lwd=2)
legend(x=1,y=1,legend=c("prognozy z modelu probitowego","frakcje ocala�ych","przedzia� ufno�ci"),pt.cex = c(1,1,1), lwd=c(-1,-1,2),lty=c(-1,-1,2),pch=c(19,19,-1), col=c("red","blue","green"), bty="n", cex=0.5, text.width=0.5, adj=0)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Dopasowanie modelu po usuni�ciu zmiennej Age dla danych pierwotnych.")

titanic1<-read.table("titanic.txt", sep="\t", header=T)
attach(titanic1)


print("Tablica kontyngencji Survived~Sex.")
ocaleni<-xtabs(Survived~Sex, data=titanic1)
martwi<-xtabs(1-Survived~Sex, data=titanic1)
tab.sur.sex2<-cbind(ocaleni,martwi)
frakcja<-tab.sur.sex2[,1]/(tab.sur.sex2[,1]+tab.sur.sex2[,2])
tab.sur.sex2<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.sex2)

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Badanie niezale�no�ci zmiennej Survived i Sex")

print("Iloraz sznas")
print(odds.ratio(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))
print("Oba testy jednoznacznie odrzucaj� hipotez� o niezale�no�ci zmiennych. Iloraz szans r�wnie� jest znacz�co wi�kszy od 1.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Tablica kontyngencji Survived~PClass")
ocaleni<-xtabs(Survived~PClass, data=titanic1)
martwi<-xtabs(1-Survived~PClass, data=titanic1)
tab.sur.pclass1<-cbind(ocaleni, martwi)
frakcja<-tab.sur.pclass1[,1]/(tab.sur.pclass1[,1]+tab.sur.pclass1[,2])
tab.sur.pclass1<-cbind(ocaleni,martwi,frakcja)
print(tab.sur.pclass1)
 
readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Badanie niezale�no�ci zmniennej Survived oraz PClass.")

print("Dok�adny test Fishera")
print(fisher.test(cbind(ocaleni,martwi)))

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Test chi- kwadrat")
print(chisq.test(cbind(ocaleni,martwi)))
print("Oba testy jednoznacznie odrzucaj� hipotez� o niezale�no�ci zmiennych.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

titanic1$PClass<-factor(titanic1$PClass, levels=c("3rd","2nd","1st"), ordered=T)

print("Model logitowy.")
modelus<-glm(titanic1$Survived~titanic1$PClass+titanic1$Sex, family=binomial(link=logit))
print(summary(modelus))
print("W modelu widzimy wsp�czynnik nieistotny dla klasy �redniej zmiennej PClass.")
print(" Kryterium Akaike jest znacznie wy�sze od wybranego modelu. �wiadczy to, �e wybrany model jest lepszy.")

readline("Prosz� nacisn�� dowolny klawisz, aby kontynuowa�.")

print("Koniec.")