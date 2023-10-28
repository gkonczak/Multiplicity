# test for example data
library(lsr)
library(copula)

#A
#norm.cop <- normalCopula(param=c(0.9,0.5,0.0,0.0,0.4,0,0,0,0,0.0),dim=5,dispstr = "un")
#A1
#norm.cop <- normalCopula(param=c(0.8,0.4,0.0,0.0,0.4,0,0,0,0,0.0),dim=5,dispstr = "un")
#Bx
# norm.cop <- normalCopula(param=c(0.8,0.3,0,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")
#B
# norm.cop <- normalCopula(param=c(0.7,0.2,0,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")
#B1
# norm.cop <- normalCopula(param=c(0.6,0.2,0,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")
#C
# norm.cop <- normalCopula(param=c(0.6,0.3,0.2,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")
#C1
# norm.cop <- normalCopula(param=c(0.5,0.3,0.2,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")
#D
# norm.cop <- normalCopula(param=c(0.4,0.3,0.2,0.1,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")

# Niezależne
norm.cop <- normalCopula(param=c(0,0,0,0,0.0,0,0,0,0,0),dim=5,dispstr = "un")

L_sym=1000
# e) wariant zmiennej zależnej i 4 niezależnych, dwie zależności - sprawdzamy ile wykryjemy

wynik=c()
wynik_H=c()
czas=Sys.time()
for (lsm in 1:L_sym){

n=40
dat <- rCopula(n, norm.cop)
dat.df=as.data.frame(dat)
names(dat.df)=c('Y','X1','X2','X3','X4')
plot(dat.df)
N_perm=9999

ASL=0

for (istotne in 1:4){
  RR=c()
  for (i in 2:5){
    RR=c(RR,cor(dat[,1],dat[,i]))
  }
  RR=abs(RR)
  T0=sort(RR,decreasing=TRUE)[istotne]
  T=T0
  for (sim in 1:N_perm){
    dat_p=data.frame(sample(dat[,1]),dat[,2:5])
    RRs=c()
    for (i in 2:5){
      RRs=c(RRs,cor(dat_p[,1],dat_p[,i]))
    }
    RRs=abs(RRs)  
    T=c(T,sort(RRs,decreasing=TRUE)[istotne])
  }
  ASL=sum (T>=T0)/(N_perm+1)
  print(ASL)
  if (ASL>=0.05) { break}
}
if (ASL<0.05) istotne=istotne+1
wynik=c(wynik,istotne-1)
wynik_H=c(wynik_H,sum(correlate(dat.df[,1],dat.df[,2:5],test=TRUE)$p.value<0.05))
}
wynik
wynik_H

table(wynik)
table(wynik_H)

table(wynik)/L_sym
table(wynik_H)/L_sym

par(mfrow=c(2,1))
plot(table(wynik)/L_sym,xlim=c(0,4),ylim=c(0,.90), type='h', xlab='number of significant correlation', ylab='estimated probability', main='Permutation method')
plot(table(wynik_H)/L_sym,xlim=c(0,4),ylim=c(0,.90), type='h', xlab='number of significant correlation', ylab='estimated probability',main='Holm method')

Sys.time()-czas

