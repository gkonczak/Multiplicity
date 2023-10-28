# test for example data
library(lsr)
library(copula)

# e) wariant zmiennej zależnej i 4 niezależnych
norm.cop <- normalCopula(param=c(0.9,0.5,0,0,0.0,0.2,0.2,0.1,0.3,0.1),dim=5,dispstr = "un")

n=40
dat <- rCopula(n, norm.cop)
dat.df=as.data.frame(dat)
names(dat.df)=c('X1','X2','X3','X4','X5')
plot(dat.df)
N_perm=9999
# Podaj s
s=3

wynik=paste('istotne ',s)
ASL=0

for (s_ in 1:s) {
  RR=c()
  for (i in 2:5){
    RR=c(RR,cor(dat[,1],dat[,i]))
  }
  RR=abs(RR)
  T0=sort(RR,decreasing=TRUE)[s_]
  T=T0
  for (sim in 1:N_perm){
    dat_p=data.frame(sample(dat[,1]),dat[,2:5])
    RRs=c()
    for (i in 2:5){
      RRs=c(RRs,cor(dat_p[,1],dat_p[,i]))
    }
    RRs=abs(RRs)  
    T=c(T,sort(RRs,decreasing=TRUE)[s_])
  }
  ASL=sum (T>=T0)/(N_perm+1)
  print(ASL)
  if (ASL>=0.05) { wynik=paste('nieistotne',s);break}
}

print(s_)
wynik











