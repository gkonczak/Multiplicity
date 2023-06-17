# test for example data

dat=read.csv('data_std.csv')

N250_low_diff <- dat$N250_low_diff
N250_high_diff <- dat$N250_high_diff
N250_Joe_diff <- dat$N250_Joe_diff
Easy <- dat$Easy.IRT
Hard <- dat$Hard.performance
dat <- data.frame(N250_low_diff, N250_high_diff, N250_Joe_diff, Easy,Hard)



N_perm=10000
s=1

RR=c(cor(dat[,1],dat[,4]),cor(dat[,2],dat[,4]),cor(dat[,3],dat[,4]),cor(dat[,1],dat[,5]),cor(dat[,2],dat[,5]),cor(dat[,3],dat[,5]))
T0=sort(RR,decreasing=FALSE)[s]
T=c()
for (sim in 1:N_perm)
{
  dat_p=data.frame(dat[,1], sample(dat[,2]), sample(dat[,3]), sample(dat[,4]),sample(dat[,5]))
  RRs=c()
  RRs=c(RRs,cor(dat_p[,1],dat_p[,4]),cor(dat_p[,2],dat_p[,4]),cor(dat_p[,3],dat_p[,4]),cor(dat_p[,1],dat_p[,5]),cor(dat_p[,2],dat_p[,5]),cor(dat_p[,3],dat_p[,5]))
  T=c(T,sort(RRs,decreasing=FALSE)[s])
}
ASL=sum (T<=T0)/N_perm
T0
hist(T)
ASL




