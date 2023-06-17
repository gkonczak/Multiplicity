# the main function
R.mht <- function(n=5,k=5,s=1,N_SIM=1000,N_perm=100) 
{
p_val=c()
for (j in 1:N_SIM)
{
dat <- rCopula(n, norm.cop)
RR=c()
for (i in 1:(k-1))  
 for (j in (i+1) :k)   
  RR=c(RR,cor(dat[,i],dat[,j]))
                     
T0=sort(RR,decreasing=TRUE)[s]

T=c()
for (sim in 1:N_perm)
{
dat_s=cbind(dat[,1],sample(dat[,s]),sample(dat[,3]),sample(dat[,4]),sample(dat[,5]))
RRs=c()
for (i in 1:(k-1))  
 for (j in (i+1) :k)   
  RRs=c(RRs,cor(dat_s[,i],dat_s[,j]))

T=c(T,sort(RRs,decreasing=TRUE)[2])
}

ASL=sum (T>=T0)/N_perm
p_val=c(p_val,ASL)
}
results=list(p.val=sum (p_val<0.05)/N_SIM,T0=T0)
results
}

