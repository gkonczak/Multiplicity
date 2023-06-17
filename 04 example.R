
library(copula)

# Four variants of the copula function
# a)
norm.cop <- normalCopula(param=c(0.2,0.1,0.1,0.2,0.1,0.2,0.1,0.2,0.2,0.2),dim=5,dispstr = "un")
# b)
norm.cop <- normalCopula(param=c(0.3,0.3,0.1,0.2,0.3,0.2,0.3,0.2,0.3,0.3),dim=5,dispstr = "un")
# c)
norm.cop <- normalCopula(param=c(0.5,0.5,0.7,0.2,0.3,0.2,0.5,0.4,0.3,0.3),dim=5,dispstr = "un")
# d)
norm.cop <- normalCopula(param=c(0.9,0.7,0.5,0.5,0.4,0.5,0.5,0.3,0.3,0.5),dim=5,dispstr = "un")


# Examples of using the R.mht function
R.mht(n=5,k=5,s=1,N_SIM=1000,N_perm=100)

R.mht(10,k=5,s=2,N_SIM=1000,N_perm=100)


# Some plots

n=100
dat <- rCopula(n, norm.cop)
dat.df=as.data.frame(dat)
names(dat.df)=c('X1','X2','X3','X4','X5')
plot(dat.df)


library(ggplot2)
library(GGally)

#create scatterplot matrix
ggpairs(dat.df,  upper = list( continuous = "cor"),
        lower = list(continuous = "points", combo = "dot_no_facet"))


library(Cairo)

Cairo::Cairo(
  30, #length
  30, #width
  file = "e:/b.jpeg",
  type = "jpeg", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
ggpairs(dat.df,  upper = list( continuous = "cor"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()


tiff(file = "e:/t1.tiff", width = 4000, height = 4000, units = "px", res = 600)
ggpairs(dat.df,  upper = list( continuous = "cor"),
        lower = list(continuous = "points", combo = "dot_no_facet"))
dev.off()



ggsave("fig.jpeg", dpi=600)



