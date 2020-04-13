library(pbapply)
library(lattice)
library(ggplot2)
library(dplyr)
library(reshape2)

## CAPON'S STATISTIC

exp=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rExp))
normal=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rNorm))
gamma=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rGamma))
weibull=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rWeibull))
logis=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rLogis))

Distribution=rep(c("Exponential","Normal","Gamma","Weibull","Logistic"),each=40)
sample_no=rep(1:40,times=5)
df1=melt(cbind(exp,normal,gamma,weibull,logis)) %>% select(-c(Var1,Var2)) %>% mutate(Distribution,sample_no)

ggplot(df,aes(x=sample_no,y=value,col=Distribution))+geom_line()+facet_grid(~Distribution)+
  labs(title = "Capon's statistic",y="Estimate of alpha")+ geom_hline(yintercept = 0.05)  +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = "none")

## SAVAGE STATISTIC

exp=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rExp))
normal=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rNorm))
gamma=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rGamma))
weibull=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rWeibull))
logis=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rLogis))

Distribution=rep(c("Exponential","Normal","Gamma","Weibull","Logistic"),each=40)
sample_no=rep(1:40,times=5)
df2=melt(cbind(exp,normal,gamma,weibull,logis)) %>% select(-c(Var1,Var2)) %>% mutate(Distribution,sample_no)

ggplot(df2,aes(x=sample_no,y=value,col=Distribution))+geom_line()+facet_grid(~Distribution)+
  labs(title = "Savage statistic",y="Estimate of alpha")+ geom_hline(yintercept = 0.05)  +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = "none")
