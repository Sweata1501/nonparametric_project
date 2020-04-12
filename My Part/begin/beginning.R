library(dplyr)
library(ggplot2)
library(reshape2)

n=10^2
theta=1.5

s1=sort(rNorm(n))
s2=sort(rLogis(n))
s3=sort(rExp(n))
s4=sort(rGamma(n))
s5=sort(rWeibull(n))

sample=rep(c(s1,s2,s3,s4,s5),times=2)
F_x=c(pnorm(s1),plogis(s2),pexp(s3),pgamma(s4,0.5),pweibull(s5,0.5))
F_theta_x=c(pnorm(theta*s1),plogis(theta*s2),pexp(theta*s3),pgamma(theta*s4,0.5),pweibull(theta*s5,0.5))
Distribution=rep(c("Normal","Logistic","Exponential","Gamma","Weibull"),each=100,times=2)

d=melt(cbind(F_x,F_theta_x))
df=cbind(d,sample,Distribution) %>% select(-Var1) %>% rename(Distn_func = Var2)

full_support=filter(df,Distribution %in% c("Normal","Logistic"))
ggplot(full_support,aes(x=sample,y=value,col=Distn_func))+geom_line(size=1)+facet_grid(~Distribution,
                                                                                       scales = "free")+
  labs(x="x",y="F(.)")

pos_support=filter(df,Distribution %in% c("Exponential","Gamma","Weibull"))
ggplot(pos_support,aes(x=sample,y=value,col=Distn_func))+geom_line(size=1)+facet_grid(~Distribution,
                                                                                      scales = "free")+
  labs(x="x",y="F(.)")
