library(ggplot2)
library(dplyr)
library(reshape2)

Distribution=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=3)
Statistic=rep(c("F","Capon","Savage"),each=5000)
theta=rep(seq(1,5,length=1000),times=15)
df=melt(cbind(Parametric,Capon,Savage)) %>% select(-variable) %>% mutate(Distribution=Distribution,Statistic=
                                                                     Statistic, theta=theta)
ggplot(df,aes(x=theta,y=value,col=Statistic))+geom_line()+facet_wrap(~Distribution,ncol=3,scales = "free")+
  labs(title = "Comparison of power graph of F, Capon and Savage statistic",y="Power")+
  theme(plot.title = element_text(hjust = 0.5))
