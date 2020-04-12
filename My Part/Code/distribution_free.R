library(dplyr)
library(reshape2)
library(ggplot2)

sample_no=rep(1:1000,times=5)
distn=rep(c("Normal","Exponential","Gamma","Weibull","Logistic"),each=1000)
capon=melt(distn_free_capon) %>% select(-variable) %>% mutate(distn=distn,sample_no=sample_no)
ggplot(capon,aes(x=value,fill=distn))+geom_histogram(bins=50,col="black")+facet_wrap(~distn,nrow=2)+
  labs(x="Value of Capon's statistic")+theme(legend.position = "none")

savage=melt(distn_free_savage) %>% select(-variable) %>% mutate(distn=distn,sample_no=sample_no)
ggplot(savage,aes(x=value,fill=distn))+geom_histogram(bins=50,col="black")+facet_wrap(~distn,nrow=2)+
  labs(x="Value of Savage statistic")+theme(legend.position = "none")
