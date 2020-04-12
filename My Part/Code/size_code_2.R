library(dplyr)
library(reshape2)
library(ggplot2)

### CAPON
## EXPONENTIAL
exp_size=as.matrix(capon_size_theta_1_000_rExp_n_5_40_m_5_40)
rownames(exp_size)=as.character(5:40)
n_5=exp_size["5",]
n_10=exp_size["10",]
n_15=exp_size["15",]
n_20=exp_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Exponential Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=exp_size["25",]
n_30=exp_size["30",]
n_35=exp_size["35",]
n_40=exp_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Exponential Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## GAMMA
gamma_size=as.matrix(capon_size_theta_1_000_rGamma_m_5_40_n_5_40)
rownames(gamma_size)=as.character(5:40)
n_5=gamma_size["5",]
n_10=gamma_size["10",]
n_15=gamma_size["15",]
n_20=gamma_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Gamma Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=gamma_size["25",]
n_30=gamma_size["30",]
n_35=gamma_size["35",]
n_40=gamma_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Gamma Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## NORMAL
normal_size=as.matrix(capon_size_theta_1_000_rNorm_n_5_40_m_5_40)
rownames(normal_size)=as.character(5:40)
n_5=normal_size["5",]
n_10=normal_size["10",]
n_15=normal_size["15",]
n_20=normal_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Normal Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=normal_size["25",]
n_30=normal_size["30",]
n_35=normal_size["35",]
n_40=normal_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Normal Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## WEIBULL
weibull_size=as.matrix(capon_size_theta_1_000_rWeibull_m_5_40_n_5_40)
rownames(weibull_size)=as.character(5:40)
n_5=weibull_size["5",]
n_10=weibull_size["10",]
n_15=weibull_size["15",]
n_20=weibull_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Weibull Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=weibull_size["25",]
n_30=weibull_size["30",]
n_35=weibull_size["35",]
n_40=weibull_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Weibull Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))


### SAVAGE
## EXPONENTIAL
exp_size=as.matrix(savage_size_theta_1_000_rExp_m_5_40_n_5_40)
rownames(exp_size)=as.character(5:40)
n_5=exp_size["5",]
n_10=exp_size["10",]
n_15=exp_size["15",]
n_20=exp_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Exponential Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=exp_size["25",]
n_30=exp_size["30",]
n_35=exp_size["35",]
n_40=exp_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Exponential Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## GAMMA
gamma_size=as.matrix(savage_size_theta_1_000_rGamma_m_5_40_n_5_40)
rownames(gamma_size)=as.character(5:40)
n_5=gamma_size["5",]
n_10=gamma_size["10",]
n_15=gamma_size["15",]
n_20=gamma_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Gamma Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=gamma_size["25",]
n_30=gamma_size["30",]
n_35=gamma_size["35",]
n_40=gamma_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Gamma Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## NORMAL
normal_size=as.matrix(savage_size_theta_1_000_rNorm_m_5_40_n_5_40)
rownames(normal_size)=as.character(5:40)
n_5=normal_size["5",]
n_10=normal_size["10",]
n_15=normal_size["15",]
n_20=normal_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Normal Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=normal_size["25",]
n_30=normal_size["30",]
n_35=normal_size["35",]
n_40=normal_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Normal Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## WEIBULL
weibull_size=as.matrix(savage_size_theta_1_000_rWeibull_m_5_40_n_5_40)
rownames(weibull_size)=as.character(5:40)
n_5=weibull_size["5",]
n_10=weibull_size["10",]
n_15=weibull_size["15",]
n_20=weibull_size["20",]
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Weibull Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))
n_25=weibull_size["25",]
n_30=weibull_size["30",]
n_35=weibull_size["35",]
n_40=weibull_size["40",]
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Weibull Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

## CAPON
n_5=pbsapply(1:40, function(i) capon_rejection(5,i,rdist = rLogis))
n_10=pbsapply(1:40, function(i) capon_rejection(10,i,rdist = rLogis))
n_15=pbsapply(1:40, function(i) capon_rejection(15,i,rdist = rLogis))
n_20=pbsapply(1:40, function(i) capon_rejection(20,i,rdist = rLogis))
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Logistic Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

n_25=pbsapply(1:40, function(i) capon_rejection(25,i,rdist = rLogis))
n_30=pbsapply(1:40, function(i) capon_rejection(30,i,rdist = rLogis))
n_35=pbsapply(1:40, function(i) capon_rejection(35,i,rdist = rLogis))
n_40=pbsapply(1:40, function(i) capon_rejection(40,i,rdist = rLogis))
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Logistic Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))


## SAVAGE
n_5=pbsapply(1:40, function(i) savage_rejection(5,i,rdist = rLogis))
n_10=pbsapply(1:40, function(i) savage_rejection(10,i,rdist = rLogis))
n_15=pbsapply(1:40, function(i) savage_rejection(15,i,rdist = rLogis))
n_20=pbsapply(1:40, function(i) savage_rejection(20,i,rdist = rLogis))
df1=melt(cbind(n_5,n_10,n_15,n_20)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df1,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Logistic Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

n_25=pbsapply(1:40, function(i) savage_rejection(25,i,rdist = rLogis))
n_30=pbsapply(1:40, function(i) savage_rejection(30,i,rdist = rLogis))
n_35=pbsapply(1:40, function(i) savage_rejection(35,i,rdist = rLogis))
n_40=pbsapply(1:40, function(i) savage_rejection(40,i,rdist = rLogis))
df2=melt(cbind(n_25,n_30,n_35,n_40)) %>% rename(Values_of_m = Var1,Values_of_n = Var2)
ggplot(df2,aes(x=Values_of_m,y=value))+geom_line()+facet_wrap(~Values_of_n,scales = "free",ncol=2)+
  geom_hline(yintercept = 0.05)+labs(title = "Logistic Distribution",y="Achieved level of significance")+
  theme(plot.title = element_text(hjust = 0.5))

