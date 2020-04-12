#results about estimation of size for Capon's statistic

#let m/N=lambda=0.5
library(pbapply)
library(lattice)
exp=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rExp))
normal=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rNorm))
gamma=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rGamma))
weibull=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rWeibull))
logis=pbsapply(1:40, function(i) capon_rejection(i,i,rdist = rLogis))
xyplot(exp+normal+gamma+weibull+logis~1:40,grid=TRUE,type="l",auto.key = TRUE,panel = function(...){
  panel.xyplot(...)
  panel.abline(h=0.05, col.line = "blue")
},xlab = "Common value of m and n", ylab = "Estimate of alpha")

#results about estimation of size for savage statistic

exp=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rExp))
normal=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rNorm))
gamma=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rGamma))
weibull=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rWeibull))
logis=pbsapply(1:40, function(i) savage_rejection(i,i,rdist = rLogis))
xyplot(exp+normal+gamma+weibull+logis~1:40,grid=TRUE,type="l",auto.key = TRUE,panel = function(...){
  panel.xyplot(...)
  panel.abline(h=0.05, col.line = "blue")
},xlab = "Common value of m and n", ylab = "Estimate of alpha")
