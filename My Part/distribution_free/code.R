library(pbapply)
library(lattice)
library(openxlsx)
repl=1000

qq <- function(x, y){
  require(ggplot2)
  sx <- sort(x); sy <- sort(y)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y
  g = ggplot() + geom_point(aes(x=sx, y=sy))
  g
}

capon_normal=replicate(repl,simulate_capon(25,25))
capon_exp=replicate(repl,simulate_capon(25,25,rdist = rExp))
capon_gamma=replicate(repl,simulate_capon(25,25,rdist = rGamma))
capon_weibull=replicate(repl,simulate_capon(25,25,rdist = rWeibull))
capon_logis=replicate(repl,simulate_capon(25,25,rdist = rLogis))
capon_matrix=cbind(capon_normal,capon_exp,capon_gamma,capon_weibull,capon_logis)
write.xlsx(capon_matrix,sprintf("distribution_free/distn-free_capon.xlsx"))

savage_normal=replicate(repl,simulate_savage(25,25))
savage_exp=replicate(repl,simulate_savage(25,25,rdist = rExp))
savage_gamma=replicate(repl,simulate_savage(25,25,rdist = rGamma))
savage_weibull=replicate(repl,simulate_savage(25,25,rdist = rWeibull))
savage_logis=replicate(repl,simulate_savage(25,25,rdist = rLogis))
savage_matrix=cbind(savage_normal,savage_exp,savage_gamma,savage_weibull,savage_logis)
write.xlsx(savage_matrix,sprintf("distribution_free/distn-free_savage.xlsx"))

library(gridExtra)
p1=qq(capon_exp,capon_normal)
p2=qq(capon_exp,capon_gamma)
p3=qq(capon_exp,capon_weibull)
p4=qq(capon_exp,capon_logis)
p5=qq(capon_gamma,capon_normal)
p6=qq(capon_gamma,capon_weibull)
p7=qq(capon_gamma,capon_logis)
p8=qq(capon_normal,capon_weibull)
p9=qq(capon_normal,capon_logis)
p10=qq(capon_weibull,capon_logis)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,nrow=2,ncol=5)


p1=qq(savage_exp,savage_normal)
p2=qq(savage_exp,savage_gamma)
p3=qq(savage_exp,savage_weibull)
p4=qq(savage_exp,savage_logis)
p5=qq(savage_gamma,savage_normal)
p6=qq(savage_gamma,savage_weibull)
p7=qq(savage_gamma,savage_logis)
p8=qq(savage_normal,savage_weibull)
p9=qq(savage_normal,savage_logis)
p10=qq(savage_weibull,savage_logis)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,nrow=2,ncol=5)
