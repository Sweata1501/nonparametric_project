find_rejection_matrix(c(5:40),c(5:40)) #normal distribution
find_rejection_matrix(c(5:40),c(5:40),stat_rejection = capon_rejection,rdist = rExp)
find_rejection_matrix(c(5:40),c(5:40),stat_rejection = capon_rejection,rdist = rGamma)

find_rejection_matrix(c(5:40),c(5:40),stat_rejection = capon_rejection,rdist = rWeibull)
find_rejection_matrix(c(5:40),c(5:40),stat_rejection = capon_rejection,rdist = rCauchy)
find_rejection_matrix(c(5:40),c(5:40),stat_rejection = capon_rejection,rdist = rLogis)