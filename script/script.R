library(MASS)
library(fitdistrplus)

set.seed(10)

x <- list()
fittati <- list()
percentile <- list() 

i <- 1
for (i in 1:100){ 
x[[i]] <- rnorm(1000,sample(-10:10, size = 1),sample(1:3, size = 1))
percentile[[i]] <- quantile(x[[i]], p=0.05) 
}

fittati <- lapply(x, distr = "norm" , fitdistrplus::fitdist)

##################
#Case Monte Carlo#
##################

p <- matrix(c(rnorm(1000,50,4), rnorm(1000,5,0.5)), ncol = 2)

colnames(p)<-c("x1", "x2")
weigths <- c(0.5,0.5)

mu<-apply(p,2,mean) 

sigma <- cov(p)

MC <- mvrnorm(10000, mu, sigma)

MCportfolio <- MC%*%diag(weigths)

percentile(MCportfolio, p = 0.05)
                         