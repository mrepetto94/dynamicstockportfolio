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

################
#Case Bootstrap#
################

x <- rnorm(1000, 0, 1)
percentile(x, p = 0.05)
ci <- quantile(x, p = 0.05)

y <- rnorm(500, 0, 1)
i<-0
boot <- 0
s <-0
k <- 100000

for (i in 1:k){
  s <- sample(y, size = k, replace = TRUE)
  boot[i] <- quantile(s, p = 0.05)
}

mean(boot)

