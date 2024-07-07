library(rjags)

rm(list=ls())

im_dir <- "./temp"

mydata.list <- readRDS(file=file.path(im_dir, "data.mdv2.rds"))

# model
model <- "
data{ 
  dx <- dim(X)
  dy <- dim(Y)
  zeros.beta <- rep(0, dx[2])
  df.beta <- dx[2]+1
  zeros.lambda <- rep(0, dy[2]-1)
  df.lambda <- dy[2]
  }
## Likelihood  
model{
  for (i in 1:dx[1]) {
  for (j in 1:dy[2]) { 
     Y[i,j] ~ dnorm(Y.hat[i,j], kappa[j])
  }
  for (j in 1:(dy[2]-1)) {
     Y.hat[i,j] <- theta[j] + mu[i]*lambda[j]
   }
   Y.hat[i,dy[2]] <- mu[i]
   mu[i] ~ dnorm(m.y[i], tau)
   m.y[i] <- inprod(X[i,], beta) + err[sid[i]]
   }
  
  for (j in 1:k) {
   err[j] ~ dnorm(0, tau.int) 
  }
   
## Priors
  for (j in 1:dy[2]) {
    kappa[j] ~ dgamma(0.01,0.01)
  }

  theta ~ dmnorm(zeros.lambda, Omega.theta)    
  Omega.theta ~ dwish(0.01*ones.lambda, df.lambda)
  
  lambda ~ dmnorm(zeros.lambda, Omega.lambda)
  Omega.lambda ~ dwish(0.01*ones.lambda, df.lambda)
  
  beta ~ dmnorm(zeros.beta, Omega.beta)
  Omega.beta ~ dwish(0.01*ones.beta, df.beta)
  
  tau ~ dgamma(0.01,0.01)
  tau.int ~ dgamma(0.01,0.01)
}"

# estimation 
## list parameters
bayes.mod.params <- c("beta","theta","lambda","kappa","tau.int","tau")

### run mcmc
samples.list <- list()
dic <- numeric()

for (j in seq_along(mydata.list)) {
mydata <- mydata.list[[j]]
mod.fit <- jags.model(textConnection(model), data=mydata, n.chains = 3, n.adapt=200)
try(update(mod.fit,10000))
samples.list[[j]] <- tryCatch(
  coda.samples(mod.fit, variable.names=bayes.mod.params, thin=10, n.iter=12000),
  error=function(e) NULL)
try(ds <- dic.samples(mod.fit, n.iter=5000, type="popt"))
dic[j] <- sum(ds$deviance+ds$penalty)
}

names(samples.list) <- names(mydata.list)
names(dic) <- names(mydata.list)

saveRDS(samples.list, file=file.path(im_dir,"postsamB.ml-India.mdv2.rds"))
saveRDS(dic, file=file.path(im_dir,"dicB.ml-India.mdv2.rds"))

