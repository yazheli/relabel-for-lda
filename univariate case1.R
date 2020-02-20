myloopfunction = function(i){
  library(MASS)
  M = 10000
  r = 0.5
  ep = 0.5
  
  n0 = M * r
  n1 = M * (1-r)
  n2 = n1 * (1-ep)
  n3 = n1 * ep
  
  rho <- 0 #correlation coefficient
  mu1 <- 0; s1 <- 1
  mu2 <- 0; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2) # Covariance matrix
  bvn1 <- mvrnorm(n0, mu = mu, Sigma = sigma )
  
  
  rho <- 0 #correlation coefficient
  mu1 <- 3; s1 <- 1
  mu2 <- 0; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
  bvn2 <- mvrnorm(n2, mu = mu, Sigma = sigma )
  
  rho <- 0 #correlation coefficient
  mu1 <- 0; s1 <- 1
  mu2 <- 3; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
  bvn3 <- mvrnorm(n3, mu = mu, Sigma = sigma )
  
  
  train = rbind(bvn1,bvn2,bvn3)
  re = factor(c(rep("c",n0), rep("s",n2), rep("v",n3)))
  bi = factor(c(rep("c",n0), rep("s+v",n2+n3)))
  library(MASS)
  re <- lda(train, re)
  bi <- lda(train, bi)
  
  
  rho <- 0 #correlation coefficient
  mu1 <- 0; s1 <- 1
  mu2 <- 0; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2) # Covariance matrix
  bvn1 <- mvrnorm(n0, mu = mu, Sigma = sigma )
  
  
  rho <- 0 #correlation coefficient
  mu1 <- 3; s1 <- 1
  mu2 <- 0; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
  bvn2 <- mvrnorm(n2, mu = mu, Sigma = sigma )
  
  rho <- 0 #correlation coefficient
  mu1 <- 0; s1 <- 1
  mu2 <- 3; s2 <- 1
  mu <- c(mu1,mu2) # Mean 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
  bvn3 <- mvrnorm(n3, mu = mu, Sigma = sigma )
  
  
  test = rbind(bvn1,bvn2,bvn3)
  #test$re = factor(c(rep("s",n0), rep("c",n2), rep("v",n3)))
  
  testbi = factor(c(rep("c",n0), rep("s+v",n2+n3)))
  
  biresult = predict(bi, newdata = test)
  reresult = predict(re, newdata = test)
  
  prbi = biresult$posterior[,2]
  prre = reresult$posterior[,2] + reresult$posterior[,3]
  
  classbi = biresult$class
  classre = reresult$class
  
  library(hmeasure)
  auc = summary(HMeasure(true.class = (testbi == 's+v'), scores = prre))[3] - summary(HMeasure(true.class = (testbi == 's+v'), scores = prbi))[3]
  
  levels(classre) <- c("c", "s", "v","s+v")
  classre[classre != 'c'] = 's+v'
  
  er = (sum(as.character(classre) == as.character(testbi)) - sum(classbi == testbi))/10000 
  return(c(as.numeric(auc),er))
}
myloopfunction(1)
cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)
library(foreach)

value = foreach(i = 1:100, .combine = 'rbind',.errorhandling='pass') %dopar% {myloopfunction(k)}
colMeans(value)


####################################
#univariate example
####################################

myloopfunction = function(i){
  library(MASS)
  M = 100000
  r = 0.5
  ep = 0.5
  
  n0 = M * r
  n1 = M * (1-r)
  n2 = n1 * (1-ep)
  n3 = n1 * ep
  
  bvn1 <- rnorm(n0, mean = 0, sd = 1)
  bvn2 <- rnorm(n2, mean = 1, sd = 1)
  bvn3 <- rnorm(n3, mean = 2, sd = 1)
  
  
  
  train = c(bvn1,bvn2,bvn3)
  train = as.matrix(train)
  re = factor(c(rep("c",n0), rep("s",n2), rep("v",n3)))
  bi = factor(c(rep("c",n0), rep("s+v",n2+n3)))
  library(MASS)
  re <- lda(train, re)
  bi <- lda(train, bi)
  
  
  bvn1 <- rnorm(n0, mean = 0, sd = 1)
  bvn2 <- rnorm(n2, mean = 1, sd = 1)
  bvn3 <- rnorm(n3, mean = 2, sd = 1)
  test = c(bvn1,bvn2,bvn3)
  test = as.matrix(test)
  #test$re = factor(c(rep("s",n0), rep("c",n2), rep("v",n3)))
  
  testbi = factor(c(rep("c",n0), rep("s+v",n2+n3)))
  
  biresult = predict(bi, newdata = test)
  reresult = predict(re, newdata = test)
  
  prbi = biresult$posterior[,2]
  prre = reresult$posterior[,2] + reresult$posterior[,3]
  
  classbi = biresult$class
  classre = reresult$class
  
  library(hmeasure)
  #auc = summary(HMeasure(true.class = (testbi == 's+v'), scores = prre))[3] - summary(HMeasure(true.class = (testbi == 's+v'), scores = prbi))[3]
  
  levels(classre) <- c("c", "s", "v","s+v")
  classre[classre != 'c'] = 's+v'
  
  #er = (sum(as.character(classre) == as.character(testbi)) - sum(classbi == testbi))/100000 
  erre = (100000-sum(as.character(classre) == as.character(testbi)))/100000
  erbi = (100000 - sum(classbi == testbi))/100000   
    
  
  return(c(erre,erbi))
}

myloopfunction(1)

cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)
library(foreach)
value = foreach(i = 1:1000, .combine = 'rbind',.errorhandling='pass') %dopar% {myloopfunction(k)}
colMeans(value)





er1 = 0.5*integrate(dnorm, mean=0, sd=1, lower= (2*log(2)+1)/2, upper= Inf, abs.tol = 0)$value
er2 = 0.25*integrate(dnorm, mean=1, sd=1, lower= -Inf, upper= (2*log(2)+1)/2, abs.tol = 0)$value
er3 = 0.25*integrate(dnorm, mean=2, sd=1, lower= -Inf, upper= (2*log(2)+1)/2, abs.tol = 0)$value

er1+er2+er3     
colMeans(value)[1]

get.yield.equation <-function(x){
  -0.5*x^2-(6*x)+2*(1.5)^2-5*log(1/sqrt(1.25))
}
library(rootSolve)
root = uniroot.all(get.yield.equation, interval=c(-20,10), tol= 0.000000000000000001)

er1 = 0.5*integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= root[1], abs.tol = 0)$value + 0.5*integrate(dnorm, mean=0, sd=1, lower= root[2], upper= Inf, abs.tol = 0)$value
er2 = 0.5*integrate(dnorm, mean=1.5, sd=sqrt(1.25), lower= root[1], upper= root[2], abs.tol = 0)$value

er1+er2
colMeans(value)[2]


mu_0 = 0
mu_2 = -4
mu_3 = 2

model = function(x){dnorm(x , mean = mu_0, sd = 1) - dnorm(x , mean = (mu_2+mu_3)/2, sd = sqrt(1+(mu_2-mu_3)^2/4))}
ss <- uniroot.all(f = model, interval = c(-100,100))
ss = tail(ss,2)
if(model(mean(ss))>0){
  0.5*(pnorm(ss[2], mean = (mu_2+mu_3)/2, sd = sqrt(1+(mu_2-mu_3)^2/4))-pnorm(ss[1], mean = (mu_2+mu_3)/2, sd = sqrt(1+(mu_2-mu_3)^2/4)))+0.5*(pnorm(ss[1], mean = mu_0, sd = 1) + 1 - pnorm(ss[2], mean = mu_0, sd = 1))
} else {
  0.5*(pnorm(ss[2], mean = mu_0, sd = 1)-pnorm(ss[1], mean = mu_0, sd = 1))+0.5*(pnorm(ss[1], mean = (mu_2+mu_3)/2, sd = sqrt(1+(mu_2-mu_3)^2/4)) + 1 - pnorm(ss[2], mean = (mu_2+mu_3)/2, sd = sqrt(1+(mu_2-mu_3)^2/4)))
}


maxfunction = function(x){
  
  if (x<((mu_2+mu_3)/2)){
    d = dnorm(x , mean = mu_2, sd = 1)
  } else {
    d = dnorm(x , mean = mu_3, sd = 1)
  }
  
  return(d)
}

model = function(x){0.5*dnorm(x , mean = mu_0, sd = 1) - 0.25*maxfunction(x)}
ss1 <- uniroot.all(f = model, interval = c(-100,0))
ss1 = tail(ss1,1)
ss2 <- uniroot.all(f = model, interval = c(0,100))
ss2 = tail(ss2,1)

if(model((ss1+ss2)/2) > 0){
  0.25*(pnorm(ss2,mu_2,1)-pnorm(ss1,mu_2,1))+0.25*(pnorm(ss2,mu_3,1)-pnorm(ss1,mu_3,1))+0.5*(pnorm(ss1,mu_0,1)+1-pnorm(ss2,mu_0,1))
}else{
}
0.25*(1 - pnorm(ss1,mu_2,1))+0.25*(pnorm(ss2,mu_3,1))+0.5*(pnorm(ss1,mu_0,1)+1-pnorm(ss2,mu_0,1))

mu_0 = 0
mu_2 = -10
mu_3 = -9
mu_1 = (mu_2+mu_3)/2

curve(0.5*dnorm(x,mu_0,1), from=-10, to=10, xlab="x", ylab="y")
curve(0.25*dnorm(x,mu_2,1), from=-10, to=mu_1, xlab="x", ylab="y", add = T) 
curve(0.25*dnorm(x,mu_3,1), from=mu_1, to=10, xlab="x", ylab="y", add = T) 

curve(0.5*dnorm(x,0,1), from=-10, to=10, xlab="x", ylab="y")
curve(0.5*dnorm(x,mu_1,sqrt(1+(mu_2-mu_3)^2/4)), from=-10, to=10, xlab="x", ylab="y", add = T) 


##########################
#please ensure mu_2<mu_3
findmax = function(x,mu_2,mu_3){
  if (x<((mu_2+mu_3)/2)){
    d = dnorm(x , mean = mu_2, sd = 1)
  } else {
    d = dnorm(x , mean = mu_3, sd = 1)
  }
  return(d)
}

maxfunction = function(x,mu_2,mu_3){
  d = sapply(x,function(x){findmax(x,mu_2 = mu_2,mu_3 = mu_3)})
  return(d)
}

minfunction = function(x,mu_0,mu_2,mu_3){
  0.25*maxfunction(x,mu_2,mu_3)-0.5*dnorm(x, mean = mu_0, sd = 1)
}

minfunction3 = function(x,mu_0,mu_1,mu_2,mu_3){
  min(c(0.5*dnorm(x , mean = mu_1, sd = sqrt(1+(mu_2-mu_3)^2/4)),0.5*dnorm(x, mean = mu_0, sd = 1)))
}


erfunction = function(mu_0,mu_2,mu_3){
  mu_1 = (mu_2+mu_3)/2
  
  root = uniroot.all(function(x){minfunction(x, mu_0 = mu_0,mu_2 = mu_2,mu_3 = mu_3)}, interval=c(-10,10), tol= 0.000000000000000001)
  if(length(root)==2){
    erre = 0.25*(1 - pnorm(root[1],mu_2,1))+ 0.25*pnorm(root[2],mu_3,1) + 0.5*(pnorm(root[1],mu_0,1)+1-pnorm(root[2],mu_0,1))
  } else {
    erre = 0.25*pnorm(root,mu_2,1)+0.25*pnorm(root,mu_3,1)+0.5*(1-pnorm(root,mu_0,1))
    erre = min(c(erre,1-erre))
  }
  erbi = integrate(Vectorize(function(x){minfunction3(x,mu_0=mu_0,mu_1=mu_1,mu_2=mu_2,mu_3=mu_3)}), lower= -Inf, upper= Inf, abs.tol = 0)$value
  return(c(erre,erbi))
}


ErrorRate = expand.grid(x = seq(-2,2,0.01), y = seq(-2,2,0.01))
ErrorRate$erre = 0
ErrorRate$erbi = 0

loopfunction = function(i){
  return(erfunction(mu_0 = 0, mu_2 = min(ErrorRate[i,]), mu_3 = max(ErrorRate[i,])))
}

library(doParallel)
registerDoParallel(cores=7)
result = foreach(i=1:nrow(ErrorRate), .combine = "rbind") %dopar% loopfunction(i)


#for(i in 1:nrow(ErrorRate)){
#  ErrorRate[i,c(3,4)] = erfunction(mu_0 = 0, mu_2 = min(ErrorRate[i,]), mu_3 = max(ErrorRate[i,]))
#  print(i)
#}
ErrorRate$erre = result[,1]
ErrorRate$erbi = result[,2]
ErrorRate$'relabel_work?' = (ErrorRate$erre - ErrorRate$erbi)<0
ggplot(ErrorRate, aes(x = as.factor(round(ErrorRate$x,digits = 1)), y = as.factor(round(ErrorRate$y,digits = 1)), fill = ErrorRate$'relabel_work?')) +
  geom_tile() + xlab("mu_2")+ ylab('mu_3')

ggplot(ErrorRate, aes(x = ErrorRate$x, y = ErrorRate$y, fill = ErrorRate$'relabel_work?')) +
  geom_tile() + xlab("mu_2")+ ylab('mu_3')


plot(synth.tr[,1:2])
points(synth.tr[1:125,1:2],col="red")