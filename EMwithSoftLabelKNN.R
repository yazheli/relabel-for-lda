source('~/Desktop/relabel in lda/softLabelKNN.R', echo=TRUE)
library(MASS)
library(caret)
library(parallel)
library(doMC)

train = synth.te
test = synth.tr

load("~/Desktop/relabel in lda/simulation_train.rdata")
train = data
names(train) = c("x1","x2","yc")

y = train[which(train$yc==1),]   
nony = train[which(train$yc==0),]   
train = rbind(y,nony)

new_weight_c1 = runif(nrow(y), 0.45, 0.55)
new_weight_c2 = 1 - new_weight_c1



####initial M step
nony$yc = rep(1, nrow(nony))
y$yc = rep(2, nrow(y))
y$yc[new_weight_c2>0.5] = 3


new_data = rbind(nony,y)
cl = cbind(c(rep(1,nrow(nony)),rep(0,nrow(y))),
           c(rep(0,nrow(nony)),new_weight_c1),
           c(rep(0,nrow(nony)),new_weight_c2))
colnames(cl) = c(1,2,3)

#new_data$yc = as.factor(new_data$yc)
trainX <- new_data[,-3]
label = apply(cl, MARGIN = 1, which.max)

cvresult = vector()
for(k in 2:100){
  cvresult = c(cvresult,mean(cv.softLableKnn(trainX, cl, label, k = k, cvNumber = 5)))
}
plot(cvresult,type = 'l')
bestk = which.max(cvresult)+1;bestk

phi_1 = sum(new_weight_c1)/nrow(y)
phi_2 = sum(new_weight_c2)/nrow(y)

plot(train[,c(1:2)])
points(trainX[label == 2,c(1:2)],col = 'red')
points(trainX[label == 3,c(1:2)],col = 'blue')

n_iteration = 40
weight = matrix(0, nrow = n_iteration, ncol = nrow(y))
phi = matrix(0,nrow = n_iteration, ncol = 2)


for(j in 1 : n_iteration){
  #update weight
  p2 = softLableKnn(trainX, trainX, cl = cl, k = bestk)[2,]/sum(new_weight_c1)
  p3 = softLableKnn(trainX, trainX, cl = cl, k = bestk)[3,]/sum(new_weight_c2)
  p2 = p2[501:1000]
  p3 = p3[501:1000]
  
  new_weight_c1 = phi_1*p2/(phi_1*p2 + phi_2*p3)
  new_weight_c2 = phi_2*p3/(phi_1*p2 + phi_2*p3)
  
  ####M step solve likelihood function
  
  #new_data = rbind(nony,y)
  
  cl = cbind(c(rep(1,nrow(nony)),rep(0,nrow(y))),
             c(rep(0,nrow(nony)),new_weight_c1),
             c(rep(0,nrow(nony)),new_weight_c2))
  
  colnames(cl) = c(1,2,3)
  
  #new_data$yc = as.factor(new_data$yc)
  #trainX <- new_data[,-3]
  label = apply(cl, MARGIN = 1, which.max)
  
  cvresult = vector()
  for(k in 2:100){
    cvresult = c(cvresult,mean(cv.softLableKnn(trainX, cl, label, k = k, cvNumber = 5)))
  }
  print(max(cvresult))
  plot(cvresult,type = 'l')
  bestk = which.max(cvresult)+1;bestk
  
  phi_1 = mean(new_weight_c1,na.rm = T)
  phi_2 = mean(new_weight_c2,na.rm = T)
  
  
  phi[j,] = c(phi_1,phi_2)
  weight[j,] = new_weight_c2
  
}



plot(train[,c(1:2)])
points(trainX[label == 2,c(1:2)],col = 'red')
points(trainX[label == 3,c(1:2)],col = 'blue')

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  softLableKnn(trainX, grid, cl = cl, k = bestk)
z=matrix(prob.grid[1,], nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="red", drawlabels=FALSE, lwd=4,add = TRUE)



plot(knnFit)
knnFit$bestTune
matplot(phi,type = 'l',ylim = c(0,1))
matplot(weight[1:10,],type = 'l')

N = 500
rho <- 0 #correlation coefficient
mu1 <- 0; s1 <- 1
mu2 <- 0; s2 <- 1
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2) # Covariance matrix
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )


rho <- 0.7 #correlation coefficient
mu1 <- 0.75; s1 <- 0.2
mu2 <- 0.75; s2 <- 0.2
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
bvn2 <- mvrnorm(250, mu = mu, Sigma = sigma )

rho <- 0.7 #correlation coefficient
mu1 <- -0.75; s1 <- 0.2
mu2 <- -0.75; s2 <- 0.2
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
bvn3 <- mvrnorm(250, mu = mu, Sigma = sigma )


data = cbind(rbind(bvn1,bvn2,bvn3))
data = as.data.frame(data)
colnames(data) = c("xc","yc")
data$class = c(rep(1,N),rep(2,250),rep(2,250))
save(data, file = "simulation_train.rdata")
