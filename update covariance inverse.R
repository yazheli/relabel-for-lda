#update the covariance matrix
library(MASS)
X = synth.te
#C = cov(X[1:999,1:2])
#CT = cov(X[1:1000,1:2])

#library(onlinePCA)
#C
#updateCovariance(C, x = unlist(X[1000,1:2]), n = 999, xbar = colMeans(X[1:999,1:2]), byrow = T)
#CT


library(MASS)
X = synth.te[,1:2]
cov(X)
X[,1] = (X[,1] - colMeans(X)[1])
X[,2] = (X[,2] - colMeans(X)[2])
colMeans(X)
cov(X)

#sum = matrix(0, nrow = 2,ncol = 2)
#for(i in 1:250){
#  sum =  sum + t(as.matrix(X[i,]))%*%as.matrix(X[i,])
#}
#sum/249 
#cov(X)



n = nrow(X)
COV = cov(X)
c = n/(n-1)/(n-1)
invCOV = solve(COV)
xn = t(as.matrix(X[1000,1:2]))


#(n-1)*COV-t(as.matrix(X[250,]))%*%as.matrix(X[250,])*n/(n-1)
#(n-2)*COVd

(n-2)/(n-1)*(invCOV) + c*(n-2)/(n-1)*invCOV%*%xn%*%t(xn)%*%invCOV/as.numeric((1-c*t(xn)%*%invCOV%*%xn))

COVd = cov(X[1:999,1:2])

invCOVd = solve(COVd)
invCOVd

system.time(for(i in 1:10000){(n-2)/(n-1)*(invCOV) + c*(n-2)/(n-1)*invCOV%*%xn%*%t(xn)%*%invCOV/as.numeric((1-c*t(xn)%*%invCOV%*%xn))
})
system.time(for(i in 1:10000){COVd = cov(X[1:999,1:2])
invCOVd = solve(COVd)})












xy <- cbind(x = 1:10, y = c(1:3, 8:5, 8:10))
w1 <- c(0,0,0,1,1,1,1,1,0,0)
w2 = rep(0.1,10)
cov.wt(xy, wt = w1) 
cov.wt(xy, wt = w2) 
cov(xy)










