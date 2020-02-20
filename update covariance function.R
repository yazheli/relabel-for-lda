load("~/Desktop/data recovery/data.rdata")
load("~/Desktop/Desktop - liyazhe’s MacBook Pro/Freddie_Mac_data/cross year begin 2000/prepare data/data2000ca.rdata")
data = data[,-26]
#data = synth.te[,1:2]
delete_ind = c(1000,999)
original_invcov = solve(cov(data))

data = as.matrix(data)


library(matrixStats)
update_inverse_covariance_function = function(data, original_invcov, delete_ind){
  
  #delete_ind = 
  #library(prodlim)
  
  #ind = row.match(delete, data)
  
  #data = data
  #COV = cov(data)
  data = t((t(data) - colMeans2(data)))
  invCOV = original_invcov
  n = nrow(data)+1
  
  #update the deleted row
  for(i in 1:length(delete_ind)){
    data = t((t(data) - colMeans2(data)))
    xn = (as.matrix(data[delete_ind[i],]))
    data = data[-delete_ind[i],]
    #update the number of the observation
    n = n-1
    c = n/(n-1)/(n-1)
    invCOV = (n-2)/(n-1)*(invCOV) + c*(n-2)/(n-1)*invCOV%*%xn%*%t(xn)%*%invCOV/as.numeric((1-c*t(xn)%*%invCOV%*%xn))
  }
  return(invCOV)
}

#data = data2000ca

system.time(for(i in 1:100){update_inverse_covariance_function(data, original_invcov,delete_ind)})
system.time(for(i in 1:100){
  solve(cov(data))
  solve(cov(data[-delete_ind,]))})

#################################
library(profvis)
profvis({for(i in 1:100){
  
  data = data
  delete_ind = c(1000,999)
  
  
  
  data = colScale(data, center = T, scale = F)
  invCOV = original_invcov
  n = nrow(data)+1
  
  #update the deleted row
  for(i in 1:length(delete_ind)){
    data = colScale(data, center = T, scale = F)
    xn = (as.matrix(data[delete_ind[i],]))
    data = data[-delete_ind[i],]
    #update the number of the observation
    n = n-1
    c = n/(n-1)/(n-1)
    invCOV = (n-2)/(n-1)*(invCOV) + c*(n-2)/(n-1)*invCOV%*%xn%*%t(xn)%*%invCOV/as.numeric((1-c*t(xn)%*%invCOV%*%xn))
  }
}})
