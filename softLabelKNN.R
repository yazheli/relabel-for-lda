library(FNN)
library(caret)

#train = synth.te[,c(1,2)]
#label = synth.te[,3]+1

#cl = runif(nrow(train),0,0.5)
#cl = cbind(cl,runif(nrow(train),0,0.5))
#cl = cbind(cl, 1-rowSums(cl))
#colnames(cl) = c(1,2,3)


#a <- FNN::get.knnx(data = train, query = train, k = bestk)
#cl = as.matrix(cl)
#index = a$nn.index
#prob = apply(index, MARGIN = 1, function(x){colMeans(cl[x,])})

softLableKnn = function(train, test, cl, k){
  a <- FNN::get.knnx(data = train, query = test, k = k)
  #cl = as.matrix(cl)
  index = a$nn.index
  prob = apply(index, MARGIN = 1, function(x){colMeans(cl[x,])})
  return(prob)
}

#type = "likelihood" or "errorRate" introduce later
#current only return likelihood

#k = 3

cv.softLableKnn = function(train, cl, label, k, cvNumber){
  fold = caret::createFolds(y = 1:nrow(train), k = cvNumber)
  
  logLikeli = vector()
  
  for(j in 1:cvNumber){
    validation = train[fold[[j]],]
    training = train[-fold[[j]],]
    trainingCl = cl[-fold[[j]],]
    validationLable = label[fold[[j]]]
    validationReturn = softLableKnn(train = training, test = validation, cl = trainingCl, k)
    logLikelihood = vector()
    
    for(i in 1:ncol(validationReturn)){
      logLikelihood = c(logLikelihood,log(validationReturn[validationLable[i],i]+0.000001))
    }
  logLikeli = c(logLikeli,sum(logLikelihood))
  }
  return(logLikeli)
}

#cv.softLableKnn(train, cl, label, k = 60, cvNumber = 5)




