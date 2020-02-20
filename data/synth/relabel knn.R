library(MASS)
train = synth.te
test = synth.tr

#functions: names() mean() colMeans() var()
#dataframe: $, extract column/row, add column/row 
#data type: factor numric character ...
#plot: density, hist, scatter plot, line plot



library(class)

prc_test_pred <- knn(train = train[,-3], test = test[,-3], cl = train$yc, k=64)   
y_value = test$yc
accracy = 1 - mean(prc_test_pred!=y_value) 
accracy
#0.864

plot(train[,c(1:2)])
points(train[train$yc == 1,c(1:2)],col = 'red')

library(caret)
train$yc = as.factor(train$yc)
trainX <- train[,-3]
preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
preProcValues

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)

#library(doParallel)
library(parallel)
library(doMC)
registerDoMC(8)
knnFit <- train(yc ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale")
                , tuneGrid = expand.grid(k = c(1:100)))

#Output of kNN fit
knnFit
#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was k = 55. 
plot(knnFit)
knnFit$results[knnFit$results$k == as.numeric(knnFit$bestTune),]$Accuracy
#0.9172

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(knnFit, newdata = grid,type="prob")
z=matrix(prob.grid[,1], nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="red", drawlabels=FALSE, lwd=4,add = TRUE)


knnPredict <- predict(knnFit,newdata = test,type="prob" )
y_value = c(test$yc == 0)
acc = 1 - mean(c(knnPredict[,1]>0.5)!=y_value) 
#acc
#0.86

#relabel for knn
#ga relabel the red class, use knnFit search best k and coresponding error rate
library(MASS)
train = synth.te
test = synth.tr
clusters <- kmeans(train[train$yc == 1,1:2], 2)
#train[train$yc == 1,]$yc =  clusters$cluster

#train$yc = as.factor(train$yc)
#trainX <- train[,-3]
#library(caret)
#preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
#preProcValues

#set.seed(400)
#ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)

#library(doParallel)
#library(parallel)
#library(doMC)
#registerDoMC(8)
knnFit <- train(yc ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale")
                , tuneGrid = expand.grid(k = c(1:300)))

#Output of kNN fit
#knnFit
#k=26
#plot(knnFit)
#knnFit$results[knnFit$results$k == as.numeric(knnFit$bestTune),]$Accuracy
#0.917222

#knnPredict <- predict(knnFit,newdata = test,type="prob" )
#grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

#prob.grid <-  predict(knnFit, newdata = grid,type="prob")
#z=matrix(prob.grid[,1], nrow=length(seq(-1.1,1.1,0.01)))

#plot(train[,c(1:2)])
#points(train[train$yc == 1,c(1:2)],col = 'red')
#points(train[train$yc == 2,c(1:2)],col = 'blue')

# plot the boundary
#contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
#        col="red", drawlabels=FALSE, lwd=4,add = TRUE)


#predict_label_c1 <- c(predict(knnFit,newdata = train,type="prob" )[,1]>0.5)
#y_value = c(train$yc == 0)
#acc = 1 - mean(predict_label_c1!=y_value) 
#0.922

#predict_label_c1 <- c(predict(knnFit,newdata = test,type="prob" )[,1]>0.5)
#y_value = c(test$yc == 0)
#acc = 1 - mean(predict_label_c1!=y_value) 
#0.864

train_generate = function(x){
  default = train[which(train$yc == 1),]
  non_default = train[which(train$yc == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(1,2)]
  colnames(new.data) = c("x1","x2")
  
  c = rep("c2",nrow(default))
  c[which(x == 1)] = "c3"
  new.data$class = c(rep("c1",nrow(non_default)), c)
  return(new.data)
}



evalFunc <- function(chromosome){
  if(sum(chromosome) == 0 | sum(chromosome) == length(chromosome)){
    return(0)
  } else {
    mldata = train_generate(chromosome)
    #f <- as.formula(paste("class ~ 1|", paste(names(mldata)[c(1,2)], collapse = " + ")))
    #train$yc = as.factor(train$yc)
    #trainX <- train[,-3]
    #library(caret)
    #preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
    #preProcValues
    set.seed(400)
    ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #library(doParallel)
    library(parallel)
    library(doMC)
    registerDoMC(8)
    knnFit <- train(class ~ ., data = mldata, method = "knn", trControl = ctrl, preProcess = c("center","scale")
                    , tuneGrid = expand.grid(k = c(1:50)))
    
    result = predict(knnFit,newdata = mldata,type="prob" )
    results = result[,2]+result[,3]
    predict_label_c1 = c(result[,1]>results)
    y_value = c(mldata$class == 'c1')
    er = Error.rate(Fit = predict_label_c1, Y=y_value)
    accracy = 1-er
    return(accracy)
  }
}

clusters <- kmeans(train[train$yc == 1,1:2], 2)
sol = clusters$cluster
c1 = train[which(train$yc == 1),][which(sol == 1),]
c2 = train[which(train$yc == 1),][which(sol == 2),]
plot(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')

#chromosome = sample(c(0,1), size = 500,replace = TRUE)
chromosome = c(sol-1)
suggestion = chromosome
pr = table(suggestion)/length(suggestion)
result <- t(sapply(1:99, function(x) {sample(c(0, 1), size = length(suggestion), replace = TRUE, prob = pr)}))
suggestion = rbind(suggestion, result)
library(wSVM)
evalFunc(suggestion[1,])
#nrow(train[which(train$yc == 1),])

library(GA)
library(memoise)

mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = nrow(train[which(train$yc == 1),]), popSize = 100, maxiter = 100, seed = 1, monitor = FALSE, parallel = 8, suggestions = suggestion)
forget(mfitness)

summary(GA)
plot(GA)
s = GA@summary

sol = GA@solution[1,]
c1 = train[which(train$yc == 1),][which(sol == 1),]
c2 = train[which(train$yc == 1),][which(sol == 0),]
plot(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')


non_default = train[which(train$yc == 0),]
relabel = rbind(non_default,c1,c2)
relabel$yc = c(rep('c1',nrow(non_default)),rep('c2',nrow(c1)),rep('c3',nrow(c2)))

train.lda <- lda(as.formula(yc~xs+ys), data = relabel)
result = predict(train.lda, newdata = test)$posterior
predict_label_c1 = (result[,2]+result[,3])>result[,1]
y_value = test$yc
er = Error.rate(Fit = predict_label_c1, Y=y_value)
accracy = 1-er



library(mixtools)
gm<-mvnormalmixEM(default[,1:2], k=3)
label = apply(gm$posterior, 1, which.max)

plot(train[,c(1:2)])
points(default[label == 1,c(1:2)],col = 'red')
points(default[label == 2,c(1:2)],col = 'green')
points(default[label == 3,c(1:2)],col = 'blue')


default$yc = label
relabel = rbind(train[train$yc == 0,],default)

train.qda <- qda(as.formula(yc~xs+ys), data = relabel)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = (1 - result[,1])>result[,1]
y_value = test$yc
acc = 1 - mean(predict_label_c1!=y_value) 
acc

plot(train[,c(1:2)])
points(train[train$yc == 1,c(1:2)],col = 'red')

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(train.qda, newdata = grid)$posterior[,1]
z=matrix(prob.grid, nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="black", drawlabels=FALSE, lwd=4,add = TRUE)


