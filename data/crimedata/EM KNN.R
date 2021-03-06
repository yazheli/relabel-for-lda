setwd("~/Desktop/relabel in lda/data/crimedata")
data <- read.table('pop_failures.txt',header = T)
data = data[,-c(1,2)]
data$outcome = !data$outcome

smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

table(data$outcome)




library(caret)
train$outcome = as.factor(train$outcome)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)
library(doParallel)
library(parallel)
library(doMC)
registerDoMC(8)
knnFit <- train(outcome ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale")
                , tuneGrid = expand.grid(k = c(1:100)))

#Output of kNN fit
plot(knnFit)
#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was k = 55. 
plot(knnFit)
knnFit$results[knnFit$results$k == as.numeric(knnFit$bestTune),]$Accuracy
#0.9329692


knnPredict <- predict(knnFit,newdata = test,type="prob" )
y_value = c(test$outcome == 0)
acc = 1 - mean(c(knnPredict[,1]>0.5)!=y_value) ;acc
#0.9185185

####EM
setwd("~/Desktop/relabel in lda/data/crimedata")
data <- read.table('pop_failures.txt',header = T)
data = data[,-c(1,2)]
data$outcome = !data$outcome

smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

table(data$outcome)


library(caret)
train$outcome = as.factor(train$outcome)

library(MASS)
library(caret)
library(parallel)
library(doMC)
library(caret)

y = train[which(train$outcome=='TRUE'),]   
nony = train[which(train$outcome=="FALSE"),]   
train = rbind(y,nony)

weight_c1 = runif(nrow(y), 0.45, 0.55)
weight_c2 = 1 - weight_c1


####initial M step
nony$outcome = rep('c0', nrow(nony))
y$outcome = rep('c1', nrow(y))
y$outcome[weight_c2>0.5] = 'c2'

new_data = rbind(nony,y)

new_data$outcome = as.factor(new_data$outcome)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10,classProbs=TRUE) 
registerDoMC(8)
knnFit <- train(outcome ~ ., data = new_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(1:100)))  


phi_1 = sum(weight_c1)/nrow(y)
phi_2 = sum(weight_c2)/nrow(y)


n_iteration = 2
weight = matrix(0, nrow = n_iteration, ncol = nrow(y))
phi = matrix(0,nrow = n_iteration, ncol = 2)


for(j in 1 : n_iteration){
  #update weight
  
  p1 = predict(knnFit,newdata = y,type="prob" )[,2]
  p2 = predict(knnFit,newdata = y,type="prob" )[,3]
  
  
  new_weight_c1 = phi_1*p1/(phi_2*p2 + phi_1*p1)
  new_weight_c2 = phi_2*p2/(phi_2*p2 + phi_1*p1)
  
  
  
  ####M step solve likelihood function
  y$outcome = rep('c1', nrow(y))
  y$outcome[new_weight_c2>0.5] = 'c2'
  
  new_data = rbind(nony,y)
  
  new_data$outcome = as.factor(new_data$outcome)
  set.seed(400)
  ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10) 
  registerDoMC(8)
  knnFit <- train(outcome ~ ., data = new_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(1:100)))  
  
  
  phi_1 = mean(new_weight_c1,na.rm = T)
  phi_2 = mean(new_weight_c2,na.rm = T)
  
  
  phi[j,] = c(phi_1,phi_2)
  weight[j,] = new_weight_c2
  
}

plot(knnFit)

matplot(weight, type = 'l')

knnPredict <- predict(knnFit,newdata = test,type="prob" )
y_value = c(test$outcome == 0)
acc = 1 - mean(c(knnPredict[,1]>0.5)!=y_value) ;acc
#0.9185185
