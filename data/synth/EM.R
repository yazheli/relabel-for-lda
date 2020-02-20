library(MASS)
library(caret)
library(parallel)
library(doMC)

train = synth.te
test = synth.tr

#auc_2 = matrix(0,nrow = 2, ncol = 1)

y = train[which(train$yc==1),]   
nony = train[which(train$yc==0),]   
train = rbind(y,nony)

new_weight_c1 = runif(nrow(y), 0.45, 0.55)
new_weight_c2 = 1 - new_weight_c1



####initial M step
nony$yc = rep('c0', nrow(nony))
y$yc = rep('c1', nrow(y))
y$yc[new_weight_c2>0.5] = 'c2'


new_data = rbind(nony,y)

new_data$yc = as.factor(new_data$yc)
trainX <- new_data[,-3]
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10,classProbs=TRUE,summaryFunction = multiClassSummary) 
registerDoMC(8)
knnFit <- train(yc ~ ., data = new_data, method = "knn", trControl = ctrl, metric='logLoss',preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(1:200)))  

#knnFit$results[knnFit$results$k == as.numeric(knnFit$bestTune),]$Accuracy
plot(knnFit)
phi_1 = sum(new_weight_c1)/nrow(y)
phi_2 = sum(new_weight_c2)/nrow(y)
print(knnFit$results[as.numeric(knnFit$bestTune),2])

n_iteration = 10
weight = matrix(0, nrow = n_iteration, ncol = nrow(y))
phi = matrix(0,nrow = n_iteration, ncol = 2)
  

for(j in 1 : n_iteration){
    #update weight
    
    p1 = predict(knnFit,newdata = y,type="prob" )[,2]/sum(new_weight_c1)
    p2 = predict(knnFit,newdata = y,type="prob" )[,3]/sum(new_weight_c2)
    
    
    new_weight_c1 = phi_1*p1/(phi_2*p2 + phi_1*p1)
    new_weight_c2 = phi_2*p2/(phi_2*p2 + phi_1*p1)
    
    
    
    ####M step solve likelihood function
    y$yc = rep('c1', nrow(y))
    y$yc[new_weight_c2>0.5] = 'c2'
    
    new_data = rbind(nony,y)
    
    new_data$yc = as.factor(new_data$yc)
    trainX <- new_data[,-3]
    set.seed(400)
    ctrl <- trainControl(method="repeatedcv",repeats = 5,number = 10,classProbs=TRUE,summaryFunction = multiClassSummary) 
    registerDoMC(8)
    knnFit <- train(yc ~ ., data = new_data, method = "knn", trControl = ctrl, metric='logLoss', preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(1:200)))  
    
    phi_1 = mean(new_weight_c1,na.rm = T)
    phi_2 = mean(new_weight_c2,na.rm = T)
    
    print(knnFit$results[as.numeric(knnFit$bestTune),2])
    phi[j,] = c(phi_1,phi_2)
    weight[j,] = new_weight_c2
    
}

plot(train[,c(1:2)])
points(y[y$yc == 'c1',c(1:2)],col = 'red')
points(y[y$yc == 'c2',c(1:2)],col = 'blue')

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(knnFit, newdata = grid,type="prob")
z=matrix(prob.grid[,1], nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="red", drawlabels=FALSE, lwd=4,add = TRUE)
plot(knnFit)
knnFit$bestTune
matplot(phi,type = 'l',ylim = c(0,1))
matplot(weight[1:10,],type = 'l')
