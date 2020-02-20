library(MASS)
train = synth.te
test = synth.tr


plot(train[,c(1:2)])
points(train[train$yc == 1,c(1:2)],col = 'red')
###############################################
#do a artifical relabel for lda
threshold = seq(-0.5,1.0,0.1)
accuracy =rep(0,length(threshold))
 

for(i in 1:length(threshold)){
  default = train[train$yc == 1,]
  default$yc[default$xs>threshold[i]] = 2
  relabel = rbind(train[train$yc == 0,],default)
  
  train.qda <- lda(as.formula(yc~xs+ys), data = relabel)
  
  result = predict(train.qda, newdata = train)$posterior
  predict_label_c1 = (result[,2]+result[,3])>result[,1]
  y_value = train$yc
  acc = 1 - mean(predict_label_c1!=y_value) 
  accuracy[i] = acc
}
accuracy
default = train[train$yc == 1,]
default$yc[default$xs>-0.5] = 2
relabel = rbind(train[train$yc == 0,],default)

train.qda <- lda(as.formula(yc~xs+ys), data = relabel)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = (result[,2]+result[,3])>result[,1]
y_value = test$yc
acc = 1 - mean(predict_label_c1!=y_value) 

plot(train[,c(1:2)])
points(train[train$yc == 1,c(1:2)],col = 'red')

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(train.qda, newdata = grid)$posterior[,1]
z=matrix(prob.grid, nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="black", drawlabels=FALSE, lwd=4,add = TRUE)


###############################################



train.qda <- lda(as.formula(yc~xs+ys), data = train)
result = predict(train.qda, newdata = train)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = train$yc
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
#accracy,0.9
grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(train.qda, newdata = grid)$posterior[,1]
z=matrix(prob.grid, nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="blue", drawlabels=FALSE, lwd=4,add = TRUE)

result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = test$yc
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
#accracy,0.844
#plot(test[,c(1:2)])
#points(test[test$yc == 1,c(1:2)],col = 'red')

#train.lda <- lda(as.formula(yc~xs+ys), data = train)
#lda.predict <- predict(train.lda, newdata = test)

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
    train.lda <- qda(as.formula(class~x1+x2), data = mldata)
    #mlogit.model<-mlogit(class ~ 1 | x1 + x2 , data = mldata, reflevel="c1")
    
    result = predict(train.lda, newdata = mldata)$posterior
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
GA = ga("binary", fitness = mfitness, nBits = nrow(train[which(train$yc == 1),]), popSize = 100, maxiter = 50000, seed = 1, monitor = FALSE, parallel = 8, suggestions = suggestion)
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
#accracy,0.856


grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(train.lda, newdata = grid)$posterior[,1]
z=matrix(prob.grid, nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="blue", drawlabels=FALSE, lwd=4,add = T)
# add points from test dataset
points(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')





#try three group

library(MASS)
train = synth.te
test = synth.tr


l = length(decimal2binary(2))

Integer2Gray = function(x){
  sapply(x, function(x) {binary2gray(decimal2binary(x,l))})
}


Gray2Integer = function(x){
  binary2decimal(gray2binary(x))
}

i = sample(c(0,1,2),size = 500,replace = TRUE)
library(GA)
chromosome = as.vector(Integer2Gray(i))
#chromosome

train_generate = function(x){
  default = train[which(train$yc == 1),]
  non_default = train[which(train$yc == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(1,2)]
  colnames(new.data) = c("x1","x2")
  
  c = rep("c2",nrow(default))
  c[which(x == 1)] = "c3"
  c[which(x == 2)] = "c4"
  new.data$class = c(rep("c1",nrow(non_default)), c)
  return(new.data)
}

evalFunc <- function(chromosome){
  l = length(decimal2binary(2))
  element = matrix(chromosome,l)
  class = apply(element, 2, Gray2Integer)
  
  if(length(unique(class)) < 3 || max(class) > 2){
    return(0.5)
  } else {
    mldata = train_generate(class)
    train.lda <- qda(as.formula(class~x1+x2), data = mldata)
    #mlogit.model<-mlogit(class ~ 1 | x1 + x2 , data = mldata, reflevel="c1")
    
    result = predict(train.lda, newdata = mldata)$posterior
    results = 1 - result[,1]
    predict_label_c1 = result[,1]>results
    y_value = mldata$class == 'c1'
    er = Error.rate(Fit = predict_label_c1, Y=y_value)
    accracy = 1-er
    return(accracy)
    
  }
}

i = sample(c(0,1,2),size = 500,replace = TRUE)
library(GA)
chromosome = as.vector(Integer2Gray(i))
#chromosome
evalFunc(chromosome)

c1 = train[which(train$yc == 1),][which(i == 0),]
c2 = train[which(train$yc == 1),][which(i == 1),]
c3 = train[which(train$yc == 1),][which(i == 2),]
plot(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')
points(c3[c(1:2)],col = 'green')

library(GA)
library(memoise)
i=sol-1
suggestion = t(as.matrix(i))
pr = table(suggestion)/length(suggestion)
result <- t(sapply(1:99, function(x) {sample(c(0, 1, 2), size = length(suggestion), replace = TRUE, prob = pr)}))
suggestion = rbind(suggestion, result)
suggestion = t(apply(suggestion, 1, function(x) as.vector(Integer2Gray(x))))
evalFunc(suggestion[1,])



mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = l*nrow(train[which(train$yc == 1),]), popSize = 1000, maxiter = 100, seed = 1, monitor = FALSE, parallel = 7,suggestions = suggestion)
forget(mfitness)

summary(GA)
plot(GA)

sol = GA@solution[1,]
element = matrix(sol,l)
class = apply(element, 2, Gray2Integer)


clusters <- kmeans(train[train$yc == 1,1:2], 3)
sol = clusters$cluster
c1 = train[which(train$yc == 1),][which(sol == 1),]
c2 = train[which(train$yc == 1),][which(sol == 2),]
c3 = train[which(train$yc == 1),][which(sol == 3),]

c1 = train[which(train$yc == 1),][which(class == 0),]
c2 = train[which(train$yc == 1),][which(class == 1),]
c3 = train[which(train$yc == 1),][which(class == 2),]
plot(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')
points(c3[c(1:2)],col = 'green')

non_default = train[which(train$yc == 0),]
relabel = rbind(non_default,c1,c2,c3)
relabel$yc = c(rep('c1',nrow(non_default)),rep('c2',nrow(c1)),rep('c3',nrow(c2)),rep('c4',nrow(c3)))

train.qda <- qda(as.formula(yc~xs+ys), data = relabel)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = (1-result[,1])>result[,1]
y_value = test$yc
er = Error.rate(Fit = predict_label_c1, Y=y_value)
accracy = 1-er
#accracy,0.86

grid <- expand.grid(xs=seq(-1.1,1.1,0.01), ys=seq(-0.2,1.2,0.01))

prob.grid <-  predict(train.qda, newdata = grid)$posterior[,1]
z=matrix(prob.grid, nrow=length(seq(-1.1,1.1,0.01)))

# plot the boundary
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="blue", drawlabels=FALSE, lwd=4,add = TRUE)
# add points from test dataset
points(train[,c(1:2)])
points(c1[c(1:2)],col = 'red')
points(c2[c(1:2)],col = 'blue')
contour(x=seq(-1.1,1.1,0.01), y=seq(-0.2,1.2,0.01), z = z, levels=0.5,
        col="black", drawlabels=FALSE, lwd=4,add = TRUE)
