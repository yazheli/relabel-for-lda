data = read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv",header = F)
table(data$V9)

names(data)[9] = 'outcome'
#table(data$outcome)

smp_size <- floor(0.75 * nrow(data))
set.seed(113)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

table(data$outcome)

library(MASS)
train.lda <- qda(as.formula(outcome~.), data = train)
result = predict(train.lda, newdata = train)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = train$outcome
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy

result = predict(train.lda, newdata = test)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = test$outcome
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy





train_generate = function(x){
  default = train[which(train$outcome == 1),]
  non_default = train[which(train$outcome == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(-9)]
  
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
    train.lda <- qda(as.formula(class~.), data = mldata)
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

chromosome = sample(c(0,1), size = sum(train$outcome),replace = TRUE)
x = chromosome
#train = data
library(wSVM)
evalFunc(chromosome)

library(GA)
library(memoise)

mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = nrow(train[which(train$outcome == 1),]), popSize = 100, maxiter = 1000, seed = 1, monitor = FALSE, parallel = 8)
forget(mfitness)

summary(GA)
plot(GA)
sol = GA@solution[1,]
c1 = train[which(train$outcome == 1),][which(sol == 1),]
c2 = train[which(train$outcome == 1),][which(sol == 0),]
non_default = train[which(train$outcome == 0),]
relabel = rbind(non_default,c1,c2)
relabel$outcome = c(rep('c1',nrow(non_default)),rep('c2',nrow(c1)),rep('c3',nrow(c2)))
train.lda <- qda(as.formula(outcome~.), data = relabel)
result = predict(train.lda, newdata = test)$posterior
predict_label_c1 = (result[,2]+result[,3])>result[,1]
y_value = test$outcome
er = Error.rate(Fit = predict_label_c1, Y=y_value)
accracy = 1-er
accracy
#0.7395833


#######################
l = length(decimal2binary(2))

Integer2Gray = function(x){
  sapply(x, function(x) {binary2gray(decimal2binary(x,l))})
}


Gray2Integer = function(x){
  binary2decimal(gray2binary(x))
}

i = sample(c(0,1,2),size = sum(train$outcome),replace = TRUE)
library(GA)
chromosome = as.vector(Integer2Gray(i))
#chromosome

train_generate = function(x){
  default = train[which(train$outcome == 1),]
  non_default = train[which(train$outcome == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(-9)]
  
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
    train.lda <- qda(as.formula(class~.), data = mldata)
    
    result = predict(train.lda, newdata = mldata)$posterior
    results = 1 - result[,1]
    predict_label_c1 = result[,1]>results
    y_value = mldata$class == 'c1'
    er = Error.rate(Fit = predict_label_c1, Y=y_value)
    accracy = 1-er
    return(accracy)
    
  }
}

i = sample(c(0,1,2),size = sum(train$outcome),replace = TRUE)
library(GA)
chromosome = as.vector(Integer2Gray(i))
#chromosome
evalFunc(chromosome)



library(GA)
library(memoise)
i = sample(c(0,1,2),size = sum(train$outcome),replace = TRUE)
suggestion = t(as.matrix(i))
pr = table(suggestion)/length(suggestion)
result <- t(sapply(1:99, function(x) {sample(c(0, 1, 2), size = length(suggestion), replace = TRUE, prob = pr)}))
suggestion = rbind(suggestion, result)
suggestion = t(apply(suggestion, 1, function(x) as.vector(Integer2Gray(x))))
evalFunc(suggestion[1,])



mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = l*nrow(train[which(train$outcome == 1),]), popSize = 100, maxiter = 1000, seed = 1, monitor = FALSE, parallel = 7,suggestions = suggestion)
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
#accracy,0.734375

library(mixtools)
default = train[train$outcome == 1,]
gm<-mvnormalmixEM(default[,-9], k=2)
label = apply(gm$posterior, 1, which.max)
table(label)

default$outcome = label
relabel = rbind(train[train$outcome == 0,],default)

train.qda <- qda(as.formula(outcome~.), data = relabel)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = (1 - result[,1])>result[,1]
y_value = test$outcome
acc = 1 - mean(predict_label_c1!=y_value) 
acc
#0.7239583

#k=3 not work
library(mixtools)
default = train[train$outcome == 1,]
gm<-mvnormalmixEM(default[,-9], k=3)
label = apply(gm$posterior, 1, which.max)
table(label)

default$outcome = label
relabel = rbind(train[train$outcome == 0,],default)

train.qda <- qda(as.formula(outcome~.), data = relabel)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = (1 - result[,1])>result[,1]
y_value = test$outcome
acc = 1 - mean(predict_label_c1!=y_value) 
acc

