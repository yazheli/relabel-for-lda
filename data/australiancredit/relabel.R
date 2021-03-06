setwd("~/Desktop/relabel in lda/data/australiancredit")
data <- read.table('australian.txt',header = F)

names(data)[15] = 'outcome'
table(data$outcome)

smp_size <- floor(0.75 * nrow(data))
set.seed(120)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


library(MASS)
train.qda <- lda(as.formula(outcome~.), data = train)
result = predict(train.qda, newdata = train)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = train$outcome
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
#0.8510638

result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = test$outcome
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
#0.8843931



train_generate = function(x){
  default = train[which(train$outcome == 1),]
  non_default = train[which(train$outcome == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(-15)]
  
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
    train.lda <- lda(as.formula(class~.), data = mldata)
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
train.lda <- lda(as.formula(outcome~.), data = relabel)
result = predict(train.lda, newdata = test)$posterior
predict_label_c1 = (result[,2]+result[,3])>result[,1]
y_value = test$outcome
er = Error.rate(Fit = predict_label_c1, Y=y_value)
accracy = 1-er

#0.8843931
