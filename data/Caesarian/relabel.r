setwd("~/Desktop/relabel in lda/data/Caesarian")
library(MASS)
data = read.csv(file="caesarian.csv",header=TRUE,sep=",")

smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

train.qda <- lda(as.formula(caesarian~.), data = train)
result = predict(train.qda, newdata = test)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = test$caesarian
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
table(data$caesarian)
#train = data

train_generate = function(x){
  default = train[which(train$caesarian == 1),]
  non_default = train[which(train$caesarian == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(-6)]
  
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

chromosome = sample(c(0,1), size = sum(train$caesarian),replace = TRUE)
x = chromosome
evalFunc(chromosome)
library(wSVM)
library(GA)
library(memoise)

mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = nrow(train[which(train$caesarian == 1),]), popSize = 100, maxiter = 1000, seed = 1, monitor = FALSE, parallel = 8)
forget(mfitness)

summary(GA)
plot(GA)

