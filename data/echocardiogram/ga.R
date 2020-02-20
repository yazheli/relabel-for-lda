setwd("~/Desktop/relabel in lda/data/echocardiogram")

my_data <- read.delim('echocardiogramdata.txt',header = T, sep = ",")
my_data = my_data[as.numeric(as.character(my_data$survival))>11,]

#my_data = my_data[,c(3:7,9,13)]
#table(my_data$alive.at.1)
my_data = my_data[,c(2:7,9)]
my_data = my_data[complete.cases(my_data), ]

index = unique(unlist(apply(X = my_data, MARGIN = 2, FUN = function(x){which(x == "?")})))
my_data = my_data[-index,]

for(i in 1:7){
  my_data[,i] = as.numeric(as.character(my_data[,i]))
}

library(MASS)
train.qda <- lda(as.formula(still.alive~.), data = my_data)
result = predict(train.qda, newdata = my_data)$posterior
predict_label_c1 = result[,2]>result[,1]
y_value = my_data$still.alive
accracy = 1 - mean(predict_label_c1!=y_value) 
accracy
#0.8860759


train_generate = function(x){
  default = train[which(train$still.alive == 1),]
  non_default = train[which(train$still.alive == 0),]
  
  new.data = as.data.frame(rbind(non_default,default))
  new.data = new.data[,c(-1)]
  
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

train = my_data
chromosome = sample(c(0,1), size = sum(train$still.alive),replace = TRUE)
x = chromosome
#train = data
library(wSVM)
evalFunc(chromosome)

library(GA)
library(memoise)

mfitness <- memoise(evalFunc)
GA = ga("binary", fitness = mfitness, nBits = nrow(train[which(train$still.alive == 1),]), popSize = 10, maxiter = 100, seed = 1, monitor = FALSE, parallel = 8)
forget(mfitness)

summary(GA)
plot(GA)
sol = GA@solution[1,]
c1 = train[which(train$still.alive == 1),][which(sol == 1),]
c2 = train[which(train$still.alive == 1),][which(sol == 0),]
non_default = train[which(train$still.alive == 0),]
relabel = rbind(non_default,c1,c2)
relabel$still.alive = c(rep('c1',nrow(non_default)),rep('c2',nrow(c1)),rep('c3',nrow(c2)))
train.lda <- lda(as.formula(still.alive~.), data = relabel)
result = predict(train.lda, newdata = train)$posterior
predict_label_c1 = (result[,2]+result[,3])>result[,1]
y_value = train$still.alive
er = Error.rate(Fit = predict_label_c1, Y=y_value)
accracy = 1-er
#0.9240506






