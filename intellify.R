############################################################################
#################    Multinomial Logistic regression  ######################

data <- read.csv("Cardiotocographic.csv")
str(data)


# factorise the NSP 
data$NSP <- as.factor(data$NSP)



# train and test dataset
id <- sample(2,nrow(data),replace = T,prob = c(.8,.2))
train <- data[id ==1,]
test <- data[id==2,]

# Model Multinomial Logistic Regression
install.packages("nnet")
library(nnet)

train$NSP <- relevel(train$NSP,ref = "1")
model <- multinom(NSP ~.,train)


# prediction of training data

p <- predict(model,train)
tab <- table(p,train$NSP)
sum(diag(tab))/sum(tab)

# prediction of testing set
p1 <- predict(model,test)
tab1 <- table(p1,test$NSP)

sum(diag(tab1))/sum(tab1)


# checking the sensitivity of training set
n <- table(train$NSP)
n/sum(n)

tab/colSums(tab)

# checking the sensitivity of testing set
n1 <- table(test$NSP)
n1/sum(n1)
tab1/colSums(tab1)






























