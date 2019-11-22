# Read data file
mydata <- read.csv(file.choose(), header = T)
summary(mydata)
str(mydata)
mydata$NSPF <- as.factor(mydata$NSP)
str(mydata)

#partation 
set.seed(1234)
ind<- sample(2,nrow(mydata),replace=TRUE,prob = c(0.7,0.3))
train <- mydata[ind==1,]
test <-  mydata[ind==2,]

# Decision tree with party
library(party)
mytree <- ctree(NSPF~LB+AC+FM, mydata, controls=ctree_control(mincriterion=0.99, minsplit=250))
plot(mytree,type="simple")

##predict the value for train 
predict(mytree,train, type="prob")

##predict the value for test 
predict(mytree,test, type="prob")


# Misclassification error
tab <- table(predict(mytree), mydata$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)


#accuracypercent
accuracy<-sum(diag(tab))/sum(tab)
acc<-accuracy *100
acc
