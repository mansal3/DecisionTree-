#import the  data into new variable name data
data<-Cardiotocographic
#checking the structure of data 
str(Cardiotocographic)
#converting the dependent variable into factor variable
data$NSP<-factor(data$NSP)
#to r randomly pick the variable use set.seed function
set.seed(1234)
#divding the data into 2 traning and testing
datasample<-sample(2,nrow(data),replace = TRUE,prob=c(0.8,0.2))
#we are not writing any thing after',' means we want all columns
train<-data[datasample==1,]
test<-data[datasample==2,]
install.packages("rpart")
library("rpart")
attach(data)
decisiontreemodel<-rpart(NSP~.,data=train)
decisiontreemodel
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(decisiontreemodel,extra=1)
p<-predict(decisiontreemodel,test,type="prob")
tab<-table(p,test$NSP)
          
           