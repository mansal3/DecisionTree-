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
#decision tree with party
install.packages("party")
library("party")
attach(data)
#decision tree model here control :minsplit means tree will split the node only if dataset size is 500 and mincriterion is the confidence interval 99%
decisiontreemodel<-ctree(NSP~.,data = train,controls=ctree_control(minsplit = 500,mincriterion = 0.99))
decisiontreemodel
plot(decisiontreemodel)
#hence we have trained are model now we will predict the value using it 
predict(decisiontreemodel,test,type="prob")
