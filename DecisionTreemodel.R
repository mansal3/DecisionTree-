library(readxl)
predictdatatransformer <- read_excel("C:/Users/Manpreet.saluja/Desktop/predictdatatransformer.xlsx")
View(predictdatatransformer)

#structure of data
str(predictdatatransformer)
predictdatatransformer$`Oil Leakage`<-as.factor(predictdatatransformer$`Oil Leakage`)
predictdatatransformer$`Fuel supply`<-as.factor(predictdatatransformer$`Fuel supply`)
predictdatatransformer$`Vector Group`<-as.factor(predictdatatransformer$`Vector Group`)
predictdatatransformer$Insulation<-as.factor(predictdatatransformer$Insulation)
predictdatatransformer$`Energy Losses`<-as.factor(predictdatatransformer$`Energy Losses`)
predictdatatransformer$`Pressure Relay`<-as.factor(predictdatatransformer$`Pressure Relay`)
predictdatatransformer$Core<-as.factor(predictdatatransformer$Core)
predictdatatransformer$Bushing<-as.factor(predictdatatransformer$Bushing)
predictdatatransformer$`Over Current Protection (OC)`<-as.factor(predictdatatransformer$`Over Current Protection (OC)`)
predictdatatransformer$`Fire Fighting Systems (FFS)`<-as.factor(predictdatatransformer$`Fire Fighting Systems (FFS)`)
predictdatatransformer$`Silica Gel Color`<-as.factor(predictdatatransformer$`Silica Gel Color`)
predictdatatransformer$Outage<-as.factor(predictdatatransformer$Outage)



attach(predictdatatransformer)

#EDA..
head(predictdatatransformer)
tail(predictdatatransformer)
summary(predictdatatransformer)
hist(predictdatatransformer$`Tap Changer`,prob = T)
library(car)
qqnorm(predictdatatransformer$`Tap Changer`)
qqline(predictdatatransformer$`Tap Changer`)
boxplot(predictdatatransformer$`Lamination thickness maintainence`)
boxplot(predictdatatransformer)
abline(predictdatatransformer)
apply(predictdatatransformer,2,function(x){sum(is.na(x))})
dim(predictdatatransformer)
library(DataExplorer)
plot_str(predictdatatransformer)
plot_missing(predictdatatransformer)
plot_histogram(predictdatatransformer)
plot_density(predictdatatransformer)
plot_correlation(predictdatatransformer)
plot_qq(predictdatatransformer)

library(PerformanceAnalytics)
chart.Correlation(predictdatatransformer, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)
plot_bar(predictdatatransformer)
create_report(predictdatatransformer)

#create training and validation data from given data
#install.packages('caTools')
library(caTools)
set.seed(88)
split <- sample.split(predictdatatransformer$Outage, SplitRatio = 0.75)

#get training and test data
train <- subset(predictdatatransformer, split == TRUE)
test <- subset(predictdatatransformer, split == FALSE)
library(rpart)
fit <- rpart(factor(Outage) ~ `Lamination thickness maintainence`+factor(Bushing)+factor(`Over Current Protection (OC)`)+factor(`Fire Fighting Systems (FFS)`)+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+factor(`Silica Gel Color`)+factor(`Tap Changer`)+`Percentage Impedance (at 75°C)`,
             method="class", data=train)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Outage")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree 
# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Outage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#predict
predict(fit, train, type = 'class')
predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$Outage, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test, type = 'class')
  table_mat <- table(test$Outage, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
for(i in 1:100)
{
control <- rpart.control(minsplit = i/i+i,
                         minbucket = round(i),
                         maxdepth = round(i/i+1),
                         cp = i)
tune_fit <- rpart(Outage~., data = train, method = 'class', control = control)
acc<-accuracy_tune(tune_fit)
print(i)
print(acc)
}

