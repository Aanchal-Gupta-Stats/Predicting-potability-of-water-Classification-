#Installing libraries
install.packages("ggplot2")
install.packages("corrplot")
install.packages("randomForest"")
install.packages("caret"")
library(ggplot2)
library(corrplot)
library(randomForest)
library(caret)


#Importing and understanding data
data<- read.csv(file.choose())
attach(data) 
str(data)
class(data$Potability)
data$Potability<-as.factor(data$Potability)
summary(data)

#Missing values analysis
name<-names(data)
misscount<- c(1:10)
names(misscount)<-name
for(i in 1:10) {
  misscount[i]<-sum(is.na(data[,i]))
  }
misscount[misscount!=0]

missprop<-c(1:10)
names(missprop)<-name
for(i in 1:10) {
  missprop[i]<-misscount[i]*100/ nrow(data)
  }
missprop[missprop!=0]

#Replacing missing values with mean
newdata<-data
for(j in 1:ncol(newdata)) {
  for(i in 1:nrow(newdata)) {
    if(is.na(newdata[i,j])==1){
      newdata[i,j]=mean(data[,j],na.rm=TRUE)
      }}}

#Detecting outliers
par(mfrow=c(3,3))
for(i in 1:9) {
  boxplot(newdata[,i]~newdata$potability, col=c("blue","green"),outline=TRUE)
  }

par(mfrow=c(3,3))
for(i in 1:9) {
  hist(newdata[,i],col=c("blue","green"))
  }

#Correlation
corrplot(cor(newdata[,-10]), type="lower", method="circle")
pairs(newdata[,1:9],lower.panel=NULL)

#Random forest model
set.seed(31967)
train_ind<-createDataPartition(newdata$Potability, p = 0.8, list = FALSE)
TrainingSet <- newdata[train_ind, ]
TestSet <- newdata[-train_ind, ]
summary(TrainingSet)
summary(TestSet)

xdf <- TrainingSet[,-10]
ydf <- TrainingSet[,10]
Bestmtry <- tuneRF(xdf,ydf,stepFactor = 1.5,improve = 1e-6,ntree = 1000, plot = F)

set.seed(31967)
RfFinal <- train(potability ~ .,data = TrainingSet,method = "rf",metric = "Accuracy",tuneGrid = expand.grid(.mtry = 4),trControl = control,ntree = 1000)
plot(RfFinal$finalModel)

#Important Variables
VarsImp <- varImp(RfFinal, scale = FALSE)
VarsImp

#Prediction
set.seed(31967)
PredRf <- predict(RfFinal,TestSet, type="raw")
confusionMatrix(data = PredRf, reference = TestSet$Potability, positive = "1")



