library(caret)
library(mlbench)
library(Regression)
data(BostonHousing)
set.seed(506)

ind<- createDataPartition(BostonHousing$medv, p = .75, 
                          list = FALSE, 
                          times = 1)
tra_data<-BostonHousing[ind,]      #split the data 75/25%
val_data<-BostonHousing[-ind,]
nrow(tra_data)                    #381 observations on training data
nrow(val_data)                    # 125 observations on test data.


model1 <- train(medv ~ ., data = tra_data, method = "leapForward")
prediction1<- predict(model1,val_data)  #prediction of the medv on the unseen data using model1
plot1<-plot(model1)                     #plot of the model in the training.
plot2<-plot(prediction1)                #plot of prediction using model1.

#fitting a model using our function with lambda =0 and using the bostonhousing data

our_model<-ridgereg(medv~.,data = tra_data,lambda = 0)
our_predic1<- our_model$rid_fitted
print(our_predic1)


#10 fold cross validation
train_control <- trainControl(method="cv", number=10)
model2 <- train(medv ~ ., data = tra_data, trControl=train_control, method = "leapForward")
plot(model2)
prediction2 <- predict(model2,val_data )
plot(prediction2)
 
 

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

fit_ridgereg <- train(medv~ .,data= tra_data, method="ridge", trControl = fitControl,
                  tuneGrid= expand.grid(lambda=seq(0,8,.5)) ) #http://finzi.psych.upenn.edu/library/AppliedPredictiveModeling/chapters/06_Linear_Regression.R

print(fit_ridgereg)
ggplot(fit_ridgereg)