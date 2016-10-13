library(caret)
library(mlbench)
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

#10 fold cross validation
train_control <- trainControl(method="cv", number=10)
model2 <- train(medv ~ ., data = tra_data, trControl=train_control, method = "leapForward")
plot(model2)
prediction2 <- predict(model2,val_data )
plot(prediction2)
 
 