# caret

install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
library("data.table") 

## primary data
train_data = as.data.frame(fread("train.csv"))[,-1]
test_data <- as.data.frame(fread("test.csv"))[,-1]

## training data ####
train_data$target = factor(train_data$target) # needs to be factor

# inTrain <- createDataPartition(y = train_data$target,
#                                  ## the outcome data are needed
#                                 p = .75,
#                                 ## The percentage of data in the
#                                 ## training set
#                                 list = FALSE)
# str(inTrain)
# training <- train_data[ inTrain,]
# testing <- test_data[-inTrain,]  # this is differenct from test dat for submisison

# limit to 1000
set.seed(0)
k = sample(1:nrow(train_data), 1000)
training_1k = train_data[k,]

y = which(colnames(train_data) %in% "target")

## rf model ####

cvCtrl = trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)

### no preprosseseing
rf_model<-train( training_1k[, -y], training_1k[, y], 
                 method="rf",
                 trControl = cvCtrl,
                 allowParallel=TRUE, tuneLength = 3)
print(rf_model)
print(rf_model$finalModel)


# pls train model ####
plsFit <- train(target ~ .,
                + data = training,
                + method = "pls",
                + tuneLength = 15,
                + trControl = ctrl,
                + preProc = c("center", "scale"))

## select tuned model
ctrl <- trainControl(method = "repeatedcv",
                     + repeats = 3,
                     + classProbs = TRUE,
                     + summaryFunction = twoClassSummary)
plsFit <- train(Class ~ .,
                  + data = training,
                  + method = "pls",
                  + tuneLength = 15,
                  + trControl = ctrl,
                  + metric = "ROC",
                  + preProc = c("center", "scale"))
plsFit
plot(plsFit)  # visualize tuning

## prediction
plsProbs <- predict(plsFit, newdata = testing, type = "prob")

## confusion matrix
confusionMatrix(data = plsClasses, testing$Class)



