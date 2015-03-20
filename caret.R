# caret

# install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
library(lubridate)
library("data.table") 
library(doMC)
registerDoMC(cores = 4)

## primary data ####
train_data = as.data.frame(fread("train.csv"))[,-1]
test_data <- as.data.frame(fread("test.csv"))[,-1]

train_data$target = factor(train_data$target) # needs to be factor

y = which(colnames(train_data) %in% "target")

## training data ####
inTrain <- createDataPartition(y = train_data[,y],
                                 ## the outcome data are needed
                                p = .5,
                                ## The percentage of data in the
                                ## training set
                                list = FALSE)
str(inTrain)
training <- train_data[ inTrain,]
testing <- test_data[-inTrain,]  # this is differenct from test dat for submisison

# complete dataset
# training = train_data

## rf model ####
cvCtrl = trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)
bootCtrl = trainControl(method = "boot", classProbs = TRUE)

run = now()
model_label = paste0("caret_", month(run), day(run), hour(run))

rfGrid <-  expand.grid(mtry = c(5,9,18))

rf_model<-train( training[, -y], training[, y], 
                 method="rf",
                 trControl = cvCtrl,
                 allowParallel=TRUE, 
                 tuneGrid = rfGrid)
save(rf_model, file = "rf_model.rda")
print(rf_model)
print(rf_model$finalModel)

submit = as.data.frame(fread("sampleSubmission.csv"))
submit[, 2:10] <- predict(rf_model, newdata=test_data, type='prob')
write.csv(submit, paste0( model_label, '.csv'), quote=FALSE, row.names = FALSE)


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



