# random forrest

library(randomForest)
library(caret)

train_data = as.data.frame(fread("train.csv"))[,-1]
test_data <- as.data.frame(fread("test.csv"))[,-1]

train_data$target = factor(train_data$target) # needs to be factor



# bagging mtry=93  ####
model <- randomForest( target ~ . , train_data, mtry=93, 
                       importance=TRUE, na.action=na.omit)

# train eval
rf.train.pred = predict(model, newdata=train_data, type='class')
ct <- table(train$target, rf.train.pred)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

submit = as.data.frame(fread("sampleSubmission.csv"))
submit[, 2:10] <- predict(model, newdata=test_data, type='prob')
write.csv(submit, 'bag_pred.csv', quote=FALSE, row.names = FALSE)



# Random Forest mtry=9  ####
model <- randomForest( target ~ . , train_data, mtry=9, 
                       importance=TRUE, na.action=na.omit)

# train eval
rf.train.pred = predict(model, newdata=train_data, type='class')
ct <- table(train$target, rf.train.pred)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

submit = as.data.frame(fread("sampleSubmission.csv"))
submit[, 2:10] <- predict(model, newdata=test_data, type='prob')
write.csv(submit, 'rf_pred.csv', quote=FALSE, row.names = FALSE)

# Random Forest mtry=3  ####
model <- randomForest( target ~ . , train_data, mtry=3, 
                       importance=TRUE, na.action=na.omit)

# train eval
rf.train.pred = predict(model, newdata=train_data, type='class')
ct <- table(train$target, rf.train.pred)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

submit = as.data.frame(fread("sampleSubmission.csv"))
submit[, 2:10] <- predict(model, newdata=test_data, type='prob')
write.csv(submit, 'rf_3_pred.csv', quote=FALSE, row.names = FALSE)
