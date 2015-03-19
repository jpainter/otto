# lda 

# first blush...lda
library("MASS")

# train$target = factor(train$target)
train_data = train %>% dplyr::select(-id)
lda.model = lda(target ~ . , data=train_data, na.action="na.omit")

# Assess the accuracy of the prediction
# percent correct for each category of G
lda.train.pred = predict(lda.model, train_data)

ct <- table(train$target, lda.train.pred$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# predict with lda
## TODO: trn this step into output function...
test_data = test %>% dplyr::select(-id) 
lda.pred = predict(lda.model, test_data)


# first blush...qda

qda.model = qda(target ~ . -id, data=train, na.action="na.omit")

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(train$target, qda.model$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))


