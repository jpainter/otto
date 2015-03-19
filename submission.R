
sample = as.data.frame(fread("sampleSubmission.csv"))
str(sample)

## convert prediction to dummy vars...
test_pred = model.matrix(~lda.pred$class) %>% as.data.frame()
colnames(test_pred)[1] = "Class_1"
test_pred[, 1 ] = 1 - rowSums(test_pred[, 2:9])

lda_test = cbind( test[,1], test_pred)
str(lda_test)
names(lda_test) = names(sample)
write.csv(lda_test, "lda_test.csv", row.names = FALSE)

submission = fread("lda_test.csv") %>% as.data.frame()
str(submission)

