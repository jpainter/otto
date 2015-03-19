
# Describe training set

library("data.table")
library("dplyr")

train = as.data.frame(fread("train.csv"))
str(train)

test = as.data.frame(fread("test.csv")) 
str(test)

# number of examples per target
train %>% count(target)

head(train)

# summarise features
library("Hmisc")
train %>%  select(-id, -target) %>% describe()

train %>%  ggplot(aes(x = sqrt(feat_90))) + geom_histogram(binwidth = 1)

