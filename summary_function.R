
# summaryFunction

LogLoss <- function(actual, predicted, eps=0.00001) {
  
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  
  -sum(actual*log(predicted))
}