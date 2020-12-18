library(e1071)
library(party)

source("src/R/tan.R")

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- TAN model
#
# Return: best matching class for given attributes values

predictTAN <- function(data, model) {
  prediction <- predictClasses(args = data, tree = model$tree, contionalProbabilities = model$condtionalProb,
                               classProbabilities = model$classesProb)
  return(prediction[which.max(prediction$predictedProb),]$class)
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- NB model
#
# Return: best matching class for given attributes values

predictNB <- function(data, model) {
  prediction <- stats::predict(object = model, newdata = data, type = "raw")
  return(as.numeric(colnames(prediction)[apply(prediction, 1, which.max)]))
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- CTREE model
#
# Return: predicted class for given attributes values

predictCTREE <- function(data, model) {
  predictedClass <- stats::predict(object = model, newdata = data, type = "response")
  return(predictedClass)
}
