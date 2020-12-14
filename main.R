# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")
source("probabilities.R")

preprocess_data <- function(dataset) {
  dataset_filename <- gsub(" ", "", paste("data/", dataset, ".csv"))
  data <- read.csv(file = dataset_filename, header = FALSE)
  data <- discretize_data(data)
  splitted_data <- split_dataset(data, 0.8)
  return(splitted_data)
}

train <- function(data, method) {
  if (method == "TAN")
    return(trainTAN(data))
  else if (method == "NB")
    return(trainNB(data))
  else if (method == "CTREE")
    return(trainCTREE(data))
}

test <- function(data, model, algorithm) {
  predicted <- NULL
  real <- NULL
  for (i in 1:(nrow(data))) {
    if (algorithm == "TAN")
      predictedClass <- predictTAN(data = data[i, 1:(ncol(data) - 1)], model)
    else if (algorithm == "NB")
      predictedClass <- predictNB(data[i, 1:(ncol(data) - 1)], model)
    else if (algorithm == "CTREE")
      predictedClass <- predictCTREE(data[i, 1:(ncol(data) - 1)], model)
    predicted <- append(predicted, predictedClass)
    real <- append(real, data[i, ncol(data)])
  }
  return(calc_prec_recall_f1(list("pred" = predicted, "real" = real)))
}

save <- function(dataset, results, method) {
  cat(paste(dataset, method), sep = "\n")
  cat(paste(round(as.numeric(results), 2)), sep = "\n")
}

main <- function() {
  datasets <- c("cmc", "diabetes", "zoo", "occupancy", "wine")
  #algorithms <- c("TAN", "NB", "CTREE")
  algorithms <- c("TAN", "NB")
  for (dataset in datasets) {
    splitted_data <- preprocess_data(dataset)
    for (algorithm in algorithms) {
      print(Sys.time())
      model <- train(splitted_data$train, algorithm)
      results <- test(splitted_data$test, model, algorithm)
      save(dataset, results, algorithm)
    }
  }
}

main()
