# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")
source("probabilities.R")

preprocess_data <- function(dataset, discretize_bins) {
  dataset_filename <- gsub(" ", "", paste("data/", dataset, ".csv"))
  data <- read.csv(file = dataset_filename, header = FALSE)
  data <- discretize_data(data, discretize_bins)
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

save <- function(dataset, results, method, bins) {
  output_filename <- gsub(" ", "", paste("results/", bins))
  cat(paste(dataset, method), file=output_filename, sep = "\n", append=TRUE)
  cat(paste(round(as.numeric(results), 2)), file=output_filename, sep = "\n",append=TRUE)
}

main <- function() {
  datasets <- c("zoo", "cmc", "diabetes", "occupancy", "wine")
  #algorithms <- c("TAN", "NB", "CTREE")
  algorithms <- c("TAN", "NB")

  t_all_start <- Sys.time()
  for(bins in seq(from = 2, to = 6)){
    for (dataset in datasets) {
      t_start <- Sys.time()
      splitted_data <- preprocess_data(dataset, bins)
      for (algorithm in algorithms) {
        model <- train(splitted_data$train, algorithm)
        results <- test(splitted_data$test, model, algorithm)
        save(dataset, results, algorithm, bins)
      }
      print(Sys.time() - t_start)
    }
    print(Sys.time() - t_all_start)
  }
}

main()
