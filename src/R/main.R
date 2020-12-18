# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("src/R/predict.R")
source("src/R/train.R")
source("src/R/probabilities.R")
source("src/R/utils.R")
source("src/R/metrices.R")

preprocess_data <- function(dataset) {
  dataset_filename <- gsub(" ", "", paste("data/", dataset, ".csv"))
  data <- read.csv(file = dataset_filename, header = FALSE)
  data <- discretize_data(data)
  splitted_data <- split_dataset(data, 0.8)
  return(splitted_data)
}

train <- function(data, method, param) {
  if (method == "TAN")
    return(trainTAN(data, param))
  else if (method == "NB")
    return(trainNB(data, param))
  else if (method == "CTREE")
    return(trainCTREE(data, param))
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

save <- function(dataset, results, method, param, output_file) {
  cat(paste(dataset, method, param), file = output_file, sep = "\n", append = TRUE)
  cat(paste(round(as.numeric(results), 5)), file = output_file, sep = "\n", append = TRUE)
}

main <- function() {
  datasets <- c("zoo", "cmc", "diabetes", "wine", "occupancy")
  algorithms <- c("TAN", "NB", "CTREE")
  output_file <- "results_dir/results"

  hyperParams <- new.env()

  hyperParams$TAN <- c(0, 0.1, 0.5, 1, 2, 3)
  hyperParams$NB <- c(0, 0.1, 0.5, 1, 2, 3)
  hyperParams$CTREE <- c(0, 1, 2, 3, 4)

  if (file.exists(output_file))
    file.remove(output_file)

  for (dataset in datasets) {
    splitted_data <- preprocess_data(dataset)
    for (algorithm in algorithms) {
      for (param in hyperParams[[algorithm]]) {
        model <- train(splitted_data$train, algorithm, param)
        results <- test(splitted_data$test, model, algorithm)
        save(dataset, results, algorithm, param, output_file)
      }
    }
  }
}

main()
