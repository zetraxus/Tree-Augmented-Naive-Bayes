# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")
source("probabilities.R")

preprocess_data <- function(dataset){
  dataset_filename <- gsub(" ", "", paste("data/", dataset, ".csv"))
  data <- read.csv(file = dataset_filename, header = FALSE)
  data <- discretize_dataset(data, dataset)
  splitted_data <- split_dataset(data, 0.8)
  return (splitted_data)
}

train <- function(data, method){
  if (method == "TAN"){
    return(trainTAN(data))
  } else if (method == "NB") {
    return(trainNB(data))
  } else if (method == "CTREE") {
    return(trainCTREE(data))
  }
}

test <- function(data, model, method){
  if (method == "TAN"){
    return(testTAN(data, model))
  } else if (method == "NB") {
    return(testNB(data, model))
  } else if (method == "CTREE") {
    return(testCTREE(data, model))
  }
}

save <- function(dataset, results, method){
  print(paste(dataset, method))
  for(i in seq_along(results))
    print(results[i])
}

main <- function(){
  datasets <- c("cmc", "occupancy", "diabetes", "wine", "zoo")
  #algorithms <- c("TAN", "NB", "CTREE")
  algorithms <- c("TAN", "NB")
  for (dataset in datasets){
    splitted_data <- preprocess_data(dataset)
    for (algorithm in algorithms){
      print(Sys.time())
      model <- train(splitted_data$train, algorithm)
      results <- test(splitted_data$test, model, algorithm)
      save(dataset, results, algorithm)
    }
  }
}

main()

# todo
# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
