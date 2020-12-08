# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")
source("probabilities.R")

preprocess_data <- function(dataset){
  data <- read.csv(file = dataset, header = FALSE)
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

save <- function(results, method){
  print(method)
  for(i in seq_along(results))
    print(results[i])
}

main <- function(){
  datasets <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
  algorithms <- c("TAN", "NB", "CTREE")
  for (dataset in datasets){
    splitted_data <- preprocess_data(dataset)
    for (algorithm in algorithms){
      model <- train(splitted_data$train, algorithm)
      results <- test(splitted_data$test, model, algorithm)
      save(results, algorithm)
    }

    if (dataset == "data/cmc.csv"){ # todo remove it later
      break
    }
  }
}

main()

# todo
# import NB pakiet e1071 dzialajacego dla plikow csv
# import Drzewa Decyzyjnego pakiet party dzialajacego dla plikow csv

# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
