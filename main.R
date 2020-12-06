# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")

preprocess_data <- function(dataset){
  data <- read.csv(file = dataset, header = FALSE)
  data <- discretize_dataset(data, dataset)
  splitted_data <- split_dataset(data, 0.8)
  return (splitted_data)
}

train <- function(data, method){
  if (method == "TAN"){
    I <- conditionalMutualInformation(data[, 1:(ncol(data)-1)], data[, ncol(data)])
    mst_undirected_tree <- MST(I)
    mst_directed_tree <- direct_tree(mst_undirected_tree)
    print(mst_directed_tree)
    conditionalProbabilities <- calculateConditionalProbabilities(tree = mst_directed_tree, args = data[, 1:(ncol(data)-1)], class = data[, ncol(data)])
    print(conditionalProbabilities)
    classesProb <- calculateClassProbabilities(data[ncol(data)])
    print(classesProb)
    return(list("tree" = mst_directed_tree, "condtionalProb" = conditionalProbabilities, "classesProb" = classesProb))
  }
}

test <- function(data, model, method){
  if (method == "TAN"){
    classesPrediction <- data.frame(matrix(ncol = 2, nrow = 0))
    columns <- c("predictedClass", "realClass")
    colnames(classesPrediction) <- columns
    for (i in 1:(nrow(data))) {
      predictedClass <- predict(data = data[i, 1:(ncol(data) - 1)], model = model)
      predictedVsReal <- data.frame(predictedClass, data[i, ncol(data)])
      colnames(predictedVsReal) <- columns
      classesPrediction <- rbind(classesPrediction, predictedVsReal)
    }

    print(classesPrediction)
    return(classesPrediction)
  }
}

save <- function(results, method){

}

main <- function(){
  datasets <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
  algorithms <- c("TAN", "NB", "CTREE")
  for (dataset in datasets){
    splitted_data <- preprocess_data(dataset)
    for (algorithm in algorithms){
      model <- train(splitted_data$train, algorithm)
      results <- test(splitted_data$test, model, algorithm)
      goodPredictions <- results %>% filter(predictedClass == realClass) %>% nrow()
      acc <- goodPredictions/(nrow(results))
      print(acc)
      save(results, algorithm)
    }

    if (dataset == "data/cmc.csv"){ # todo remove it later
      break
    }
  }
}

main()

# gotowe
# stworzenie macierzy liczba_atrybutow x liczba_atrybutow
# wyliczenie informacji wzajemnej miedzy atrybutami i wpisanie do macierzy
# posortowanie wszystkich trojek (Atrybut x Atrybut x Informacja wzajemna miedzy nimi)
# wyznaczenie drzewa rozpinajacego - (liczba_atrybutow -1) krawedzi (Atrybut x Atrybut x Informacja)
# podzial zbioru na treningowy/testowy, chwilowo mozna operowac na calosci

# todo
# wyliczenie prawdopodobienstw warunkowych dla kazdego wezla z wykorzystaniem wygladzenia Laplace'a
# predykcja przy wykorzystaniu drzewa

# import NB pakiet e1071 dzialajacego dla plikow csv
# import Drzewa Decyzyjnego pakiet party dzialajacego dla plikow csv

# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
