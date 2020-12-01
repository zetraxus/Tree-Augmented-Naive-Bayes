# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
library(infotheo)
source("functions.R")

file_list <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
curr_file <- file_list[1]

data <- read.csv(file = curr_file, header = FALSE)

# discretize column "V1" in dataframe "data", bins = 5
#data$V1 <- discretize(data$V1, disc="equalwidth", nbins = 5)

I <- conditionalMutualInformation(data[,1:9], data[,10])
mst_undirected_tree <- MST(I)
mst_directed_tree <- direct_tree(mst_undirected_tree)
print(mst_directed_tree)
conditionalProbabilities <- calculateConditionaProbabilities(mst_directed_tree, data[,1:9], data[,10])
print(conditionalProbabilities)

# gotowe
# stworzenie macierzy liczba_atrybutow x liczba_atrybutow
# wyliczenie informacji wzajemnej miedzy atrybutami i wpisanie do macierzy
# posortowanie wszystkich trojek (Atrybut x Atrybut x Informacja wzajemna miedzy nimi)
# wyznaczenie drzewa rozpinajacego - (liczba_atrybutow -1) krawedzi (Atrybut x Atrybut x Informacja)

# todo
# podzial zbioru na treningowy/testowy, chwilowo mozna operowac na calosci
# wyliczenie prawdopodobienstw warunkowych dla kazdego wezla z wykorzystaniem wygladzenia Laplace'a
# predykcja przy wykorzystaniu drzewa

# import NB pakiet e1071 dzialajacego dla plikow csv
# import Drzewa Decyzyjnego pakiet party dzialajacego dla plikow csv

# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
