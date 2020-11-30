# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)
source("functions.R")

file_list <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
curr_file <- file_list[1]

data <- read.csv(file = curr_file, header = FALSE)
I <- conditionalMutualInformation(data[,1:9], data[,10])
mst <- MST(I)
print(mst)

# gotowe
# stworzenie macierzy liczba_atrybutow x liczba_atrybutow
# wyliczenie informacji wzajemnej miedzy atrybutami i wpisanie do macierzy
# posortowanie wszystkich trojek (Atrybut x Atrybut x Informacja wzajemna miedzy nimi)
# wyznaczenie drzewa rozpinajacego - (liczba_atrybutow -1) krawedzi (Atrybut x Atrybut x Informacja)

# todo
# podzial zbioru na treningowy/testowy, chwilowo mozna operowac na calosci
# tu nie jestem pewien:
# wyliczenie prawdopodobienstw warunkowych dla kazdego wezla z wykorzystaniem wygladzenia Laplace'a
# predykcja przy wykorzystaniu drzewa

# import NB pakiet e1071 dzialajacego dla plikow csv
# import Drzewa Decyzyjnego pakiet party dzialajacego dla plikow csv

# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
