# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

install.packages("infotheo")
library(infotheo)

file_list <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
curr_file <- file_list[1]

data <- read.csv(file = curr_file, header = FALSE)
head(data)
tail(data)

# podzial zbioru na treningowy/testowy, chwilowo mozna operowac na calosci

# stworzenie macierzy liczba_atrybutow x liczba_atrybutow
# wyliczenie informacji wzajemnej miedzy atrybutami i wpisanie do macierzy

getConditionalMutualInformation <- function(attributes, class) {
    mutualInformations <- data.frame(matrix(ncol = 3, nrow = 0))
    columns <- c("I", "atr1", "atr2")
    colnames(mutualInformations) <- columns
    for (i in 1:(ncol(attributes) - 1)) {
        for (j in i:ncol(attributes)) {
            mutualInformation <- data.frame(condinformation(attributes[,i], attributes[,j], class), i, j)
            colnames(mutualInformation) <- columns
            mutualInformations <- rbind(mutualInformations, mutualInformation)
        }
    }

    return(mutualInformations)
}

I <- getConditionalMutualInformation(data[,1:9], data[,10])
head(I)
# posortowanie wszystkich trojek (Atrybut x Atrybut x Informacja wzajemna miedzy nimi)
# wyznaczenie drzewa rozpinajacego - (liczba_atrybutow -1) krawedzi (Atrybut x Atrybut x Informacja)

# tu nie jestem pewien:
# wyliczenie prawdopodobienstw warunkowych dla kazdego wezla z wykorzystaniem wygladzenia Laplace'a
# predykcja przy wykorzystaniu drzewa

# import NB pakiet e1071 dzialajacego dla plikow csv
# import Drzewa Decyzyjnego pakiet party dzialajacego dla plikow csv

# funkcja testujaca i zapisujaca wyniki
# funkcja podsumowujaca wyniki eksperymentow
