# Title     : Tree Augmented Naive Bayes (TAN)
# Created on: 26.11.2020

library(dplyr)

file_list <- c("data/cmc.csv", "data/diabetes.csv", "data/occupancy.csv", "data/wine.csv", "data/zoo.csv")
curr_file <- file_list[1]

data <- read.csv(file = curr_file, header = FALSE)

# podzial zbioru na treningowy/testowy, chwilowo mozna operowac na calosci

# stworzenie macierzy liczba_atrybutow x liczba_atrybutow
# wyliczenie informacji wzajemnej miedzy atrybutami i wpisanie do macierzy
conditionalMutualInformation <- function(attributes, class) {
    mutualInformations <- data.frame(matrix(ncol = 3, nrow = 0))
    columns <- c("I", "atr1", "atr2")
    colnames(mutualInformations) <- columns
    for (i in 1:(ncol(attributes) - 1)) {
        for (j in (i + 1):ncol(attributes)) {
            atr1atr2class <- data.frame(matrix(ncol = 3, nrow = nrow(attributes)))
            colnames(atr1atr2class) <- c("atr1", "atr2", "class")
            atr1atr2class$atr1 <- attributes[,i]
            atr1atr2class$atr2 <- attributes[,j]
            atr1atr2class$class <- class
            I <- condInformation(atr1atr2class)
            mutualInformation <- data.frame(I, i, j)
            colnames(mutualInformation) <- columns
            mutualInformations <- rbind(mutualInformations, mutualInformation)
        }
    }

    return(mutualInformations)
}
head(mutinformation(data))

condInformation <- function(atr1atr2class) {
    condinformation = 0.0
    for (i in unique(atr1atr2class$atr1)) {
        for (j in unique(atr1atr2class$atr2)) {
            for (c in unique(atr1atr2class$class)) {
                rowsWithClass <- atr1atr2class %>% filter(class == c) %>% nrow()
                rowsWithClassAndAtr1 <- atr1atr2class %>% filter(atr1 == i) %>% filter(class == c) %>% nrow()
                rowsWithClassAndAtr2 <- atr1atr2class %>% filter(atr2 == j) %>% filter(class == c) %>% nrow()
                rowsWithAtr1AndAtr2AndClass <- atr1atr2class %>% filter(atr1 == i) %>% filter(atr2 == j) %>% filter(class == c) %>% nrow()
                multiProb <- (rowsWithAtr1AndAtr2AndClass / nrow(atr1atr2class))
                condProbAtr1Atr2 <- (rowsWithAtr1AndAtr2AndClass / rowsWithClass)
                condProbAtr1 <- (rowsWithClassAndAtr1 / rowsWithClass)
                condProbAtr2 <- (rowsWithClassAndAtr2 / rowsWithClass)
                if (multiProb == 0 || condProbAtr1 == 0 || condProbAtr2 == 0 || condProbAtr1Atr2 == 0) {
                    partialCondInf <- 0.0
                } else {
                   partialCondInf <- multiProb * (log((condProbAtr1Atr2 / (condProbAtr1 * condProbAtr2)), 2))
                }
                condinformation = condinformation + partialCondInf
            }
        }
    }

    return(condinformation)
}

I <- conditionalMutualInformation(data[,1:9], data[,10])
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
