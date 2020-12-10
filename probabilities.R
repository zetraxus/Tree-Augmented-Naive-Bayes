source("math.R")

# Calculate conditional mutual information between every two attributes
#
# Arguments:
#   attributes <- table with training attributes values
#   class <- table with class values for every row from attributes
#
# Example return:
#  I   |  atr1  | atr2
# -----|--------|------
# 0.5  |   2    |   5
#
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

# Calculate conditional mutual information for two attributes
#
# Arguments:
#   atr1atr2class <- table with values of first and second attribute and class
#
# Return: conditional mutual informatin for two attributes
#
condInformation <- function(atr1atr2class) {
    condinformation <- 0.0

    for (i in unique(atr1atr2class$atr1)) {
        for (j in unique(atr1atr2class$atr2)) {
            for (c in unique(atr1atr2class$class)) {
                condinformation <- condinformation + calculatePartialCondInformation(i, j, c, atr1atr2class)
            }
        }
    }

    return(condinformation)
}

# Calculate partial condtitional mutual information for given attributes and class values
#
# Arguments:
#   atr1Val <- value of first attribute
#   atr2Val <- value of second attribute
#   classVal <- value of class
#   atr1atr2class <- table with values of first and second attribute and class from training data
#
# Return: partial condtitional mutual information for given attributes and class values
#
calculatePartialCondInformation <- function(atr1Val, atr2Val, classVal, atr1atr2class) {
  rowsWithClass <- atr1atr2class %>% filter(class == classVal)
  numberOfrowsWithClass <- nrow(rowsWithClass)
  numberOfRowsWithClassAndAtr1 <- rowsWithClass %>% filter(atr1 == atr1Val) %>% nrow()
  numberOfRowsWithClassAndAtr2 <- rowsWithClass %>% filter(atr2 == atr2Val) %>% nrow()
  numberOfRowsWithAtr1AndAtr2AndClass <- rowsWithClass %>% filter(atr1 == atr1Val) %>% filter(atr2 == atr2Val) %>% nrow()
  multiProb <- (numberOfRowsWithAtr1AndAtr2AndClass / nrow(atr1atr2class))
  condProbAtr1Atr2 <- (numberOfRowsWithAtr1AndAtr2AndClass / numberOfrowsWithClass)
  condProbAtr1 <- (numberOfRowsWithClassAndAtr1 / numberOfrowsWithClass)
  condProbAtr2 <- (numberOfRowsWithClassAndAtr2 / numberOfrowsWithClass)

  if (multiProb == 0 || condProbAtr1 == 0 || condProbAtr2 == 0 || condProbAtr1Atr2 == 0) {
    partialCondInf <- 0.0
  } else {
    partialCondInf <- multiProb * (log((condProbAtr1Atr2 / (condProbAtr1 * condProbAtr2)), 2))
  }
}


# Calculate conditional probabilities for every node form tree
#
# Arguments:
#   tree <- directed TAN tree
#   args <- table with training attributes values
#   class <- table with class values for every row from attributes
#
# Example return:
#  atrNum   |  atrVal  | parentVal  |  classVal  | probability
# ----------|----------|------------|------------|-------------
#    1      |    15    |    NA      |     2      |   0.7
#    2      |    7     |    15      |     2      |   0.3
#
calculateConditionalProbabilities <- function (tree, args, class) {
  probabilities <- data.frame(matrix(ncol = 5, nrow = 0))
  columns <- c("atrNum", "atrVal", "parentVal", "classVal", "probability")
  colnames(probabilities) <- columns

  rootAtr <- tree[1,]$results_atr1
  rootClass <- data.frame(args[,rootAtr], class)
  colnames(rootClass) <- c("root", "class")
  probabilities <- rbind(probabilities, calculateConditionalProbabilitiesForRoot(rootAtr, rootClass))

  for(row in seq_len(nrow(tree))) {
    atrNum <- tree[row,]$results_atr2
    parentNum <- tree[row,]$results_atr1
    atrParentClass <- data.frame(args[,atrNum], args[,parentNum], class)
    colnames(atrParentClass) <- c("atr", "parent", "class")
    probabilities <- rbind(probabilities, calculateConditionalProbabilitiesForAtribute(atrNum, atrParentClass))
  }

  return(probabilities)
}

# Calculate conditional probability for given attribute
#
# Arguments:
#   atrNum <- attribute number
#   atrParentClass <- table with values of given attribute, its parent from TAN tree and class from training data
#
# Example return:
#  atrNum   |  atrVal  | parentVal  |  classVal  | probability
# ----------|----------|------------|------------|-------------
#    2      |    1     |    15      |     2      |   0.7
#    2      |    2     |    15      |     2      |   0.3
#
calculateConditionalProbabilitiesForAtribute <- function(atrNum, atrParentClass) {
  probabilities <- data.frame(matrix(ncol = 5, nrow = 0))
  columns <- c("atrNum", "atrVal", "parentVal", "classVal", "probability")
  colnames(probabilities) <- columns

  for (a in unique(atrParentClass$atr)) {
    for (p in unique(atrParentClass$parent)) {
      for (c in unique(atrParentClass$class)) {

        conditionalProbability <- data.frame(atrNum, a, p, c, calculateCondProbWithLaplaceCorrectionForAtr(a, p, c, atrParentClass))
        colnames(conditionalProbability) <- columns
        probabilities <- rbind(probabilities, conditionalProbability)
      }
    }
  }

  return(probabilities)
}

# Calculate conditional probability for root attribute
#
# Arguments:
#   rootNum <- number of root attribute
#   rootClass <- table with values of root attribute and class from training data
#
# Example return:
#  atrNum   |  atrVal  | parentVal  |  classVal  | probability
# ----------|----------|------------|------------|-------------
#    1      |    1     |    NA      |     2      |   0.7
#    1      |    2     |    NA      |     2      |   0.3
#
calculateConditionalProbabilitiesForRoot <- function(rootNum, rootClass) {
  probabilities <- data.frame(matrix(ncol = 5, nrow = 0))
  columns <- c("atrNum", "atrVal", "parentVal", "classVal", "probability")
  colnames(probabilities) <- columns

  for(r in unique(rootClass$root)) {
    for (c in unique(rootClass$class)) {
      conditionalProbability <- data.frame(rootNum, r, NA, c, calculateCondProbWithLaplaceCorrectionForRoot(r, c, rootClass))
      colnames(conditionalProbability) <- columns

      probabilities <- rbind(probabilities, conditionalProbability)
    }
  }

  return(probabilities)
}

# Calculate probability of every class
#
# Arguments:
#   classes <- table with class values from training data
#
# Example return:
#   class  |  predictedProb
# ---------|----------------
#     1    |     0.2
#     2    |     0.3
#     3    |     0.5
#
calculateClassProbabilities <- function(classes) {
  colnames(classes) <- "class"
  probabilities <- data.frame(matrix(ncol = 2, nrow = 0))
  columns <- c("class", "prob")
  colnames(probabilities) <- columns
  for (c in unique(classes$class)) {
    probability <- data.frame(c, calculateClassProbability(c, classes));
    colnames(probability) <- columns
    probabilities <- rbind(probabilities, probability);
  }

  return(probabilities)
}

# Predict probability of every class for given attributes values
#
# Arguments:
#   args <- arguments values
#   tree <- maximum spanning tree for TAN model
#   contionalProbabilities <- conditional probabilities for every node in tree
#   classProbabilities <- probabilities of every class from training data
#
# Example return:
#   class  |  predictedProb
# ---------|----------------
#     1    |     0.2
#     2    |     0.3
#     3    |     0.5
#
predictClasses <- function(args, tree, contionalProbabilities, classProbabilities) {
  classesPrediction <- data.frame(matrix(ncol = 2, nrow = 0))
  columns <- c("class", "predictedProb")
  colnames(classesPrediction) <- columns

  defaultProb <- getDefaultProb(contionalProbabilities)
  for (c in unique(classProbabilities$class)) {
    classProbability <- predictClass(c, args, tree, contionalProbabilities, classProbabilities, defaultProb)
    classPrediction <- data.frame(c, classProbability)
    colnames(classPrediction) <- columns
    classesPrediction <- rbind(classesPrediction, classPrediction)
  }

  return(classesPrediction)
}
