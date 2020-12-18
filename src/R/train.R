library(e1071)
library(party)

source("tan.R")

# Train TAN model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#
# Return: trained TAN model

trainTAN <- function(data, laplaceCorrection) {
  I <- conditionalMutualInformation(data[, 1:(ncol(data) - 1)], data[, ncol(data)])
  mst_undirected_tree <- MST(I)
  mst_directed_tree <- direct_tree(mst_undirected_tree)
  conditionalProbabilities <- calculateConditionalProbabilities(tree = mst_directed_tree, args = data[, 1:(ncol(data) - 1)],
                                                                class = data[, ncol(data)], laplaceCorrection = laplaceCorrection)
  classesProb <- calculateClassProbabilities(data[ncol(data)])
  return(list("tree" = mst_directed_tree, "condtionalProb" = conditionalProbabilities, "classesProb" = classesProb))
}

# Train NB (Naive Bayes) model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#   laplaceCorrection <- value of Laplace correction
#
# Return: trained NB model

trainNB <- function(data, laplaceCorrection) {
  return(naiveBayes(x = data[, 1:(ncol(data) - 1)], y = data[, ncol(data)], laplace = laplaceCorrection))
}

# Train CTree model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#   maxDepth <- maximum depth of tree
#
# Return: trained CTree model

trainCTREE <- function(data, maxDepth) {
  data[ncol(data)] <- as.factor(data[, ncol(data)])
  classCol <- colnames(data[ncol(data)])
  FORMULA <- as.formula(paste(classCol, " ~ ."))
  return(ctree(FORMULA, data, control = ctree_control(maxdepth = maxDepth)))
}
