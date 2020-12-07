LAPLACE_CORRECTION <- 1

# Calculate probability of given class
#
# Arguments:
#   c <- class value
#   classes <- table with class values from training data
#
# Return: probability of given class
#
calculateClassProbability <- function(c, classes) {
  numberOfClass <- classes %>% filter(class == c) %>% nrow()
  return (numberOfClass / nrow(classes))
}

# Caculate conditional probability with Laplace correction for given attribute, attribute's parent and class value
#
# Arguments:
#   a <- attribute value
#   p <- parent value
#   c <- class value
#   atrParentClass <- table with values of attribute, its parent and class from training data
#
# Return: conditional probability with Laplace correction for given attribute, attribute's parent and class value
#
calculateCondProbWithLaplaceCorrectionForAtr <- function(a, p, c, atrParentClass) {
  rowsWithParentAndClass <- atrParentClass %>% filter(parent == p) %>% filter(class == c)
  numberOfRowsWithParentAndClass <- nrow(rowsWithParentAndClass) + LAPLACE_CORRECTION
  numberOfRowsWithX <- rowsWithParentAndClass %>% filter(atr == a) %>% nrow()
  numberOfRowsWithX <- numberOfRowsWithX + LAPLACE_CORRECTION

  return(numberOfRowsWithX / numberOfRowsWithParentAndClass)
}

# Caculate conditional probability with Laplace correction for given root and class value
#
# Arguments:
#   r <- root attribute value
#   c <- class value
#   rootClass <- table with values of root attribute and class from training data
#
# Return: conditional probability with Laplace correction for given root attribute and class value
#
calculateCondProbWithLaplaceCorrectionForRoot <- function(r, c, rootClass) {
  rowsWithClass <- rootClass %>% filter(class == c)
  numberOfRowsWithClass <- nrow(rowsWithClass) + LAPLACE_CORRECTION
  numberOfRowsWithRootAndClass <- rowsWithClass %>% filter(root == r) %>% nrow()
  numberOfRowsWithRootAndClass <- numberOfRowsWithRootAndClass + LAPLACE_CORRECTION

  return (numberOfRowsWithRootAndClass / numberOfRowsWithClass)
}

# Predict probability of given class for given attributes values
#
# Arguments:
#   class <- class value
#   args <- arguments values
#   tree <- maximum spanning tree for TAN model
#   contionalProbabilities <- conditional probabilities for every node in tree
#   classProbabilities <- probabilities of every class from training data
#
# Return: probability of given class for given attributes values
#
predictClass <- function (class, args, tree, contionalProbabilities, classProbabilities) {
  contionalProbForClass <- contionalProbabilities %>% filter(classVal == class)
  classProbability <- classProbabilities[classProbabilities$class == class, ]$prob
  rootAtrNum <- tree[1,]$results_atr1
  rootAtrVal <- args[1, rootAtrNum]
  rootProbRow <- contionalProbForClass %>% filter(atrNum == rootAtrNum) %>% filter(atrVal == rootAtrVal)
  classProbability <- classProbability * rootProbRow$probability

  for(row in seq_len(nrow(tree))) {
    parent <- tree[row,]$results_atr1
    atr <- tree[row,]$results_atr2
    pValue <- args[1, parent]
    atrValue <- args[1, atr]
    conditionalProbRow <- contionalProbForClass %>% filter(atrNum == atr)  %>% filter(atrVal == atrValue) %>% filter(parentVal == pValue)
    classProbability <- classProbability * conditionalProbRow$probability
  }

  return(classProbability)
}
