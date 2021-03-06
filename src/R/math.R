DEFAULT_PROB_DIVIDER <- 10

# Calculate probability of given class
#
# Arguments:
#   c <- class value
#   classes <- table with class values from training data
#
# Return: probability of given class

calculateClassProbability <- function(c, classes) {
  numberOfClass <- classes %>% filter(class == c) %>% nrow()
  return(numberOfClass / nrow(classes))
}

# Calculate conditional probability with Laplace correction for given attribute, attribute's parent and class value
#
# Arguments:
#   a <- attribute value
#   p <- parent value
#   c <- class value
#   atrParentClass <- table with values of attribute, its parent and class from training data
#
# Return: conditional probability with Laplace correction for given attribute, attribute's parent and class value

calculateCondProbWithLaplaceCorrectionForAtr <- function(a, p, c, atrParentClass, laplaceCorrection) {
  rowsWithParentAndClass <- atrParentClass %>%
    filter(parent == p) %>%
    filter(class == c)
  numberOfRowsWithParentAndClass <- nrow(rowsWithParentAndClass) + laplaceCorrection
  numberOfRowsWithX <- rowsWithParentAndClass %>%
    filter(atr == a) %>%
    nrow()
  numberOfRowsWithX <- numberOfRowsWithX + laplaceCorrection

  return(numberOfRowsWithX / numberOfRowsWithParentAndClass)
}

# Calculate conditional probability with Laplace correction for given root and class value
#
# Arguments:
#   r <- root attribute value
#   c <- class value
#   rootClass <- table with values of root attribute and class from training data
#
# Return: conditional probability with Laplace correction for given root attribute and class value

calculateCondProbWithLaplaceCorrectionForRoot <- function(r, c, rootClass, laplaceCorrection) {
  rowsWithClass <- rootClass %>% filter(class == c)
  numberOfRowsWithClass <- nrow(rowsWithClass) + laplaceCorrection
  numberOfRowsWithRootAndClass <- rowsWithClass %>% filter(root == r) %>% nrow()
  numberOfRowsWithRootAndClass <- numberOfRowsWithRootAndClass + laplaceCorrection

  return(numberOfRowsWithRootAndClass / numberOfRowsWithClass)
}

# Predict probability of given class for given attributes values
#
# Arguments:
#   class <- class value
#   args <- arguments values
#   tree <- maximum spanning tree for TAN model
#   conditionalProbabilities <- conditional probabilities for every node in tree
#   classProbabilities <- probabilities of every class from training data
#
# Return: probability of given class for given attributes values

predictClass <- function(class, args, tree, contionalProbabilities, classProbabilities, defaultProb) {
  contionalProbForClass <- contionalProbabilities %>% filter(classVal == class)
  classProbability <- classProbabilities[classProbabilities$class == class,]$prob

  rootAtrNum <- tree[1,]$results_atr1
  rootAtrVal <- args[1, rootAtrNum]
  rootProbRow <- contionalProbForClass %>%
    filter(atrNum == rootAtrNum) %>%
    filter(atrVal == rootAtrVal)
  rootProb <- rootProbRow$probability
  if (isEmpty(rootProbRow)) {
    rootProb <- defaultProb
  }
  classProbability <- classProbability * rootProb

  for (row in seq_len(nrow(tree))) {
    parent <- tree[row,]$results_atr1
    atr <- tree[row,]$results_atr2
    pValue <- args[1, parent]
    atrValue <- args[1, atr]
    conditionalProbRow <- contionalProbForClass %>%
      filter(atrNum == atr) %>%
      filter(atrVal == atrValue) %>%
      filter(parentVal == pValue)
    conditionalProb <- conditionalProbRow$probability
    if (isEmpty(conditionalProbRow)) {
      conditionalProb <- defaultProb
    }
    classProbability <- classProbability * conditionalProb
  }

  return(classProbability)
}

# Return default probability

getDefaultProb <- function(contionalProb) {
  minProb <- contionalProb[which.min(contionalProb$probability),]$probability
  return(minProb / DEFAULT_PROB_DIVIDER)
}

# Check whether object is empty
#
# Return true if object is empty

isEmpty <- function(x) {
  return(nrow(x) == 0)
}
