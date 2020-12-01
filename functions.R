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

condInformation <- function(atr1atr2class) {
    condinformation <- 0.0
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
                condinformation <- condinformation + partialCondInf
            }
        }
    }

    return(condinformation)
}

MST <- function(df){
  df <- df[order(-df$I),]
  atr1_vec <- vector()
  atr2_vec <- vector()
  I_vec <- vector()
  conn_comp <- seq(from = 1, to = max(df$atr2))

  for(i in seq_len(nrow(df))) {
    row <- df[i,]
    if (conn_comp[row$atr1] != conn_comp[row$atr2]){
      atr1_vec <- append(atr1_vec, row$atr1)
      atr2_vec <- append(atr2_vec, row$atr2)
      I_vec <- append(I_vec, row$I)

      conn_comp_atr1 <- conn_comp[row$atr1]
      for (j in seq_along(conn_comp)){
        if (conn_comp[j] == conn_comp_atr1)
          conn_comp[j] <- conn_comp[row$atr2]
      }
    }
  }

  return(data.frame(atr1_vec, atr2_vec, I_vec))
}

direct_tree <- function(tree){
  added_to_queue <- integer(max(tree$atr2))
  start_node <- sample(1:(max(tree$atr2)), 1)
  queue <- start_node
  results_atr1 <- NULL
  results_atr2 <- NULL
  results_I <- NULL
  while (length(queue) !=0){
    atr <- head(queue, 1)
    queue <- tail(queue, length(queue) - 1)
    added_to_queue[atr] <- 1
    for (row in seq_len(nrow(tree))) {
      if (tree[row, "atr1_vec"] == atr && added_to_queue[tree[row, "atr2_vec"]] == 0){
        added_to_queue[tree[row, "atr2_vec"]] <- 1
        queue <- append(queue, tree[row, "atr2_vec"])
        results_atr1 <- append(results_atr1, atr)
        results_atr2 <- append(results_atr2, tree[row, "atr2_vec"])
        results_I <- append(results_I, tree[row, "I_vec"])
      }
      if (tree[row, "atr2_vec"] == atr && added_to_queue[tree[row, "atr1_vec"]] == 0){
        added_to_queue[tree[row, "atr1_vec"]] <- 1
        queue <- append(queue, tree[row, "atr1_vec"], after = length(queue))
        results_atr1 <- append(results_atr1, atr, after = length(results_atr1))
        results_atr2 <- append(results_atr2, tree[row, "atr1_vec"], after = length(results_atr2))
        results_I <- append(results_I, tree[row, "I_vec"])
      }
    }
  }
  return(data.frame(results_atr1, results_atr2, results_I))
}
