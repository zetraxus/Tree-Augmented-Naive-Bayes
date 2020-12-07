
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

discretize_dataset <- function(data, dataset_name){
  # discretize column "V1" in dataframe "data", bins = 5
  # data$V1 <- discretize(data$V1, disc="equalwidth", nbins = 5)
  return (data) # todo fix it
}

split_dataset <- function(data, train_size){
  bound <- floor(nrow(data) * train_size)
  #data <- data[sample(nrow(data)), ] #sample rows
  data.train <- data[1:bound, ]
  data.test <- data[(bound+1):nrow(data), ]
  return (list("train" = data.train, "test" = data.test))
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- TAN model
#
# Return: best matching class for given attributes values
predict <- function(data, model) {
  prediction <- predictClasses(args = data, tree = model$tree, contionalProbabilities = model$condtionalProb,
                        classProbabilities = model$classesProb)
  return(prediction[which.max(prediction$predictedProb),]$class)
}

calc_acc <- function(list_pred_real){
  df_pred_real <- data.frame(list_pred_real)
  correct <- df_pred_real %>% filter(pred == real) %>% nrow()
  acc <- correct / nrow(df_pred_real)
  return (acc)
}
