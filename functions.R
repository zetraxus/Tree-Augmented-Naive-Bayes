library(e1071)
library(party)
source("probabilities.R")

# Train TAN model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#
# Return: trained TAN model

trainTAN <- function(data) {
  I <- conditionalMutualInformation(data[, 1:(ncol(data) - 1)], data[, ncol(data)])
  mst_undirected_tree <- MST(I)
  mst_directed_tree <- direct_tree(mst_undirected_tree)
  conditionalProbabilities <- calculateConditionalProbabilities(tree = mst_directed_tree, args = data[, 1:(ncol(data) - 1)], class = data[, ncol(data)])
  classesProb <- calculateClassProbabilities(data[ncol(data)])
  return(list("tree" = mst_directed_tree, "condtionalProb" = conditionalProbabilities, "classesProb" = classesProb))
}

# Train NB (Naive Bayes) model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#
# Return: trained NB model

trainNB <- function(data) {
  return(naiveBayes(x = data[, 1:(ncol(data) - 1)], y = data[, ncol(data)], laplace = 1))
}

# Train CTree model on given data
#
# Arguments:
#   data <- training data (last column contains class value)
#
# Return: trained CTree model

trainCTREE <- function(data) {
  data[ncol(data)] <- as.factor(data[, ncol(data)])
  classCol <- colnames(data[ncol(data)])
  FORMULA <- as.formula(paste(classCol, " ~ ."))
  return(ctree(FORMULA, data))
}

MST <- function(df) {
  df <- df[order(-df$I),]
  atr1_vec <- NULL
  atr2_vec <- NULL
  I_vec <- NULL
  conn_comp <- seq(from = 1, to = max(df$atr2))

  for (i in seq_len(nrow(df))) {
    row <- df[i,]
    if (conn_comp[row$atr1] != conn_comp[row$atr2]) {
      atr1_vec <- append(atr1_vec, row$atr1)
      atr2_vec <- append(atr2_vec, row$atr2)
      I_vec <- append(I_vec, row$I)

      conn_comp_atr1 <- conn_comp[row$atr1]
      for (j in seq_along(conn_comp)) {
        if (conn_comp[j] == conn_comp_atr1)
          conn_comp[j] <- conn_comp[row$atr2]
      }
    }
  }

  return(data.frame(atr1_vec, atr2_vec, I_vec))
}

direct_tree <- function(tree) {
  added_to_queue <- integer(max(tree$atr2))
  start_node <- sample(1:(max(tree$atr2)), 1)
  queue <- start_node
  results_atr1 <- NULL
  results_atr2 <- NULL
  results_I <- NULL
  while (length(queue) != 0) {
    atr <- head(queue, 1)
    queue <- tail(queue, length(queue) - 1)
    added_to_queue[atr] <- 1
    for (row in seq_len(nrow(tree))) {
      if (tree[row, "atr1_vec"] == atr && added_to_queue[tree[row, "atr2_vec"]] == 0) {
        added_to_queue[tree[row, "atr2_vec"]] <- 1
        queue <- append(queue, tree[row, "atr2_vec"])
        results_atr1 <- append(results_atr1, atr)
        results_atr2 <- append(results_atr2, tree[row, "atr2_vec"])
        results_I <- append(results_I, tree[row, "I_vec"])
      }
      if (tree[row, "atr2_vec"] == atr && added_to_queue[tree[row, "atr1_vec"]] == 0) {
        added_to_queue[tree[row, "atr1_vec"]] <- 1
        queue <- append(queue, tree[row, "atr1_vec"])
        results_atr1 <- append(results_atr1, atr)
        results_atr2 <- append(results_atr2, tree[row, "atr1_vec"])
        results_I <- append(results_I, tree[row, "I_vec"])
      }
    }
  }

  return(data.frame(results_atr1, results_atr2, results_I))
}

# discretize column "V1" in dataframe "data", bins = 5
# discretize(data$V1, disc="equalwidth", nbins = 5)

discretize_data <- function(data) {
  for (i in 1:(ncol(data) - 1))
    if (length(unique(data[, i])) > bins)
      data[, i] <- discretize(data[, i], disc = "equalwidth", nbins = 10)
  return(data)
}

split_dataset <- function(data, train_size) {
  data <- data[sample(nrow(data)),] # sample rows
  bound <- floor(nrow(data) * train_size)
  data.train <- data[1:bound,]
  data.test <- data[(bound + 1):nrow(data),]
  return(list("train" = data.train, "test" = data.test))
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- TAN model
#
# Return: best matching class for given attributes values

predictTAN <- function(data, model) {
  prediction <- predictClasses(args = data, tree = model$tree, contionalProbabilities = model$condtionalProb,
                               classProbabilities = model$classesProb)
  return(prediction[which.max(prediction$predictedProb),]$class)
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- NB model
#
# Return: best matching class for given attributes values

predictNB <- function(data, model) {
  prediction <- stats::predict(object = model, newdata = data, type = "raw")
  return(as.numeric(colnames(prediction)[apply(prediction, 1, which.max)]))
}

# Predict class for given attributes values based on given model
#
# Arguments:
#   data <- attributes values
#   model <- CTREE model
#
# Return: predicted class for given attributes values

predictCTREE <- function(data, model) {
  predictedClass <- stats::predict(object = model, newdata = data, type = "response")
  return(predictedClass)
}

calc_prec_recall_f1 <- function(list_pred_real) {
  real_unique_values <- unique(list_pred_real$real)

  true_positive <- rep(0, length(real_unique_values))
  predicted <- rep(0, length(real_unique_values))
  real <- rep(0, length(real_unique_values))

  idx <- 1
  for (i in real_unique_values) {
    for (j in seq_along(list_pred_real$pred)) {
      if (list_pred_real$real[j] == i & list_pred_real$pred[j] == i)
        true_positive[idx] <- true_positive[idx] + 1
      if (list_pred_real$real[j] == i)
        real[idx] <- real[idx] + 1
      if (list_pred_real$pred[j] == i)
        predicted[idx] <- predicted[idx] + 1
    }
    idx <- idx + 1
  }

  precision <- rep(0, length(real_unique_values))
  recall <- rep(0, length(real_unique_values))
  f1_score <- rep(0, length(real_unique_values))

  for (i in seq_along(real_unique_values)) {
    if (predicted[i] == 0)
      precision[i] <- 0
    else
      precision[i] <- true_positive[i] / predicted[i]
    if (real[i] == 0)
      recall[i] <- 0
    else
      recall[i] <- true_positive[i] / real[i]
    if (precision[i] + recall[i] == 0)
      f1_score[i] <- 0
    else
      f1_score[i] <- 2 * (precision[i] * recall[i]) / (precision[i] + recall[i])
  }

  precision_avg <- 0
  recall_avg <- 0
  f1_score_avg <- 0
  precision_weighted <- 0
  recall_weighted <- 0
  f1_score_weighted <- 0

  for (i in seq_along(real_unique_values)) {
    precision_avg <- precision_avg + (precision[i] / length(real_unique_values))
    recall_avg <- recall_avg + (recall[i] / length(real_unique_values))
    f1_score_avg <- f1_score_avg + (f1_score[i] / length(real_unique_values))

    precision_weighted <- precision_weighted + precision[i] * (real[i] / sum(real))
    recall_weighted <- recall_weighted + recall[i] * (real[i] / sum(real))
    f1_score_weighted <- f1_score_weighted + f1_score[i] * (real[i] / sum(real))
  }

  return(list("prec_avg" = precision_avg, "rec_avg" = recall_avg, "f1_avg" = f1_score_avg, "prec_w" = precision_weighted, "rec_w" = recall_weighted, "f1_w" = f1_score_weighted))
}
