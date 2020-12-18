
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


