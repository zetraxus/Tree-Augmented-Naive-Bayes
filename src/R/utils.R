BINS <- 10

# discretize column "V1" in dataframe "data", bins = 5
# discretize(data$V1, disc="equalwidth", nbins = 5)

discretize_data <- function(data) {
  for (i in 1:(ncol(data) - 1))
    if (length(unique(data[, i])) > BINS)
      data[, i] <- discretize(data[, i], disc = "equalwidth", nbins = BINS)
  return(data)
}

split_dataset <- function(data, train_size) {
  data <- data[sample(nrow(data)),] # sample rows
  bound <- floor(nrow(data) * train_size)
  data.train <- data[1:bound,]
  data.test <- data[(bound + 1):nrow(data),]
  return(list("train" = data.train, "test" = data.test))
}

