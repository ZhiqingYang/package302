# load the global variable
data("my_penguins", envir = environment())
my_penguins <- na.omit(my_penguins)
train_data <- my_penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
#' Random Forest Cross-Validation
#'
#' Predicts penguin species using random forest methods.
#'
#' @param k Integer representing Number of folds.
#' @keywords prediction
#'
#' @return A numeric with the cross-validation error.
#'
#' @import class
#' @import randomForest
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr contains
#' @importFrom stats predict
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # randomly assign to fold
  fold <- sample(rep(1:k, length = nrow(train_data)))
  train_data$fold <- fold

  # create a vector to store error for each iteration
  cv_error <- c()

  # loop through each fold
  for (i in 1:k) {
    # separate training and testing set
    data_training_i <- train_data[fold != i, 1:4]
    data_testing_i <- train_data[fold == i, 1:4]

    # create random forest model
    rf_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_training_i, ntree = 100)

    # use the random forest model to predict the testing (validation) set
    pred<- predict(rf_model, data_testing_i[, 1:3])

    # compute MSE
    cv_error[i] <- mean((as.vector(pred) - data_testing_i$body_mass_g)^2)
  }

  # return the average MSE across folds
  return(as.numeric(mean(cv_error)))
}
