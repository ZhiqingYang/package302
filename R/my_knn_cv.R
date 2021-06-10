#' KNN Cross-Validatin
#'
#' It uses the k-nearest-neighbors method to predict the class.
#'
#'
#' @param train Input data frame.
#' @param cl A data frame that contains the true class value of the
#'   input training data \code{train}.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list containing the following objects:
#' \itemize{
#'   \item class: objects class that contains a vector of the predicted
#'                class for all observations.
#'   \item cv_error: a numeric represents cross-validation misclassification error.
#' }
#'
#' @import class
#' @import magrittr
#' @import tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr contains
#'
#' @examples
#'
#' my_knn_cv(penguins[, -5], penguins[, 5], 5, 1)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # randomly assign to fold
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  train <- train %>% mutate(fold = fold)

  # randomly assigns observations of true class value with folds
  cl <- tibble("class" = cl, "fold" = fold)

  # output that records predicted class
  output <- list("class" = NA, "cv_error" = NA)

  # create a vector to store error for each iteration
  cv_error <- rep(NA, k_cv)

  # loop through the fold
  for (i in 1:k_cv) {
    # get the training data
    data_train <- train %>% filter(fold != i) %>% select(-contains("fold"))
    # get the test data
    data_test <- train %>% filter(fold == i) %>% select(-contains("fold"))
    # get the true class value of training data
    cl_train <- cl %>% filter(fold != i) %>% select(-contains("fold"))
    # get the true class value of test data
    cl_test <- cl %>% filter(fold == i) %>% select(-contains("fold"))
    # predict the class for test data
    prediction <- knn(train = data_train, test = data_test, cl = cl_train$class,
                      k = k_nn)
    # caLculate the misclassification error
    cv_error[i] <- (sum(as.vector(prediction) != cl_test)) / nrow(cl_test)
  }

  # train the model with the full data
  output$class <- knn(train = train, test = train, cl = cl$class, k = k_nn)
  # average misclassification rate
  output$cv_error <- round(mean(cv_error), 5)
  return(output)
}
