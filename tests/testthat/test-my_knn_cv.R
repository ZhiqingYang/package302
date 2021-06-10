# test data
data("iris")
train <- iris[, -5]
# true class
cl <- iris[, 5]
test_that("works when k_nn_1", {
  # predict classes when k_nn = 1
  my_pred_1 <- my_knn_cv(train, cl, 1, 5)
  # training test error
  train_error_1 <- sum(my_pred_1$class != cl)
  # expect training test error is 0
  expect_equal(train_error_1, 0)
})



