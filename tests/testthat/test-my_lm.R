# test data
data(mtcars)
test <- lm(mpg ~ hp + wt, data = mtcars)

test_that("my_lm works", {
  result <- summary(test)
  my_result <- my_lm(mpg ~ hp + wt, data = mtcars)

  real <- as.data.frame(result$coefficients[, -5]) %>% round(4)
  my_result_matr <- as.matrix(my_result[, -4])
  # check whether the estimate, se, and t.value equal to the real ones
  #expect_equivalent(real, my_result_matr)
})

