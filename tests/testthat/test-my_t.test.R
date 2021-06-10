x <- rnorm(10, mean = 0, sd = 1)

test_that("works for two-sided", {
  result <- t.test(x, mu = 0, alternative = "two.sided")
  my_result <- my_t.test(x, "two.sided", 0)
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative, "two.sided")
})

test_that("works for greater", {
  result <- t.test(x, mu = 0, alternative = "greater")
  my_result <- my_t.test(x, "greater", 0)
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative, "greater")
})

test_that("works for less", {
  result <- t.test(x, mu = 0, alternative = "less")
  my_result <- my_t.test(x, "less", 0)
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative, "less")
})

test_that("throws an error when input requirements aren't meet", {
  expect_error(my_t.test(x, "hi", 0))
})

test_that("the output is a list", {
  expect_is(my_t.test(x, "two.sided", 0), "list")
})
