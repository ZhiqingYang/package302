test_that("output is a number", {
  expect_type(my_rf_cv(5), "double")
})
