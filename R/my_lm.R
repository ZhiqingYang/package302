#' linear models
#'
#' my_lm is used to fit linear models.
#'
#' @param formula A class object
#' @param data A set of data
#' @keywords prediction
#'
#' @return A dataframe of summary with rows for each coefficient and columns for the
#'   Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @import magrittr
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats pt
#'
#' @export
my_lm <- function(formula, data) {

  # extract all the objects in the formula
  model <- model.frame(formula = formula, data = data)
  # extract all independent variable
  x <- model.matrix(object = formula, data = data)
  # extract all dependent variable
  y <- as.matrix(model.response(model))

  # solve for beta
  beta <- solve(t(x) %*% x) %*% t(x) %*% y

  # get the degree of freedom
  df <- nrow(x) - ncol(x)

  # get the variance
  var <- sum((y - x %*% beta)^2 / df)

  # get the standard error of each variable
  se <- sqrt(diag(var * solve(t(x) %*% x)))

  # get the t-value
  t <- (beta / se) %>% round(4)

  # get the p-value
  pval <- pt(abs(t), df, lower.tail = FALSE) * 2

  # round beta, se, t_value to 5 decimals to display
  beta <- round(beta, 4)
  se <- round(se, 4)
  t <- round(t, 4)

  # combine all the information to one single matrix
  result <- cbind(beta, se, t, pval)
  colnames(result) <- c("Estimate", "Std.Error", "t.value", "Pr(>|t|)")

  # convert the result to a data frame in order to display as a table
  result <- data.frame(result)
  return(result)
}
