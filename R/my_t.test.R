#' t-test
#'
#' Performs a one sample t-test on vectors of data.
#'
#' @param data A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis, must be
#'   one of the "two.sided (default)", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#' @keywords t-test
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item test_stat: the numeric test statistic;
#'   \item df: the degrees of freedom;
#'   \item alternative: the value of the parameter alternative;
#'   \item p_val: the numeric p-value.
#' }
#'
#' @examples
#' my_t_test(data = 1:5, alt = "greater", mu = 3)
#'
#'
#' @importFrom stats sd
#' @importFrom stats pt
#'
#' @export
my_t.test <- function(data, alternative, mu) {
  #check if the alternative satisfies the requirment
  if( alternative == "two.sided" | alternative == "less" | alternative == "greater") {
    degree <- length(data) - 1
    test_stat <- (mean(data) -mu) * sqrt(length(data)) / sd(data)

    # check each case
    if(alternative == "less") {
      p_val <- pt(test_stat, df = degree)

    } else if (alternative == "two.sided") {
      p_val <-  pt(abs(test_stat), df = degree, lower.tail = FALSE) + pt(-abs(test_stat), df = degree)

    } else {
      p_val <- 1 - pt(test_stat, df = degree)
    }
    y <- list("test_stat" = round(test_stat, 5) , "df" = degree, alternative = alternative, "p_val" = round(p_val,4))
    return(y)

  } else {
    # throw an error if the alternative is wrong
    stop("alternative must be \"two.sided\", \"less\", or \"greater\"")
  }
}
