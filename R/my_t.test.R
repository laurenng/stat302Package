#' T-test function
#'
#' This function performs a t-test with the given parameters:
#' vector, alternative hypothesis, and mu number.
#'
#' @param x a numeric vector of data
#' @param alternative a character string specifying the alternative hypothesis.
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords t-test, inference
#'
#' @return list with the following elements: numeric test statistic,
#'         degree of freedom, value of alternative parameter, and p-value
#'
#' @examples
#' my_t.test(stat302Package::my_gapminder$lifeExp, alternative = "two.sided", mu = 2)
#'
#' @export

my_t.test <- function(x, alternative, mu) {
  if (alternative != "two.sided"
      && alternative != "less"
      && alternative != "greater") {
    stop("Please provide an alternative string of two.sided, less, or greater")
  }

  # calculating t statistics
  mean <- mean(x)
  se <- sd(x) / sqrt(length(x))
  t_stat <- (mean - mu) / se

  # calculating degrees of freedom
  df <- length(x) - 1

  # performing the correct pvalue depending on the alternative value
  if (alternative == "two.sided") {
    pvalue <- pt(abs(t_stat), df, lower.tail = FALSE) * 2
  } else if (alternative == "less") {
    pvalue <- 1 - pt(abs(t_stat), df, lower.tail = FALSE)
  } else {
    pvalue <- pt(abs(t_stat), df, lower.tail = FALSE)
  }

  # returning the list
  result <- list("test_stat" = t_stat,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = pvalue)
  return(result)
}
