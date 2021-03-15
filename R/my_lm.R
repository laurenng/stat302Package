
#' linear model fit function
#'
#' This function conducts a linear model function on the given formula and data
#'
#' @param formula formula for the linear model (ie: lifeExp ~ gdpPercap + pop)
#' @param data data frame the formula is based on
#' @keywords linear model
#'
#' @return table with rows for each coefficient and columns for the Estimate,
#'         Std. Error, t value, and Pr(>|t|)
#'
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
#'
#' @export

my_lm <- function(formula, data) {
  # fitting data into a matrix
  model_frame <- model.frame(formula, data)
  x_mat <- model.matrix(formula, data)
  y <- model.response(model_frame)
  trans_x <- t(x_mat)

  # calculating the estimate aka beta hat
  inverse_paren <- solve(trans_x %*% x_mat)
  beta_hat <- inverse_paren %*% trans_x %*% y
  estimate <- beta_hat

  # counting number of variables in formula aka the covariates
  formula_var <- as.character(stringr::fun(str_split(formula, " ~ ")[3]))
  right_variables <- str_split(formula_var, " \\+ ")
  covariates <- sapply(right_variables, length) + 1

  # calculating sigma squared
  formula_top <- (y - (x_mat %*% beta_hat))^2
  df <- nrow(data) - covariates
  sigma_squared <- sum(formula_top / df)

  # calculating standard error
  standard_error_formula <- sqrt(abs(sigma_squared * inverse_paren))
  standard_error <- diag(standard_error_formula)

  # calculating t-statistics
  t_stat <- estimate / standard_error

  # calculating p-value
  pvalue <- pt(abs(t_stat), df, lower.tail = FALSE) * 2

  # putting it all together
  estimate_error_bind <- cbind(estimate, standard_error)
  stat_pval_bind <- cbind(t_stat, pvalue)
  result <- cbind(estimate_error_bind, stat_pval_bind)

  # adding column names to table
  colnames(result) <- c("Estimate", "Std. Error", " t value", "Pr(>|t|)")
  return(result)
}
