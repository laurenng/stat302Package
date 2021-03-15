#' Random Forest Cross-Validation Function
#'
#' This function performs a cross validation using the random forest package
#' to train a model. predicting \code{body_mass_g} using the covariates of
#' \code{bill_length_mm}, \code{bill_depth_mm}, and \code{flipper_length_mm}.
#'
#' @param k number of folds
#' @keywords RandomForest, Cross Validation
#'
#' @return number with the cross-validation error value
#'
#' @importFrom randomForest randomForest
#' @importFrom  tidyr drop_na
#' @examples
#' my_rf_cv(4)
#'
#' @export
my_rf_cv <- function(k) {
  # length of penguins dataset
  data <- stat302Package::my_penguins %>% drop_na()
  n <- nrow(data)

  # spliting data into different samples based on fold number
  fold <- sample(rep(1:k, length = n))

  MSE_error <- c()
  for (i in 1:k) {
    # initializing the test and train data
    train <- data[fold != i, ]
    test <- data[fold == i,]

    # train a rain forest model
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = train, ntree = 100)
    # create prediction for the species name
    prediction <- predict(model, test[, -1])

    # calculate MSE error for each row and append to vector list
    cv_err <- (prediction - test$body_mass_g)^2
    MSE_error <- append(MSE_error, cv_err)
  }

  # finding mean of all MSE errors
  mean_MSE_error <- mean(MSE_error)
  return(mean_MSE_error)
}

