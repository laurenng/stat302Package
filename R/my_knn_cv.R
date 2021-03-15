#' K-nearest Neighbors Cross Validation Function
#'
#' This function performs knn cross validation on a set of data.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords knn_cv
#'
#' @return list with the following elements:
#'           class: a vector of the predicted class y for all observations
#'           cv_err: a numeric with the cross-validation misclassification error
#'
#' @examples
#' data <- penguins %>%
#'         drop_na()
#' train_data <- data %>%
#'               select(bill_length_mm, bill_depth_mm,
#'                      flipper_length_mm, body_mass_g)
#' target_class <- data$species
#'
#' my_knn_cv(train_data, target_class, alternative = 2, mu = 2)
#'
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # finding length of dataset we're working with
  n <- length(cl)

  # splitting data randomly according to folds
  fold <- sample(rep(1:k_cv, length = n))

  # storing predictions
  class <- c() # matrix(NA, n, 1)
  miscalc_all <- list()
  for (i in 1:k_cv) {
    # consider all data expect for the current fold
    X_train <- train[fold != i, ]
    X_test <- train[fold == i, ]

    Y_train <- cl[fold != i]
    Y_test <- cl[fold == i]

    # perform knn_predictions to find the y test variable
    knn_prediction <- knn(X_train, X_test, Y_train, k=k_nn)

    # Record predictions
    class <- append(class, knn_prediction)

    # calculating misclaculation rate and adding to list
    miscalc_rate <- mean(knn_prediction != Y_test)
    miscalc_all[[i]] <- miscalc_rate
  }

  # converting numeric values in class to the string equivalent
  labeled_class <- c()
  for (i in 1:n) {
    current <- class[[i]]
    if (current == 1) {
      labeled_class <- append(labeled_class, "Adelie")
    } else if (current == 2) {
      labeled_class <- append(labeled_class, "Chinstrap")
    } else {
      labeled_class <- append(labeled_class, "Gentoo")
    }
  }

  # finding average of all miscalculation rates
  cv_error <- mean(unlist(miscalc_all))

  # creating list of class and cv_error
  result <- list("class" = labeled_class,
                 "cv_error" = cv_error)
  return(result)
}
