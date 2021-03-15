test_that("works accordingly", {

  data <- my_penguins %>%
            drop_na()
  train_data <- data %>%
    dplyr::select(bill_length_mm, bill_depth_mm,
           flipper_length_mm, body_mass_g)
  target_class <- data$species

  myTest <- my_knn_cv(train_data, target_class, 2, 2)


})
