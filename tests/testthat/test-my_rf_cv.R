test_that("types are correct", {
  expect_type(my_rf_cv(7), "double")
})
test_that("expecting error", {
  expect_error(my_rf_cv("hello"))
})
