test_that("coefficients are correct", {
  myTest <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
  correctTest <- lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)

  myEstimate <- myTest[,"Estimate"]
  correctEstimate <- correctTest$coefficients

  expect_equal(myEstimate, correctEstimate)
})
test_that("structures are good", {
  myTest <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)

  expect_type(myTest, "double")
})
test_that("expect errors", {
  expect_error(my_lm("potatoes", data = my_gapminder))
})
