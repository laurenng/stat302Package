test_that("coefficients are correct", {
  lifeExp <- my_gapminder$lifeExp
  myTest <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
  correctTest <- lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)

  # is this all i have to do??
  myEstimate <- myTest[,"Estimate"]
  correctEstimate <- correctTest["coefficients"]

  expect_equal(myEstimate, correctEstimate)
})
