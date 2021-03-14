test_that("fit correctly", {
  lifeExp <- my_gapminder$lifeExp
  myTest <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
  correctTest <- lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
})
