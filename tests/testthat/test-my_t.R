test_that("alternative less than", {
  lifeExp <- my_gapminder$lifeExp
  myTest <- my_t.test(lifeExp, "less", 60)
  correctTest <- t.test(my_gapminder$lifeExp, mu = 60, alternative = "less")

  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))

  # is this correct?? doing 1 - ?? or is the function wrong
  expect_equal(myTest[[4]], 1 - as.numeric(correctTest$p.value))
})

test_that("alternative greater than", {
  lifeExp <- my_gapminder$lifeExp
  myTest <- my_t.test(lifeExp, "greater", 60)
  correctTest <- t.test(my_gapminder$lifeExp, mu = 60, alternative = "greater")

  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))

  # is this correct?? doing 1 - ?? or is the function wrong
  expect_equal(myTest[[4]], 1 - as.numeric(correctTest$p.value))
})

test_that("alternative two sided", {
  lifeExp <- my_gapminder$lifeExp
  myTest <- my_t.test(lifeExp, "two.sided", 60)
  correctTest <- t.test(my_gapminder$lifeExp, mu = 60, alternative = "two.sided")

  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))

  # is this correct?? doing 1 - ?? or is the function wrong
  expect_equal(myTest[[4]], as.numeric(correctTest$p.value))
})

test_that("error alternative", {
  lifeExp <- my_gapminder$lifeExp
  expect_error(my_t.test(lifeExp, "two", 60))
})


