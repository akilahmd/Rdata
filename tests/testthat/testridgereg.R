#'@import MASS
library(testthat)
library(Regression)
library(MASS)


test_that("Checking if the output is same",{
  
  expect_that(as.numeric(ridgereg(formula = Petal.Length ~ Species , data = iris,lambda = 0)$rid_beta)[1],
              equals(as.numeric(coef(lm.ridge(formula = Petal.Length ~ Species, data = iris,lambda = 0))[1])))
  expect_that(as.numeric(ridgereg(formula = Petal.Length ~ Species , data = iris,lambda = 0)$rid_beta)[2],
              equals(as.numeric(coef(lm.ridge(formula = Petal.Length ~ Species, data = iris,lambda = 0))[2])))
  
  }
)

test_that('Expecting error for wrong output',{
  expect_error(ridgereg(formula = as.formula(Petal.Length ~ Petal.Width), data = iris,lambda = 'r'))
  expect_error(ridgereg(formula = as.formula(k), data = iris,lambda = 0))
  expect_error(ridgereg(formula = as.formula(Petal.Length ~ Petal.Width), data = 0,lambda = 'r'))
}
)