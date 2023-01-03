library(testthat)
library(inLabZero)

context("fun 'modelM' check")


test_that("class check", {

	data(pcy)
	pcy2 <- model(pcy, type="class", model="cart")
	expect_true(inherits(modelM(pcy2), "rpart"))

})
