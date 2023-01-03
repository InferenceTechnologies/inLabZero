library(testthat)
library(inLabZero)

context("fun 'outlierFilter' check")


test_that("logical output", {

	data(pcy)

	vec <- pcy[,PARAM01]
	idx <- sample(1:length(vec), 6)
	vec[idx[4:6]] <- quantile(vec, 0.25) - 4*IQR(vec)
	vec[idx[1:3]] <- quantile(vec, 0.75) + 5*IQR(vec)
	pcy2 <- variable(pcy, "PARAM01", vec)
	outlier <- outlierFilter(pcy2, "PARAM01", k = c(3,4), output="logical")
	expect_true(all(idx%in%which(!outlier)))

})
