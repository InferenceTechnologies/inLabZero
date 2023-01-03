library(testthat)
library(inLabZero)

context("fun 'unfoldCluster' check")

test_that("unfold cluster", {
    
	data(pcy)
	pcy2 <- variable(pcy, "multiClass", round(runif(nrow(pcy), 1, 3)), "C")
	pcy2u <- unfoldCluster(pcy2, "multiClass", labels="clust", add = TRUE)

	expect_identical(pcy2u[,clust1], as.integer(pcy2[,multiClass]==1))
 	  	    
})
