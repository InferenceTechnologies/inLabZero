library(testthat)
library(inLabZero)

context("fun 'transfer' check")


test_that("from wbm to pcy", {
    
    wbm2 <- variable(wbm, "edge2", wbm[,edge], "C")
	pcy2 <- transfer(wbm2, pcy, "edge2")
	expect_identical(pcy2[,edge], pcy2[,edge2])
    
})

