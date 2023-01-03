library(testthat)
library(inLabZero)

context("fun 'yield' check")

test_that("check with demo data yield", {
    
    wbm2 <- yield(wbm, fromBin = "all", allName = "ProbeYieldTest")
    expect_identical(wbm2[,ProbeYieldTest], wbm[,ProbeYield])
    
})




