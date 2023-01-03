library(testthat)
library(inLabZero)

context("fun 'heat' check")

test_that("'center' cluster averaging", {
      
    expect_equal(heat(wbm)[wafer=="center"]$mat[[1]][1,13], 
    	mean(wbm[center>0]$mat[[1]][,13]), tolerance=1e-3)

})
