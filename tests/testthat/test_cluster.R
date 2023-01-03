library(testthat)
library(inLabZero)

context("fun 'cluster' check")

test_that("full from -> to: target", {
      
    expect_equal(as.logical(cluster(wbm, from=clear, to=center)[,center]),
    	(wbm[,clear]==1|wbm[,center]==1))

})

test_that("full from -> to: source add=FALSE", {
      
    expect_false(any(cluster(wbm, from=clear, to=center, add=FALSE)[,clear]==1))

})
