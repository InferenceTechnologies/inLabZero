library(testthat)
library(inLabZero)

context("fun 'colCor' check")


test_that("check correlation between two columns by lot", {
    
    expect_equal(colCor(pcy[lot=="LOT001"], "PARAM01", "PARAM02", by="lot")$gcor, 
    	cor(pcy[lot=="LOT001",PARAM01], pcy[lot=="LOT001",PARAM02]), tolerance=1e-3)
})