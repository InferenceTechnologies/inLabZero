library(testthat)
library(inLabZero)

context("fun 'degrade' check")

test_that("wafer level", {
      
    expect_equal(degrade(pcy, "wafer", fun=median)[lotWafer=="LOT001-W12", PARAM01], 
    	median(pcy[lotWafer=="LOT001-W12",PARAM01]), tolerance=1e-3)

})

test_that("lot level", {
      
    expect_equal(degrade(pcy, "lot", fun=mean)[lot=="LOT001", PARAM01],
		mean(pcy[lot=="LOT001",PARAM01]), tolerance=1e-3)

})