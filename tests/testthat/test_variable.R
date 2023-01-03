library(testthat)
library(inLabZero)

context("fun 'variable' check")


test_that("add new variable and check dataType", {
    
    myPARAMData <- 2*pcy[,PARAM02]
	pcy2 <- variable(pcy, "myPARAM", myPARAMData, "PC")
	expect_identical(pcy2[,myPARAM], myPARAMData)
	expect_true("myPARAM" %in% dataType(pcy2)$PC)	    
})


