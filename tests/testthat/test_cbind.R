library(testthat)
library(inLabZero)

context("fun 'cbind' check")


test_that("dataType check", {
    
	pcyX <- rename(pcy, dataType(pcy)$PC, paste0("x_", dataType(pcy)$PC))
	pcyY <- rename(pcy, dataType(pcy)$PC, paste0("y_", dataType(pcy)$PC))
	pcyB <- cbind(pcyX, pcyY[,dataType(pcyY)$PC, with=FALSE])
	expect_identical(dataType(pcyB)$PC, c(dataType(pcyX)$PC, dataType(pcyY)$PC))

})
