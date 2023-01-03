library(testthat)
library(inLabZero)

context("fun 'unfoldCol' check")

test_that("unfold column", {
    
	data(pcy)
	pcyu <- unfoldCol(pcy, "siteid", method="merge", key="lotWafer", dataType="PC", discardCol=c("siteid", "lotWaferSiteid"))
	pcParam <- dataType(pcy)$PC
	pcuParam <- paste0("1_", dataType(pcy)$PC)
	pcVal <- as.matrix(pcy$tab[lotWaferSiteid=="LOT001-W12-S01", pcParam, with=FALSE])[1,]
	pcuVal <- as.matrix(pcyu$tab[lotWafer=="LOT001-W12", pcuParam, with=FALSE])[1,]
	names(pcVal) <- NULL
	names(pcuVal) <- NULL

    expect_equal(pcVal, pcuVal, tolerance=1e-3)
  	    
})
