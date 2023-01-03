library(testthat)
library(inLabZero)

context("fun 'rename' check")


test_that("multiple rename", {
    
  pcy2 <- rename(pcy, c("clear", "center", "PARAM01"), c("clear2", "center2", "PARAM99"))
  idx <- which(dataType(pcy2)$C %in% c("clear", "center"))
  expect_identical(dataType(pcy2)$C[idx], dataType(pcy)$C[idx])

})
