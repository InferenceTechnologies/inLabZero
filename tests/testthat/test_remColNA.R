library(testthat)
library(inLabZero)

context("fun 'remColNA' check")


test_that("remove column with NA values", {
    
   pcy2 <- variable(pcy, "myPARAM", NA ,"PC")
   pcy2 <- remColNA(pcy2)  
   expect_false("myPARAM" %in% colnames(pcy2))  
})
