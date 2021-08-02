library(farspackage)

test_that("file name is a character",{
   expect_type(make_filename(2013),"character")
})
