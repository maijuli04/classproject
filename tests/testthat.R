library(testthat)
library(farspackage)

test_check("farspackage")

testfile <- make_filename(2013)
expect_is(testfile,"character")
