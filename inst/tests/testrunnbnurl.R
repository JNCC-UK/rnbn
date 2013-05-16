context("Test runnbnurl")
test_that("runnbnurl gets a response", {
    expect_that(length(runnbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001", startYear="1990", endYear="2010")) > 0, is_true()) 
})

    