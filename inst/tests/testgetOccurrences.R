context("Test getOccurrences")
dt <- getOccurrences(tvks="NBNSYS0000007073", datasets="SGB00001", 
                     startYear="1990", endYear="2010")
test_that("Check that we get some data back", {
    expect_that(nrow(dt) > 0, is_true()) 
    expect_that(sum(which(dt$absence == TRUE)), equals(0)) ## no absences
})
