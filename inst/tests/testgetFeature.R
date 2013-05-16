context("Test getFeature")
test_that("Get details for a feature", {
    expect_equal(as.character(getFeature("97479")['label']), "SN413499")    
})
