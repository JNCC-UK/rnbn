context("Test getTaxon")
test_that("Get details for a taxon", {
    expect_equal(as.character(getTaxon("NBNSYS0000007094")['name']), "Volucella bombylans")    
})
