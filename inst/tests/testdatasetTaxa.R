context("Test datasetTaxa")

test_that("Errors given", {
    expect_error(datasetTaxa(), 'datasets parameter cannot be NULL') 
})

test_that("Warnings given", {
    expect_warning(datasetTaxa(datasets='foo'), 'Dataset foo returned no taxa, check this is spelt correctly') 
})

test_that("Taxa are returned", {
    expect_is(taxa <- datasetTaxa(datasets=c('GA001044')), 'data.frame')
    expect_that(nrow(taxa) > 0, is_true())
    expect_that('Balaenoptera acutorostrata' %in% taxa$name, is_true())
})
