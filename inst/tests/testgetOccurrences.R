context("Test getOccurrences")
test_that("Errors given", {
    expect_error(getOccurrences(tvks="badger", silent=T), 'Error in makenbnurl*') 
    expect_error(getOccurrences(tvks="NBNSYS0000002987", datasets="G3", silent=T), 'Error in makenbnurl*')
    expect_error(getOccurrences(tvks="NBNSYS0000002987", datasets="GA000373", startYear="1992", endYear="1991", silent=T), 'Error in makenbnurl*')
    expect_error(getOccurrences(tvks="NBNSYS0000002987", group="reptile", silent=T), 'Error in getOccurrences*')
    expect_error(getOccurrences(silent=T), 'Error in getOccurrences*')
})
dt <- getOccurrences(tvks="NBNSYS0000002987", datasets="GA000373", 
                     startYear="1990", endYear="1991", silent = TRUE)
test_that("Check single TVK search", {
    expect_that(nrow(dt) > 0, is_true()) 
    expect_that(sum(which(dt$absence == TRUE)), equals(0)) ## no absences
    expect_that(length(unique(dt$taxonVersionKey)), equals(1))
    expect_that(unique(dt$taxonVersionKey), equals('NBNSYS0000002987'))
    expect_that(length(unique(dt$datasetKey)), equals(1))
    expect_that(unique(dt$datasetKey), equals('GA000373'))
})
rm(dt)
dt <- getOccurrences(tvks=c("NBNSYS0000002987","NHMSYS0001688296","NHMSYS0000080210"),
                     startYear="1990", endYear="1991", silent = TRUE)
test_that("Check multi TVK search", {
    expect_that(nrow(dt) > 0, is_true()) 
    expect_that(sum(which(dt$absence == TRUE)), equals(0)) ## no absences
    expect_that(length(unique(dt$pTaxonVersionKey)), equals(3))
    expect_that(sort(unique(dt$pTaxonVersionKey)), equals(c('NBNSYS0000002987','NHMSYS0000080210','NHMSYS0001688296')))
})
rm(dt)
dt <- getOccurrences(group="quillwort", startYear="1990", endYear="1992",
                     VC="Shetland (Zetland)", silent = TRUE)
test_that("Check group search", {
    expect_that(nrow(dt) > 0, is_true()) 
    expect_that(sum(which(dt$absence == TRUE)), equals(0)) ## no absences
    expect_that(length(unique(dt$pTaxonVersionKey)), equals(2))
    expect_that(sort(unique(dt$pTaxonVersionKey)), equals(c('NBNSYS0000002008','NBNSYS0000002009')))
})
rm(dt)