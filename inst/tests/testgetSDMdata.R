context("Test getSDMdata")
sp <- data.frame(tvk="NBNSYS0000002987",
                 name="Silene uniflora",
                 stringsAsFactors = FALSE)
dst <- c("GA000373","GA000483","GA000152")
syr <- "1990" 
eyr <- "2010"

test_that("Invalid parameters are detected", {
    expect_error(getSDMdata(sp, dst, syr, eyr, gridSystem="osgb"), "Error in match.arg.*")
    expect_error(getSDMdata(sp, dst, syr, eyr, units="inches"), "Error in match.arg.*")
    expect_error(getSDMdata(datasets=dst, startYear=syr, endYear=eyr), "species parameter required")
    expect_error(getSDMdata("NBNSYS0000007094", dst, syr, eyr), "tvk column required in species parameter")
    expect_error(getSDMdata(data.frame(tvk="NBNSYS0000007094"), dst, syr, eyr), "name column required in species parameter")
})

fname <- paste(tempdir(), "/vol_bomb.csv", sep="")
if (file.exists(fname)) file.remove(fname)
dt <- getSDMdata(sp, dst, syr, eyr, includeNames=TRUE, file=fname)
test_that("getSDMdata worked as expected", {
    expect_that(nrow(dt) > 0, is_true()) # test we got some data
    expect_that(dt, is_a("data.frame"))
    expect_that(("name" %in% colnames(dt)), is_true())
    expect_that(("x" %in% colnames(dt)), is_true())
    expect_that(("y" %in% colnames(dt)), is_true())
    expect_that(unique(dt$name), equals("Silene uniflora"))
    expect_that(min(dt$x) > 0, is_true())
    expect_that(max(dt$x) < 700, is_true())
    expect_that(min(dt$y) > 0, is_true())
    expect_that(max(dt$y) < 1250, is_true())
    expect_that(file.exists(fname), is_true())
})

dt2 <- getSDMdata(sp, dst, syr, eyr, includeNames=FALSE, unique=FALSE)
test_that("getSDMdata unique and includeNames", {
    expect_that(("name" %in% colnames(dt2)), is_false()) ## we shouldn't have a name column now
    expect_that(("x" %in% colnames(dt2)), is_true())
    expect_that(("y" %in% colnames(dt2)), is_true())
    expect_that(nrow(dt2) > nrow(dt), is_true()) ## dt2 should have duplicates
    expect_that(nrow(dt) == nrow(dt2[!duplicated(dt2),]), is_true()) ## if we remove the duplicates from dt2 we should have the same rows at dt
})
rm(dt2)

# implement this test when all data is available
# dt2 <- getSDMdata(sp, dst, syr, eyr, precision=0, units="m")
# test_that("getSDMdata precision and units", {
#     expect_that(nrow(dt2) > nrow(dt), is_true()) ## dt2 should include 10kms
#     expect_that(min(dt2$x) > 0, is_true())
#     expect_that(max(dt2$x) < 700, is_false()) ## should now be in m
#     expect_that(max(dt2$x) < 700000, is_true())
#     expect_that(min(dt2$y) > 0, is_true())
#     expect_that(max(dt2$y) < 1250, is_false()) ## should now be in m
#     expect_that(max(dt2$y) < 1250000, is_true())
#     
# })
#rm(dt, dt2)
    
sp <- data.frame(tvk=c("NBNSYS0000002987", "NBNSYS0000003107","NBNSYS0000002774"),
                 name=c("Silene uniflora", "Beta vulgaris subsp. maritima", "Glaucium flavum"),
                 stringsAsFactors = FALSE)
dt <- getSDMdata(sp, dst, syr, eyr)
test_that("getSDMdata multiple species", {
    expect_that(("name" %in% colnames(dt)), is_true()) ## we have more than one species, so names should be output
    expect_that(("x" %in% colnames(dt)), is_true())
    expect_that(("y" %in% colnames(dt)), is_true())
    expect_that(is.unsorted(dt$name), is_false()) ## should be sorted by species
})

