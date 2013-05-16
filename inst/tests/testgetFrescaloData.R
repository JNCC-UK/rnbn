context("Test getFrescaloData")
sp <- data.frame(tvk=c("NBNSYS0000007073","NBNSYS0000007022","NBNSYS0000006865","NBNSYS0000006868"),
                 name=c("Pelecocera tricincta","Lejogaster metallina","Melanostoma dubium","Melanostoma dubium"),
                 stringsAsFactors = FALSE)
dst <- c("SGB00001","GA000483","GA000152")
p <- list()
p$breakYear <- seq(from=1980, to=2012, by=2)
p$plabel <- as.character(seq(from=1980, to=2010, by=2))
fname <- paste(tempdir(), "/frescalo_test.csv", sep="")
if (file.exists(fname)) file.remove(fname)

test_that("Invalid parameters are detected", {
    expect_error(getFrescaloData(file=fname, species=sp, datasets=dst, periods=p, location="10km"), "Error in match.arg.*")
    expect_error(getFrescaloData(file=fname, species=sp, datasets=dst, periods=p, gridSystem="osgb"), "Error in match.arg.*")
    expect_error(getFrescaloData(file=fname, species="NBNSYS0000007073", datasets=dst, periods=p), "tvk column required in species parameter")
})

getFrescaloData(file=fname, species=sp, datasets=dst, periods=p)
dt <- read.csv(fname, as.is=TRUE)
n <- nrow(dt)
test_that("getFrescaloData worked as expected", {
    expect_that(file.exists(fname), is_true())
})

getFrescaloData(file=fname, species=sp, datasets=dst, periods=p, gridSystem="OSGB")
dt <- read.csv(fname, as.is=TRUE)
test_that("getFrescaloData worked as expected when restricted to GB", {
    expect_that(file.exists(fname), is_true())
    expect_that(n > nrow(dt), is_true()) ## some Irish data should have been filtered
})


