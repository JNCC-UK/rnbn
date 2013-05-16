#' Get occurrences for a given species
#' 
#' Gets publicly available occurrences from the NBN for a species (or list
#' of species) specified by Taxon Version Keys in the \code{tvks} parameter.
#' You can also specify a (list of) dataset key(s) in the \code{datasets}
#' parameter for the datasets from which you want the data to come (otherwise
#' all publicly available data will be returned) and the earliest and/or latest 
#' years for which you want data in the \code{startYear} and \code{endYear}
#' parameters (otherwise occurrences at any date will be returned).
#'
#' @export
#' @param tvks a list of TVKs which are strings of 16 alphanumeric characters
#' @param datasets a list of dataset keys which are strings of 8 alphanumeric 
#'   characters
#' @param startYear a 4 digit integer year
#' @param endYear a 4 digit integer year
#' @return a data.frame of occurence records
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @seealso \code{\link{getFeature}}, \code{\link{getTaxon}}
#' @examples \dontrun{ 
#'  occ <- getOccurrences(tvks="NBNSYS0000007073", datasets="SGB00001", 
#'                        startYear="1990", endYear="2010")
#' }
#' 
getOccurrences <- function(tvks=NULL, datasets=NULL, startYear=NULL, 
                           endYear=NULL) {
    
    ## return a JSON object (list of lists)
    json <- runnbnurl(service="obs", tvks=tvks, datasets=datasets, 
                      startYear=startYear, endYear=endYear) 
    
    if (length(json) > 0) {
        ## find the unique names that are used in occ
        n <- unique(unlist(c(sapply(json, names))))
        ## dimension a matrix for the required number of rows and cols
        d <- matrix(nrow=length(json), ncol=length(n), 
                    dimnames=list(seq(1:length(json)),n))
        ## now we can go through the list and insert
        ## the values into the correct cells of the matrix
        ## This should be quick because the matrix is pre-allocated
        for (i in 1:length(json)) {
            for (j in 1:length(json[[i]])) {
                k <- grep(names(json[[i]][j]),n)
                d[i,k] <- json[[i]][[j]]
            }
        }
        ## cooerce the matrix to a data.frame
        d <- as.data.frame(d)
        ## we are only interested in presences (not absences)
        if ("absence" %in% colnames(d)) {
            d <- d[which(d$absence == FALSE),]
        }
        return(d)
    }
}
