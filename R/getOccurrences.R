#' Get occurrences for a given species
#' 
#' Gets publicly available occurrences from the NBN for a species (or list
#' of species) specified by Taxon Version Keys in the \code{tvks} parameter.
#' Alternativly a group can be specified to \code{group} as a string, such as
#' 'reptile' (see \code{\link{listGroups}}). To get access to data you must first
#' register at https://data.nbn.org.uk/User/Register. You will need your username and
#' password when running this function for the first time.
#' You can also specify a (list of) dataset key(s) in the \code{datasets}
#' parameter for the datasets from which you want the data to come (otherwise
#' all publicly available data will be returned) and the earliest and/or latest 
#' years for which you want data in the \code{startYear} and \code{endYear}
#' parameters (otherwise occurrences at any date will be returned). A vice-county
#' can be specified by passing a name to \code{VC} (see \code{\link{listVCs}}).
#'
#' @export
#' @param tvks a list of TVKs which are strings of 16 alphanumeric characters.
#' You can look these up using \code{getTVKQuery}
#' @param datasets a list of dataset keys which are strings of 8 alphanumeric 
#'   characters
#' @param startYear a 4 digit integer year
#' @param endYear a 4 digit integer year
#' @param VC a string giving a vice-county name (see \code{\link{listVCs}})
#' @param group a string giving the name of a group (see \code{\link{listGroups}})
#' @return a data.frame of occurence records
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @seealso \code{\link{getFeature}}, \code{\link{getTaxon}}
#' @examples \dontrun{ 
#'  occ <- getOccurrences(tvks="NBNSYS0000007073", datasets="SGB00001", 
#'                        startYear="1990", endYear="2010")
#'                        
#'  occ <- getOccurrences(group="quillwort", startYear="1990", endYear="2010",
#'                        VC="Shetland (Zetland)")
#' }
#' 
getOccurrences <- function(tvks=NULL, datasets=NULL, startYear=NULL, 
                           endYear=NULL, VC=NULL, group=NULL, ...) {
    
    if(!is.null(tvks) & !is.null(group)) stop('group and tvks cannot be used at the same time')
    
    ## If you have more than 5 TVKs break it up into batches of 5
    # Set up parameters
    tvks <- unique(tvks)
    nTVK <- length(tvks)
    start <- 1
    d_master <- NULL
    
    while(start <= nTVK){
        
        cat('Requesting batch', ceiling(start/2), 'of', ceiling(nTVK/2),'\n', sep=' ')
        end <- start + 1
        temp_tvks <-  na.omit(tvks[start:end])
        
        ## return a JSON object (list of lists)
        json <- runnbnurl(service="obs", tvks=temp_tvks, datasets=datasets, 
                          startYear=startYear, endYear=endYear, VC=VC, group=group) 
        
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
            d <- as.data.frame(d, stringsAsFactors = F)
            
            if(is.null(d_master)){d_master <- d} else{d_master <- merge(d_master, d, all = TRUE)}
                                
            }
        
            start <- start + 2
        
        }#end of while

        ## we are only interested in presences (not absences)
        if ("absence" %in% colnames(d_master)) {
            d_master <- d_master[which(d_master$absence == FALSE),]
        }
        
        ## Format date columns as dates
        if ("startDate" %in% colnames(d_master)) d_master$startDate <- as.Date(d_master$startDate)
        if ("endDate" %in% colnames(d_master)) d_master$endDate <- as.Date(d_master$endDate)
                
        return(d_master)
    }