#' Get occurrences for a given species
#' 
#' Gets publicly available occurrences from the NBN for a species (or list
#' of species) specified by Taxon Version Keys in the \code{tvks} parameter (see \code{\link{getTVKQuery}}).
#' Alternativly a group can be specified to \code{group} as a string, such as
#' 'reptile' (see \code{\link{listGroups}}). To get access to data you must first
#' register at \url{https://data.nbn.org.uk/User/Register}. You will need your username and
#' password when running this function for the first time.\cr
#' You can also specify a (list of) dataset key(s) in the \code{datasets}
#' parameter for the datasets from which you want the data to come (otherwise
#' all publicly available data will be returned) and the earliest and/or latest 
#' years for which you want data in the \code{startYear} and \code{endYear}
#' parameters (otherwise occurrences at any date will be returned). A vice-county
#' or grid reference can be specified by passing a name to \code{VC}
#' (see \code{\link{listVCs}}), or \code{gridRef}.
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
#' @param gridRef a string giving a gridreference in which to search for occurrences
#' @param acceptTandC if set to \code{TRUE} you accept the NBN gateway terms and 
#' conditions and privacy policy. These can be found at \url{https://data.nbn.org.uk/Terms}.
#' Accepting the terms and conditions supresses the corresponding warning message.
#' @param silent If TRUE batch request information is supressed
#' @return a data.frame of occurence records
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @seealso \code{\link{getFeature}}, \code{\link{getTaxon}}
#' @examples \dontrun{ 
#'  dt1 <- getOccurrences(tvks="NBNSYS0000002987", datasets="GA000373", 
#'                        startYear="1990", endYear="1991")
#'                       
#'  dt2 <- getOccurrences(tvks=c("NBNSYS0000002987","NHMSYS0001688296","NHMSYS0000080210"),
#'                        startYear="1990", endYear="1991")
#'                        
#'  dt3 <- getOccurrences(group="quillwort", startYear="1990", endYear="2010",
#'                        VC="Shetland (Zetland)")
#' }
#' 
getOccurrences <- function(tvks=NULL, datasets=NULL, startYear=NULL, 
                           endYear=NULL, VC=NULL, group=NULL, gridRef=NULL,
                           acceptTandC=FALSE, silent=FALSE) {
    
    if(!is.null(tvks) & !is.null(group)) stop('group and tvks cannot be used at the same time')
    if(is.null(tvks) & is.null(group) & is.null(gridRef)) stop('One of group, tvks or gridRef must be given')
    
    # If we are searching by group get the group tvks
    if(!is.null(group)) tvks <- getGroupSpeciesTVKs(group)
        
    ## If you have more than 5 TVKs break it up into batches of 5
    # Set up parameters
    if(!is.null(tvks)){
        tvks <- unique(tvks)
        nTVK <- length(tvks)
    } else {
        tvks <- 1
        nTVK <- 1
    }
    
    start <- 1
    d_master <- NULL
    
    while(start <= nTVK){
        
        if(!silent) cat('Requesting batch', ceiling(start/2), 'of', ceiling(nTVK/2),'\n', sep=' ')
        end <- start + 1
        if(!is.null(tvks)){temp_tvks <-  na.omit(tvks[start:end])}else{temp_tvks=NULL}
        
        ## return a JSON object (list of lists)
        json <- runnbnurl(service="obs", tvks=temp_tvks, datasets=datasets, 
                          startYear=startYear, endYear=endYear, VC=VC, gridRef=gridRef) 
        
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
        
        ## Write out a statement about the T's & C's
        if(!acceptTandC) warning(call.=FALSE, 'IMPORTANT: By using this package you are agreeing to the Gateway Terms & Conditions and Privacy Policy. These can be found at https://data.nbn.org.uk/Terms. This message can be supressed using the acceptTandC argument') 
    
        return(d_master)
    }