#' Run the constructed url and get data
#' 
#' The URL to call a web-service is constructed using makenbnurl()
#' This is run and, hopefully, returns a JSON object
#' 
#' @export
#' @import RCurl
#' @import RJSONIO
#' @param service the service you want to call. One of \code{"obs"} for the 
#'   taxonObservations service, \code{"feature"} for the features service or 
#'   \code{"taxon"} for the taxa service. The first letter is sufficient
#' @param tvks a list of TVKs which are strings of 16 alphanumeric characters
#' @param datasets a list of dataset keys which are strings of 8 alphanumeric 
#'   characters
#' @param feature a featureID which is a string of 8 alphanumeric characters
#' @param startYear a 4 digit integer year
#' @param endYear a 4 digit integer year
#' @return a JSON object resulting from the call
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @examples \dontrun{ 
#'  json <- runnbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001", startYear="1990", endYear="2010")
#' }
#' 
runnbnurl <- function(service=NULL, tvks=NULL, datasets=NULL, feature=NULL,
                       startYear=NULL, endYear=NULL) {
    
    url <- makenbnurl(service, tvks, datasets, feature, startYear, endYear)
    if (url.exists(url)) {
        resp <- getURL(url)
        return(fromJSON(resp, asText=TRUE))
    } else {
        stop("url not found")
    }
}
