#' Run the constructed url and get data
#' 
#' The URL to call a web-service is constructed using makenbnurl()
#' This is run and, hopefully, returns a JSON object
#' 
#' @export
#' @import RCurl
#' @import RJSONIO
#' @param service the service you want to call. One of \code{"obs"} for the 
#'   taxonObservations service, \code{"feature"} for the features service, 
#'   \code{"taxon"} for the taxa service, \code{"list"} for listing services,
#'   \code{"ancestry"} for taxonomy service and \code{"species"} for the 
#'   species service. The first letter is sufficient
#' @param tvks a list of TVKs which are strings of 16 alphanumeric characters
#' @param datasets a list of dataset keys which are strings of 8 alphanumeric 
#'   characters
#' @param feature a featureID which is a string of 8 alphanumeric characters
#' @param startYear a 4 digit integer year
#' @param endYear a 4 digit integer year
#' @param list url segment as a string as required to append to the base url to 
#'        give the list required as a part of the \code{"list"} service.
#' @param VC a string giving a vice-county name (see \code{\link{listVCs}})
#' @param group a string giving the name of a group (see \code{\link{listGroups}})
#' @return a JSON object resulting from the call
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @examples \dontrun{ 
#'  json <- runnbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001", startYear="1990", endYear="2010")
#' }
#' 
runnbnurl <- function(service=NULL, tvks=NULL, datasets=NULL, feature=NULL,
                       startYear=NULL, endYear=NULL, list=NULL, VC=NULL, group=NULL) {
    
    url <- makenbnurl(service=service, tvks=tvks, datasets=datasets, feature=feature,
                      startYear=startYear, endYear=endYear, list=list, VC=VC,
                      group=group)
       
    #if (url.exists(url)) { #this may slow down the function
        resp <- getURL(url)
        return(fromJSON(resp, asText=TRUE))
    #} else {
    #    stop("url not found")
    #}
}
