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
#' @param query a string used to perform a taxa search
#' @param gridRef a string giving a gridreference in which to search for occurrences
#' @param attributes if \code{TRUE} then attribute data is returned
#' @return a JSON object resulting from the call
#' @author Stuart Ball, JNCC \email{stuart.ball@@jncc.gov.uk}
#' @examples \dontrun{ 
#'  json <- runnbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001",
#'                    startYear="1990", endYear="2010")
#' }
#' 
runnbnurl <- function(service=NULL, tvks=NULL, datasets=NULL, feature=NULL,
                      startYear=NULL, endYear=NULL, list=NULL, VC=NULL, group=NULL,
                      query=NULL, gridRef=NULL, attributes=FALSE) {
    
    url <- makenbnurl(service=service, tvks=tvks, datasets=datasets, feature=feature,
                      startYear=startYear, endYear=endYear, list=list, VC=VC,
                      group=group, query=query, gridRef=gridRef, attributes=attributes)
    
    # Set SSL certs globally, if not done then RCurl will not accept the NBN certificate
    options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
    
    # Run login script, this checks whether the user has a cookie that the webservice 
    # knows and if not gets them to log in and stores the cookies
    nbnLogin()
        
    # set up Curl
    agent = "rnbn v0.1"
    options(RCurlOptions = list(sslversion=3L, ssl.verifypeer = FALSE))
    curl = getCurlHandle()
    #cookiePath<-'~/rnbn'
    #cookies <- paste(cookiePath, 'cookies.txt', sep = '/')
    cookies <- 'rnbn_cookies.txt'
    curlSetOpt(cookiefile = cookies, cookiejar = cookies,
               useragent = agent, followlocation = TRUE, curl=curl)
    
    #if (url.exists(url)) { #this may slow down the function (not sure it works either)
    
    # The server sometimes seems to fail to handshake(?) this is not repeatable and
    # can be resolved by trying again. Here if it fails I get it to try again 5 times
    # before reporting an error
    a=0
    while(a<5){
        #print(url)
        resp <- try(getURL(url, curl = curl), silent=TRUE)
        if(is.null(attr(resp,'class'))) attr(resp,'class') <- 'success'
        #print(attr(resp,'class'))
        if(attr(resp,'class') == 'try-error'){
            a=a+1
            if(a==5) stop('The server is not responding correctly, please try again in a minute')
        } else {
            a=999
        }
    }
    
    attr(resp,'class') <- NULL
    
    # If the response is empty '' pass this on
    if(resp!=''){
        
        #Some character strings in the JSON as returned from the gateway
        #dont play ball in fromJSON, this includes a '\\' in some of the 
        #attrStr fields (found in red squirrel searches). Here I use gsub
        #to get rid of these occurrences so that it works. This is annoying
        removeBSL <- function(x){ #Removes BackSLashes
            x <- gsub(" \\\\","\\", x)  
        }
                
        resp1 <- try(fromJSON(resp, asText=TRUE), silent = TRUE)
        
        if(class(resp1) == 'try-error'){ # If it fails try removing backslashes
            message <- attr(resp1,'condition')$message
            warning(paste('Converting JSON failed:', message,'. Attempting to remove problem characters'))
            resp1 <- try(fromJSON(removeBSL(resp), asText=TRUE), silent = TRUE)    
        }
        
        if(class(resp1) == 'try-error'){ # If it still fails throw an error and return object
            message <- attr(resp1,'condition')$message
            resp <<- resp
            stop(paste('When interpreting response from NBN servers:',
                     message,
                     '. This can occur if the NBN servers are experiencing problems.',
                     'If the problem persists report this error to the package maintainers'))  
        } 
    } else {
        
        resp1 <- resp
    
    }
    
    return(resp1)

}
