#' A simple function for removing cookies
#' 
#' This function looks for cookies used for logging into the NBN. Sometimes
#' this login process can go wrong, using this function you can delete cookies
#' resolving some issues.
#' 
#' @export
#' @param path The directory where the cookies are stored, this is your working
#' directory.
#' @param remove Logical, if TRUE the cookie file is deleted if FALSE the existance
#' of the file is reported.
#' @return NULL
#' @author Tom August, CEH \email{tomaug@@ceh.ac.uk}
#' @examples \dontrun{ 
#'  NBNCookies()
#'  }

NBNCookies <- function(path = getwd(), remove = FALSE){
    
    fileList <- list.files(path=path)
    
    if('rnbn_cookies.txt' %in% fileList){
        
        if(remove){
            
            unlink(file.path(path, 'rnbn_cookies.txt'))
            cat('Cookies deleted\n')
        
        } else {
            
            cat('Cookies found, not deleted\n')    
            
        }
    
    } else {
        
        cat('No cookies file found\n')
        
    }
    
}