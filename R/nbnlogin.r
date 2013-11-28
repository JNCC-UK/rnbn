#' Get Login credentials for NBN access
#' 
#' Opens a dialog box for the user to enter credentials and manages cookies
#' 
#' @export
#' @return NULL
#' @author Tom August, CEH \email{tom.august@@ceh.ac.uk}


nbnLogin <- function(){
    
    # set up Curl
    agent = "rnbn v0.1"
    options(RCurlOptions = list(sslversion=3L, ssl.verifypeer = FALSE))
    curl = getCurlHandle()
    cookies <- 'rnbn_cookies.txt'
    curlSetOpt(cookiefile = cookies, cookiejar = cookies,
               useragent = agent, followlocation = TRUE, curl=curl)
    
    # See if we are known
    whoamI <- "https://data.nbn.org.uk/api/user"
    
    # sometimes the server fails to handshake, i try to get around this by trying again
    a=0
    while(a<5){
        resp_who <- try(getURL(whoamI, curl = curl), silent=TRUE)
        if(is.null(attr(resp_who,'class'))) attr(resp_who,'class') <- 'success'
        if(attr(resp_who,'class') == 'try-error'){
            a=a+1
            if(a==5) stop('The server is issuing an alert handshake failure, please try again ni a minute')
        } else {
            a=999
        }
    }
    
    attr(resp_who,'class') <- NULL
    resp <- fromJSON(resp_who, asText = TRUE) # resp$id == 1 if we are not logged in
    #print(resp)
    
    if(resp['id'] == 1){
        
        # Create the directory for cookies
        #dir.create(cookiePath, showWarnings = FALSE)
              
        # Get username and password
        UP <- getLogin()
        username <- UP$username
        password <- UP$password
                
        # Create login URL
        urlLogin <- paste("https://data.nbn.org.uk/api/user/login?username=",username,
                    "&password=", password, sep='')
        
        # Check that login was a success (if not stop)
        resp <- fromJSON(getURL(urlLogin,curl=curl), asText=TRUE) #login result
        if(!resp$success){
            stop('Username and password invalid, have another go')
        } else {
            print('Login successful')
        }
    }
    
    # To write cookies to file we need to remove the curl object and run garbage collection
    rm(curl)
    invisible(gc())
}