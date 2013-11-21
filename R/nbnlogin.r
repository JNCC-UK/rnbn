#' Get Login credentials for NBN access
#' 
#' Opens a dialog box for the user to enter credentials and manages cookies
#' 
#' @export
#' @return NULL
#' @author Tom August, CEH \email{tom.august@@ceh.ac.uk}


nbnLogin <- function(){
    
    library(tcltk)
    
    # set up file path where cookies are saved
    cookiePath<-paste(.Library,'rnbn/cookies',sep='/')
    
    # set up Curl
    agent = "rnbn v0.1"
    curl = getCurlHandle()
    cookies <- paste(cookiePath, 'cookies.txt', sep = '/')
    curlSetOpt(cookiefile = cookies, cookiejar = cookies,
               useragent = agent, followlocation = TRUE, curl=curl)
    
    # See if we are known
    whoamI <- "https://data.nbn.org.uk/api/user"
    resp <- fromJSON(getURL(whoamI, curl = curl), asText = TRUE) # resp$id == 1 if we are not logged in
    #print(resp)
    
    if(resp['id'] == 1){
        
        # Create the directory for cookies
        dir.create(cookiePath, showWarnings = FALSE)
              
        # Get username and password
        UP <- getLogin()
        username <- UP$username
        password <- UP$password
                
        # Create login URL
        urlLogin <- paste("https://data.nbn.org.uk/api/user/login?username=",username,
                    "&password=", password, "&remember=true", sep='')
        
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