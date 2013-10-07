getLogin <- function(){
    # Create a "main" window with a button which activates our dialog
    require(tcltk)
    require(digest)
    
    # Open a dialogue box to get username
    username <- modalDialog("Username", "Enter Your Username", "")
    if (username == "ID_CANCEL") stop('You must log in to use rbn. Visit https://staging-data.nbn.org.uk/User/Register to register')
    
    # Open a dialogue box to get password
    password <- modalDialog("Password", "Enter Your Password", "")
    if (password == "ID_CANCEL") stop('You must log in to use rbn. Visit https://staging-data.nbn.org.uk/User/Register to register')
    
    # Encrypt password using MD5 hash ### CURRENTLY NOT FUNCTIONING
    #password <- digest(password)
    
    # Return username and encrypted password
    return(list(username=username,password=password))
}