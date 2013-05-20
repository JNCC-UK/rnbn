getGroupID<-function(name){
    
    if(class(name) != 'character'){
        warning('Group name non-character, conversion attempted')
        name <- as.character(name)
    }
    
    groupsDB<-listGroups()
    groupsDB$name<-tolower(groupsDB$name)
        
    name<-tolower(name)
    
    if(name %in% groupsDB$name){
        groupID<-groupsDB$key[groupsDB$name==name]
        return(groupID)
    } else {
        stop(paste(name,'is not a recognised group. Use listGroups to find valid names'))
    }
}