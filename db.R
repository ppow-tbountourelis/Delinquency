# Customizing Startup:
# At startup, R will source the Rprofile.site file. The file is located in C:\Program Files\R\R-n.n.n\etc directory.
# It will then look for a .Rprofile file to source in the current working directory.
# If it doesn't find it, it will look for one in the user's home directory. 
# There are two special functions you can place in these files. 
# .First( ) will be run at the start of the R session and .Last( ) will be run at the end of the session.

# Source the current file from .First() 
library(RODBC)
#-------------------------------------------------------------
openCon <- function() {
	
	username = "tbountourelis"
	password = "TbRpt19$"   
	con <- odbcConnect(dsn="PRD-RPT-DB-01", uid=username, pwd=password,  readOnly=FALSE)
	return (con)
}
#-------------------------------------------------------------
# Input: query filename 
# Output: data frame
dbQuery <- function(con, filename, stringsAsFactors = FALSE){

    query <- paste(readLines(filename), collapse=" ")
	  a <- sqlQuery(con, query, as.is = FALSE, stringsAsFactors = FALSE)
	  return (a)
}
#-------------------------------------------------------------
# Input: query file 
# Output: data frame
exeQuery = function(filename){

  con <- openCon()
	a <- dbQuery(con, filename)
	close(con)
	return (a)
}
#-------------------------------------------------------------
# Input: query string 
# Output: data frame
exeQueryString = function(string, stringsAsFactors = FALSE){
  
    con <- openCon()
    a <- sqlQuery(con, string, as.is = FALSE, stringsAsFactors)
    close(con)
    return (a)
}