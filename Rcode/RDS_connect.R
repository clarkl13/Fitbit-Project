## Functions that interface with database on AWS. 

#=======================================================================================================================
# connect to database 
# returns a connection object
connectDB <- function(){
  
  # Link to cnf file to your DB
  settings_file <- "C:/Users/sysadmin/OneDrive/Programming Projects/AWS Connection Info/fitbitDB.cnf"
  
  # Create connection object
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             default.file = settings_file,
                             group = "fitbit")
  return(con)

}  

# Queries DB for last date in table-------------------------------------------------------------------------------------
# table_name = character string with name of table in DB to add data
# need ` ` for date column in MySQL
getTableLastUpdateDate <- function(table_name){
  
  # create connection object
  con <- connectDB()
  
  # create SQL query for last date list in that particular table in desc order, only get 1st row
  query <- paste("SELECT `date` FROM `", table_name, "` order by `date` desc limit 1", sep = "")
  
  # query DB
  date <- RMariaDB::dbGetQuery(con, query)
  
  # close connection
  RMariaDB::dbDisconnect(con)
  
  return(date)
  
}
  
# Insert data into table------------------------------------------------------------------------------------------------
# API_data = DF with data to insert into table
# table_name = character string with name of table in DB to add data
insertDataDB <- function(API_data, table_name){
  
  # create connection object
  con <- connectDB()
  
  # insert DF into DB table that specified
  RMariaDB::dbWriteTable(con, 
                         value = API_data, 
                         row.names = FALSE, 
                         name = table_name, 
                         append = TRUE,
                         overwrite = FALSE)
  
  # close connection
  RMariaDB::dbDisconnect(con)
}

# Get status info for table in DB---------------------------------------------------------------------------------------
# table_name = character string with name of table in DB to add data
getTableInfo <- function(table_name){
  
  # create connection object
  con <- connectDB()
  
  # create SQL query to get status info from table specified
  query <- paste("SHOW TABLE STATUS LIKE '", table_name, "'", sep = "")
  
  # query DB
  status <- RMariaDB::dbGetQuery(con, query)
  
  # close connection
  RMariaDB::dbDisconnect(con)
  
  return(status)


}
