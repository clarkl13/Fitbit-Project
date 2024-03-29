#Update Fitbit Database on AWS

#=======================================================================================================================

# libraries
if (!require("pacman")){install.packages("pacman")} else {library(pacman)}

pacman::p_load(DT, knitr, kableExtra, pander, RMariaDB, fs, httr, RMySQL, DBI, odbc, jsonlite, lubridate, devtools, 
               fitbitr, tidyverse)

options(scipen = 999)


# load in database function---------------------------------------------------------------------------------------------
source("Rcode/RDS_connect.R") 
# functions:  connectDB returns DB connection to my AWS RDS fitbit DB
#             getTableLastUpdateDate returns last date of data entered into table in DF
#             insertDataDB inserts DF into table specified
#             getTableinfo returns stats about specified table in DB

# today's date
date_today <- Sys.Date()

# Create vector of table names to get last date of data in each table
tables <- c("steps", "heart.rate", "activity", "calories", "sleep.stages", "weight")

# Loop over tables and get last date in each table bind data into DF with names of tables from DB
last_entered_dates <- lapply(tables, getTableLastUpdateDate)
last_entered_dates <- bind_rows(last_entered_dates)
last_entered_dates$table <- tables


# load in functions for accessing fitbit API----------------------------------------------------------------------------
source("Rcode/fitbitAPI.R")
# functions:  getToken connects to fitbit API 
#             getActivityData returns DF of activity data
#             getCalorieData returns DF of calories burned
#             getHeartRateData returns DF of heart rate data
#             getSleepData returns DF of sleep data
#             getStepsData returns DF of steps data
#             getWeightData returns DF of weight logs



# loop over DF 
updateDB <- apply(last_entered_dates, 1, function(x){    
  
  # if the date in the DB is before today create vector of dates to pull from DB
  if(date_today > ymd(x["date"])){
    
    dates_to_pull <- seq(ymd(x["date"]), ymd(date_today), by = "1 day")
    
    if(x["table"] == "steps"){

      data <- getStepsData(dates_to_pull)
    }  
    
    if(x["table"] == "heart.rate"){

      data <- getHeartRateData(dates_to_pull)
    }

    if(x["table"] == "activity"){

      data <- getActivityData(dates_to_pull)
    }

    if(x["table"] == "calories"){

      data <- getCalorieData(dates_to_pull)
    }

    if(x["table"] == "sleep.stages"){

      data <- getSleepData(dates_to_pull)
    }

    if(x["table"] == "weight"){

      data <- getWeightData(dates_to_pull)
    }
    
    # Write pulled data to approp table in DB
    DB <- insertDataDB(API_data = data, table_name = x["table"])
    
    # Returns a list of NULL or TRUE, NULL dates match, TRUE it updated or No data pulled for that date range 
    DB <- append(DB, x["table"])
    return(DB)

  }

})


# get status info from each table in DB---------------------------------------------------------------------------------
status_info <- lapply(tables, GetTableInfo)

# bind rows into DF reformat and only keep variable we care about
db_table_info <- status_info %>%
  bind_rows() %>%
  transmute(Name = Name, 
         Engine = Engine, 
         Version = Version, 
         Rows = prettyNum(as.numeric(Rows), big.mark = ","), 
         size_Mb = round(Data_length / 1e+6, 1),
         date_time_created = Create_time, 
         date_time_updated = Update_time)

db_table_info %>%
  kable(align = rep('c', 7), caption = "AWS Fitbit Database Table Information") %>%
  kable_styling(bootstrap_options = c("striped"), position = "center") 



