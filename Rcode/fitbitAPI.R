#=============================================================================================================
# functions to pull data from fitbit API
#==============================================================================================================

# Fibit API KEY & SECRET INFO
FITBIT_KEY  <- " "
FITBIT_SECRET <- " "
FITBIT_CALLBACK <- " "

source("Rcode/fitbit_keys.R")

# Get token
getToken <- function(){
  token <- fitbitr::oauth_token(key = FITBIT_KEY, 
                                secret = FITBIT_SECRET,
                                callback = FITBIT_CALLBACK)
  return(token)
}  


# Download data from API------------------------------------------------------------------------------------------------
# code was adpted from https://www.r-bloggers.com/downloading-fitbit-data-histories-with-r/

#download step data using fitbitr library
getStepsData <- function(dates){
  token <- getToken()
  
  #loop over vector of dates to download steps data returns list of dataframe
  steps <- lapply(dates, function(x){
    steps_dflist <- fitbitr::get_activity_intraday_time_series(token, "steps", x, detail_level="1min")
    return(steps_dflist)
  })
  
  #bind the list of dataframes to create 1 DF
  steps <- bind_rows(steps)
  steps <- rename(steps, date = dateTime)
  steps <- steps %>%
    mutate(value = as.numeric(value), datasetType = as.character(datasetType))
  return(steps)
}

#-----------------------------------------------------------------------------------------------------------------------
# download heart rate data using the fitbitr library
# (colnames)sec in day, time, heart rate, date 
getHeartRateData <- function(dates){  
  token <- getToken()
  
  #loop over vector of dates to download heart rate data returns list of dataframe
  heart_rate <- lapply(dates, function(x){
    df <- get_heart_rate_intraday_time_series(token, date = x, detail_level = "1min")
    df$date <- x
    df <- df %>%
      rename(., heart_rate = value) %>%
      mutate(., heart_rate = as.numeric(heart_rate))
    
    return(df)
  })
  
  #bind the list of dataframes to create 1 DF
  heart_rate <- bind_rows(heart_rate)
  return(heart_rate)
}  
#-----------------------------------------------------------------------------------------------------------------------
# download calories burned per day data by 1 minute increment
# col names dateTime,	value, dataset_level, dataset_mets, dataset_time,	dataset_value,
# datasetInterval, datasetType
getCalorieData <- function(dates){
  token <- getToken()
  
  #loop over vector of dates to download calories burned data returns list of dataframe
  cals <- lapply(dates, function(x){
    df <- get_activity_intraday_time_series(token, "calories", date = x, detail_level = "1min")
    df <- rename(df, date = dateTime)
    return(df)
  })
  
  #bind the list of dataframes to create 1 DF
  cals <- bind_rows(cals)
  cals$value <- as.numeric(cals$value)
  cals$datasetType <- as.numeric(cals$datasetType)
  return(cals)
}  
  
#-----------------------------------------------------------------------------------------------------------------------
# download weight data one day at a time
getWeightData <- function(dates){
  token <- getToken()
  
  # loop over vector of dates download weight data returns list of dataframes
  weight <- lapply(dates, function(x){
    df <- GET(paste0('https://api.fitbit.com/1/user/-/body/log/weight/date/', x,'.json'),
            httr::config(token = token$token))
    df <- jsonlite::fromJSON(httr::content(df, as = "text"))
    
    # return DF of weight
    df <- df$weight
    return(df)
})
  # bind togther all lists of DF to make one DF of dates requested
  weight <- bind_rows(weight)
  weight <- weight %>%
    select(., -logId, -source)
  return(weight)
}

#----------------------------------------------------------------------------------------------------------------------
###format so it fits in DB
getSleepData <- function(dates){
  token <- getToken()
  
  sleep <- lapply(dates, function(x){
    df <- GET(paste0('https://api.fitbit.com/1.2/user/-/sleep/date/',x,'.json'),
                   httr::config(token = token$token))

    df <- jsonlite::fromJSON(httr::content(df, as = "text"))
    df <- df$sleep
    df <- bind_rows(df$levels$data)
    df$levels <- NULL
    return(df)
})
  
  # bind togther all lists of DF to make one DF of dates requested
  sleep <- bind_rows(sleep)
  
  # parse out date/time info
  sleep <- sleep %>%
    separate(., dateTime, into = c("date", "time"), sep = "[T]") %>%
    separate(., time, into = c("time", "remove"), sep = "[.]") %>%
    select(-remove)
  
  return(sleep)
}
 
#-----------------------------------------------------------------------------------------------------------------------
# download activity data 
getActivityData <- function(dates){
  token <- getToken()
  
  #download activity data & format only activity minutes into a dataframe
  act_sum <- lapply(dates, function(x){
    df <- get_activity_summary(token, date = x)
    act_sumDF <- data.frame(fairly_active_min = df$summary$fairlyActiveMinutes,
                            lightly_active_min = df$summary$lightlyActiveMinutes,
                            very_active_min = df$summary$veryActiveMinutes,
                            sedentary_min = df$summary$sedentaryMinutes,
                            resting_hr = ifelse(is.null(df$summary$restingHeartRate), 0, df$summary$restingHeartRate),
                            date = x)
    return(act_sumDF)
    
  })
  
  act_sum <- bind_rows(act_sum)
  return(act_sum)
  
}  





