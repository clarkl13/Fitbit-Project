#=======================================================================================================================
# Clean data from DB 

# data is loaded from DB in Rmd doc.
#=======================================================================================================================
# clean activity Data into wide dataframe------------------------------------------------------------------------------
# rm duplicated dates should only be one row of data per day, wideDF
wide_DF <- main_actDF[!duplicated(main_actDF$date),]
wide_DF$date <- ymd(wide_DF$date)
wide_DF <- select(wide_DF, date, everything(), -idactivity)
wide_DF$resting_hr <- ifelse(wide_DF$resting_hr == 0, NA, as.numeric(wide_DF$resting_hr))

#  clean calories burned data and join to wide format------------------------------------------------------------------
# rm duplicate dates
calDF <- main_calDF[!duplicated(main_calDF$date),]

# only want data for each day not per min, format date, 
calDF <- calDF %>%
  select(date, value) %>%
  mutate(date = ymd(date))

# join to wideDF
wide_DF <- wide_DF %>%
  full_join(., y = calDF, by = "date") %>%
  rename(., cals_burned = value)

# rm calDF
rm(calDF)

# clean heart rate data------------------------------------------------------------------------------------------------
# data heart rate per min 
# find the median for each day
heartDF <- main_heartDF %>%
  group_by(date) %>%
  summarise(median_heart_rate = median(heart_rate)) %>%
  mutate(., date = ymd(date))

# join heart rate data to wideDF
wide_DF <- wide_DF %>%
  full_join(., heartDF, by = "date")

# rm heartDF
rm(heartDF)

# clean sleep data-----------------------------------------------------------------------------------------------------
# format date, summarize by day 
sleepDF <- main_sleepDF %>%
  mutate(date = ymd(date)) %>%
  group_by(date, level) %>%
  summarise(sleep_type_sec = sum(seconds)) %>%
  spread(level, sleep_type_sec) %>%
  rename(sec_asleep = asleep, 
         sec_awake = awake, 
         sec_deep = deep, 
         sec_light = light, 
         sec_rem = rem,
         sec_restless = restless) %>%
  mutate(sec_awake = ifelse(is.na(.data$sec_awake), .data$wake, .data$sec_awake)) %>%
  select(-wake, -unknown)

# add to DF
wide_DF <- wide_DF %>%
  full_join(., sleepDF, by = "date")

#rm sleepDF
rm(sleepDF)

# clean steps DF---------------------------------------------------------------------------------------------------------
stepsDF <- main_stepsDF %>%
  select(., date, value) %>%
  mutate(., date = ymd(date)) %>%
  rename(., steps = value) %>%
  distinct(., date, .keep_all = TRUE) 

# Join to wideDF
wide_DF <- wide_DF %>%
  full_join(., stepsDF, by = "date")

# rm stepsDF
rm(stepsDF)

# clean weight data-----------------------------------------------------------------------------------------------------
weightDF <- main_weightDF %>%
  arrange(., date, -weight) %>%
  distinct(., date, .keep_all = TRUE) %>%
  mutate(weight = round(weight * 2.205, 1), 
         date = ymd(date), 
         bmi = round(bmi, 1), 
         bodyfat_per = round(fat, 1)) %>%
  select(-idweight, -time, -fat)

# join to wideDF
wide_DF <- wide_DF %>%
  full_join(., weightDF, by = "date") %>%
  mutate(year = year(date), month = month(date))


# rm weightDF
rm(weightDF)

#-----------------------------------------------------------------------------------------------------------------------
# convert to wideDF to longDF
long_DF <- wide_DF %>%
  select(date, everything()) %>%
  gather(., metric, value, fairly_active_min:bodyfat_per) 

#rm extra varibles------------------------------------------------------------------------------------------------------
rm(main_actDF, main_calDF, main_heartDF, main_sleepDF, main_stepsDF, main_weightDF)
