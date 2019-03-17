# preparatory work of csv's
library(readr)
library(lubridate)
library(tidyverse)
# read in tables, 
flowering <- read_csv('~/sakura/data/flowering.csv') %>% 
  mutate('f_day'=day,'f_rm'=rm) %>% select(-c(day,rm))
bloom <- read_csv('sakura/data/bloom.csv') %>% 
  mutate('b_day'=day,'b_rm'=rm) %>% select(-c(day,rm))

# join data
df <- full_join(flowering,bloom,by = c('l_code','year','l_name'))
rm(bloom,flowering)

# NOTE: there are some outliers in early years
# where flowering most likely occured in previous Dec.
df[as.integer(df$b_day)-as.integer(df$f_day) < 0,] %>% 
  filter(!is.na(year))

# ---------blooming date work------------
# fix date format so we can parse month as decimal
df$b_day_raw <- mapply(FUN = function(x,y) {
  return(format(paste0('0',x,y),format='%m%d%Y'))
},df$b_day,df$year,USE.NAMES = FALSE) 

df$f_day_raw <- mapply(FUN = function(x,y) {
  return(format(paste0('0',x,y),format='%m%d%Y'))
},df$f_day,df$year,USE.NAMES = FALSE) 
  
# create date col
df$b_date <- as.Date(df$b_day_raw,format='%m%d%Y')
df$f_date <- as.Date(df$f_day_raw,format='%m%d%Y')

# prep step for daymonth variable creation (2019 is default year, this doesn't
# take leap year into account but should be good enough for the purpose of 
# this analysis
df$b_daymonth_raw <- mapply(FUN = function(x,y) {
  return(format(paste0('0',x,'-2019'),format='%m%d%Y'))
},df$b_date,USE.NAMES = FALSE)
df$f_daymonth_raw <- mapply(FUN = function(x,y) {
  return(format(paste0('0',x,'-2019'),format='%m%d%Y'))
},df$f_date,USE.NAMES = FALSE) 

# create daymonth variables
df$b_daymonth <- as.Date(df$b_daymonth_raw,format='%m%d-%Y')
df$f_daymonth <- as.Date(df$f_daymonth_raw,format='%m%d-%Y')

df <- df %>% 
  select(-c(b_daymonth_raw,b_day_raw,f_daymonth_raw,f_day_raw))
df <- df %>% select(-ends_with('rm'))
