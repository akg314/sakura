# preparatory work of csv's
library(readr)
library(lubridate)
library(tidyverse)
library(mondate)
# read in tables, 
flowering <- read_csv('~/archive/sakura/data/flowering.csv') %>% 
  mutate('f_day'=day,'f_rm'=rm) %>% select(-c(day,rm))
bloom <- read_csv('~/archive/sakura/data/bloom.csv') %>% 
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
},df$b_day,USE.NAMES = FALSE)
df$f_daymonth_raw <- mapply(FUN = function(x,y) {
  return(format(paste0('0',x,'-2019'),format='%m%d%Y'))
},df$f_day,USE.NAMES = FALSE)

# create daymonth variables
df$b_daymonth <- as.Date(df$b_daymonth_raw,format='%m%d-%Y')
df$f_daymonth <- as.Date(df$f_daymonth_raw,format='%m%d-%Y')

df <- df %>% 
  select(-c(b_day_raw,f_day_raw))
df <- df %>% select(-ends_with('rm'))
df <- df %>% select(-ends_with('raw')) %>% select(-ends_with('day')) %>%
  select(-l_name)

df <- df %>% left_join(df2,by=c('l_code'))


jsonlite::toJSON(df) %>% write_lines('~/archive/flowers-japan-web/public/yearlydata.json')
write.csv(file="~/archive/flowers-japan-web/src/components/public/yearlydata.csv", df,
          quote=c(1,3,4),row.names = FALSE)

write.csv(file="~/archive/flowers-japan-web/src/components/public/locations.csv", df2,
          quote=c(1,4,5),row.names = FALSE)


df2 <- read_csv('~/archive/sakura/data/geocoded_locations.csv') %>% 
cbind(.,read_csv('~/archive/sakura/data/location_romaji.csv', col_names = c('_','romaji'),skip=0)) %>%
  select(-`_`) %>% left_join(.,read_csv('~/archive/sakura/data/flowering.csv') %>%
                               select(l_code,l_name) %>% distinct(),by=c('loc_names'='l_name'))

jsonlite::toJSON(df2) %>% write_lines('~/archive/flowers-japan-web/public/locations.json')

#%>%
  write_csv('~/archive/sakura/data/locations.csv')
