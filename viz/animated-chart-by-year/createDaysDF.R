library(lubridate)
lastyr <- lastyr %>% select(-c(b_daymonth,f_daymonth,f_day,b_day))
max_day <- max(lastyr$b_date,na.rm = TRUE) + 30
rows <- nrow(lastyr)
df_days <- data.frame(day_counter = rep(seq(from=as.Date('2018-01-01'),
                                            to=max_day,by=1),
                                        rows),lastyr)

# creates a scaled float value that we'll use to mimic bloom pattern
# over time
df_days$color_value <- mapply(FUN = function(b_date,f_date,tdy) {
  # if it hasn't flowered yet, na (white)
  if (f_date > tdy) {
    NA
  } else if (f_date - tdy == 0) {
    0
  } else if (f_date < tdy & b_date > tdy) {
    (tdy - f_date)*1.5/as.integer(b_date - f_date) # evenly space between .1 and 1.5
    # how far along we are / how many periods to space
    # scale by 1.5
  } else if(b_date - tdy == 0) { # full bloom
    1.5
  } else if(b_date < tdy) {
    # evenly space from 1.5 to 3 until 3 at and after 3 days past
    min(as.integer(tdy - b_date) * (3/4) + (3/4),3)
  }
},df_days$b_date,df_days$f_date,df_days$day_counter)

# safety check for nulls
if (nrow(df_days[is_null(df_days$color_value),]) > 0) {
  "Warning! NULL Values Generated. Check the function above ^"
}