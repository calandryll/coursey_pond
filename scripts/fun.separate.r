## Take DO, Water Temp and Wind Speed to calculate GPP and R
## required:
## data = data frame name
## date = Date column name (Must be in POSIXCT i.e. lubridate ymd)
## temperature = Temperature column name, units in C
## dissoxy = DO column name, units in mg L
## odo_sat = DO Saturation column nmae
## wind = Wind Speed column name
## lat and long = lattitude and longitude for site to calculate sunrise and sunsent values


library(lubridate) # Pull Month, Day, Year from date
library(ISOweek) # Determine Week number
library(tidyverse)
library(maptools) # Used for determining sunrise sunset values

schmidt_number = function(data, temperature = 'Temp', wind = 'WSpd', gage_height = 4.5){
  # From Wanninkof, 2014 LNO:Methods
  cat('Calculating Schmidt Number\n')
  data = mutate(data, Schmidt_Num = 1745.1-(124.34*data[[temperature]])+(4.8055*data[[temperature]]^2)-(0.10115*data[[temperature]]^3)+(0.00086842*data[[temperature]]^4))
  # From Raymond et al. 2012 LNO:Fluids & Environments
  #data = mutate(data, Schmidt_Num = 1568 - (86.04 * data[[temperature]]) + (2.142 * data[[temperature]]^2) - (0.0216 * data[[temperature]]^3))
  
  
  # Adjust for height of wind gage
  cat('Adjusting for Wind Gauge Height\n')
  data = data %>% mutate(Wind_cor = data[[wind]] * (10 / gage_height)^0.15)
}

gas_exchange = function(data, method = c('bilinear', 'power', 'static', 'temperature', 'size'), lake.size = 2.43, dissoxy = 'ODO_mgL', Date = 'Date', temperature = 'Temp'){
  if(method == 'bilinear'){
    data = data %>% mutate(K_wind = ifelse(Wind_cor < 3.7, ((0.72 * Wind_cor) * (Schmidt_Num/600)^-0.5) * 24 / 100, (((4.33 * Wind_cor) - 13.3) * (Schmidt_Num/600)^-0.5) * 24 / 100))
  } else if(method == 'power'){
    data = data %>% mutate(K_wind = (((0.228 * Wind_cor^2.2) + 0.168) * (Schmidt_Num / 600)^-0.5) * 24 / 100)
  } else if(method == 'static'){
    data = data %>% mutate(K_wind = ifelse(Wind_cor < 3.7, (1 * (Schmidt_Num/600)^-0.5) * 24 / 100, (((5.14 * Wind_cor) - 17.9) * (Schmidt_Num/600)^-0.5) * 24 / 100))
  } else if(method == 'temperature'){
    data = data %>% mutate(K_wind = ifelse(data[[temperature]] > lag(data[[temperature]]), (((1.74 * Wind_cor) - 0.15) * (Schmidt_Num/600)^-0.5) * 24 / 100, ((((2.04 * Wind_cor)-2.0) * (Schmidt_Num/600)^-0.5) * 24 / 100)))
  } else if(method == 'size'){
    data = data %>% mutate(K_wind = ((2.51 + (1.48 * Wind_cor) + (0.39 * Wind_cor * log10(lake.size)) * (Schmidt_Num/600)^-0.5) * 24 / 100))
  }
  data = data %>% mutate(Gas_exchange = K_wind * (C_sat - data[[dissoxy]]), correctedDO = data[[dissoxy]] + Gas_exchange)
}

production = function(data, date = 'Date', site.long = -75.510, site.lat = 38.988, GPP = "csv/GPP.csv", R = "csv/R.csv", final = "csv/Coursey_cleaned.csv", NEP = "csv/NEP.csv", writedata = FALSE){
  # Sunrise Sunset Function - library maptools
  # Lat and Long for data Pond
  latlong = matrix(c(site.long, site.lat), nrow = 1)
  latlong.spatial = SpatialPoints(latlong, proj4string = CRS("+proj=longlat +datum=WGS84"))
  sunrise.sunset = cbind(sunriset(latlong.spatial, as.POSIXct(data[[date]], tz ="EST"), direction="sunrise", POSIXct.out=TRUE), sunriset(latlong.spatial, as.POSIXct(data[[date]], tz ="EST"), direction="sunset", POSIXct.out=TRUE))
  colnames(sunrise.sunset) = c("Length_sunrise", "Sunrise", "Length_sunset", "Sunset")
  data = mutate(data, Sunrise = sunrise.sunset$Sunrise, Sunset = sunrise.sunset$Sunset)
  
  # Estimation of Production
  cat('Calculating Production\n')
  days = levels(as.factor(as_date(data[[date]])))
  days_length = as.numeric(length(days))
  night_data = NULL
  day_data = NULL
  data_all = NULL
  data_day = NULL
  for(i in 1:days_length)
  {
    # Take data for Night time as Half hour before Sunrise and an hour after sunset
    # This is also equal to R
    # Check that there is an entire days worth of data there.
    data_day = data %>% filter(Date_2 == days[i])
    # Data > 23 is hourly at a minimum
    if(nrow(data_day) > 1){
      #cat("Calcuating Production for", days[i], "\n")
      night_time = rbind(filter(data, Date_2 == days[i], Date > (Sunset + 60*60)), filter(data, Date_2 == days[i+1], Date < (Sunrise - 60*30)))
      # Date for day time - Half an hour after sunrise and at an hour after sunsent
      day_time = filter(data, Date_2 == days[i], Date > (Sunrise + 60*30), Date < (Sunset + 60*60))
      # Do not run if there is no night time data or has only 2 or fewer hours
      # Analysis for Night Time data
      if(nrow(night_time) > 2)
      {
        lm_night = lm(night_time$correctedDO~julian(night_time$Date))
        conf_night = confint(lm_night, level = 0.95)
        slope_night = lm_night$coefficients[2]
        #timedif_night = as.numeric(max(julian(night_time$Date))) - as.numeric(min(julian(night_time$Date)))
        #Julhour_night = timedif_night / 0.041818
        timedif_night = time_length(interval(min(night_time$Date), max(night_time$Date)), 'hours')
        Julhour_night = 24
        rate_night = (slope_night * timedif_night) / Julhour_night
        upper_conf_night = conf_night[4] * 24 / Julhour_night
        lower_conf_night = conf_night[2] * 24 / Julhour_night
        night_ge = mean(day_time$Gas_exchange)
        vec_night = data.frame(Date = ymd(days[i], tz = "EST"), Rate = as.numeric(rate_night), Upper_conf = as.numeric(upper_conf_night), Lower_conf = as.numeric(lower_conf_night), Gas_Exchange = as.numeric(night_ge), Slope = slope_night)
        night_data = bind_rows(night_data, vec_night)
        vec_night = NULL
        if(nrow(day_time) > 5)
        {
          lm_day = lm(day_time$correctedDO~julian(day_time$Date))
          conf_day = confint(lm_day, level = 0.95)
          slope_day = lm_day$coefficients[2]
          #timedif_day = as.numeric(max(julian(day_time$Date))) - as.numeric(min(julian(day_time$Date)))
          #Julhour_day = timedif_day / 0.041818
          timedif_day = time_length(interval(min(day_time$Date), max(day_time$Date)), 'hours')
          Julhour_day = 24
          rate_day = (slope_day * timedif_day) / Julhour_day
          upper_conf_day = conf_day[4] * 24 / Julhour_day
          lower_conf_day = conf_day[2] * 24 / Julhour_day
          day_ge = mean(day_time$Gas_exchange)
          vec_day = data.frame(Date = ymd(days[i], tz = "EST"), Rate = as.numeric(rate_day), Upper_conf = as.numeric(upper_conf_day), Lower_conf = as.numeric(lower_conf_day), Gas_Exchange = as.numeric(day_ge), Slope = slope_day)
          day_data = bind_rows(day_data, vec_day)
          vec_day = NULL
        }
        else
        {
          cat("No day time data for ", days[i], "\n")
        }
        
      }
      else
      {
        cat("No night Time Data for ", days[i], "\n")
      }
      
    }
    else
    {
      cat("Not enough time points for ", days[i], "\n")
    }
    numobs = NULL
  }
  data_all = full_join(day_data, night_data, by = "Date") %>% mutate(GPP = Rate.x - Rate.y) %>% select(Date = Date, GPP, NEP = Rate.x, R = Rate.y, GE = Gas_Exchange.x, Slope_Day = Slope.x, Slope_Night = Slope.y)
  if(writedata == TRUE){
    write.csv(data, file = 'csv/complete.csv', row.names = F)
    write.csv(day_data, file = GPP, row.names = F)
    write.csv(night_data, file = R, row.names = F)
    write.csv(data_all, file = final, row.names = F)
  }
  return(data_all)
}