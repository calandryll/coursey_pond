library(dplyr)
library(lubridate) # Pull Month, Day, Year from date
library(ISOweek) # Determine Week number
library(marelac)
exo.data = read.csv('csv/exo_data.csv')
weather.data = read.csv('csv/weather.csv')

exo.data = exo.data %>% mutate(Date = mdy_hm(Date_Time, tz = 'EST')) %>% select(Date, Temp = Temp_C, Cond = Cond_uScm, SpCond = SpCond_uScm, pH, ODO_sat, ODO_mgL, Turbidity = Turbidity_FNU, fDOM = fDOM_QSU, Chl = Chlorophyll_ugL, BGA = BGA_PC_ugL, Depth = Depth_m)

weather.data = weather.data %>% mutate(Date = mdy_hm(Date, tz = 'EST')) %>% select(Date, WSpd, MaxWSpd, Wdir, TotPAR, TotPrcp, BP)

# Calculate O2 saturation concentration
merged = left_join(exo.data, weather.data) %>% mutate(Year = year(Date), Month = month(Date), Day = day(Date), Week = ISOweek(Date), C_sat = gas_O2sat(S = 0, Temp), Date_2 = as_date(Date))

# Remove this date because it is truncated.  Remove days with no PAR
merged = merged %>% filter(Date_2 != '2015-03-10') %>% filter(!is.na(TotPAR))

write.csv(merged, 'csv/CP_cleaned.csv', row.names = FALSE)
