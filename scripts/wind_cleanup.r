library(tidyverse)
library(lubridate)

harr = read.csv('originals/Wind/DHAR_201312310000-201501010000.dat')
harr2 = read.csv('originals/Wind/DHAR_201501010001-201701020001.dat')

harr = harr %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)
harr2 = harr2 %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)

harrington = rbind(harr, harr2)

wood = read.csv('originals/Wind/DWDS_201312310000-201501010000.dat')
wood2 = read.csv('originals/Wind/DWDS_201501010001-201701020001.dat')

wood = wood %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)
wood2 = wood2 %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)

woodside = rbind(wood, wood2)

murder = read.csv('originals/Wind/274012_201312310000-201501010000.dat')
murder2 = read.csv('originals/Wind/274012_201501010001-201701020001.dat')

murder = murder %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)
murder2 = murder2 %>% mutate(DateTime = paste(Date, Time, sep = ' '), Date2 = with_tz(ymd_hms(DateTime, tz = 'UTC'), tz = 'America/New_York')) %>% select(Date = Date2, WSpd = Wind.Speed..m.s.1., WDir = Wind.Direction..deg..)

murderkill = rbind(murder, murder2)

write.csv(wind, 'csv/wind.csv', row.names = FALSE)
