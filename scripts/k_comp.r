source("scripts/fun.production.r")

coursey = read.csv("csv/CP_cleaned.csv")
coursey = coursey %>% mutate(Date = ymd_hms(Date, tz = 'EST'))
wind = read.csv('csv/wind.csv')
wind = wind %>% mutate(Date = ymd_hms(Date, tz = 'EST'))

coursey2 = inner_join(coursey, wind, by = 'Date')

coursey2 = coursey2 %>% select(Date, Temp, ODO_mgL, ODO_sat, WSpd = WSpd.x, WDir = WDir.x, TotPAR, TotPrcp, Year, Month, Day, Week, C_sat, Date_2)

coursey.bi = production(coursey2, gage_height = 3)
coursey.power = production(coursey2, gage_height = 3, method = 'power')
coursey.static = production(coursey2, gage_height = 3, method = 'static')
coursey.temp = production(coursey2, gage_height = 3, method = 'temperature')

coursey.prod = full_join(coursey.bi, coursey.power, by = 'Date') %>% full_join(coursey.static, by = 'Date') %>% full_join(coursey.temp, by = 'Date') %>% select(Date, GPP.bi = GPP.x, GPP.pow = GPP.y, GPP.static = GPP.x.x, GPP.temp = GPP.y.y, R.bi = R.x, R.pow = R.y, R.static = R.x.x, R.temp = R.y.y, GE.bi = GE.x, GE.pow = GE.y, GE.static = GE.x.x, GE.temp = GE.y.y)
