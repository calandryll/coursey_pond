source("scripts/fun.production.r")

coursey = read.csv("csv/CP_cleaned.csv")
coursey = coursey %>% mutate(Date = ymd_hms(Date, tz = 'EST'))
#coursey = coursey %>% mutate(Date_2 = ymd(Date_2, tz = 'EST'))

coursey.prod = production(coursey, smooth = TRUE, smooth.time = 'Date_2', smooth.set = 0.001)

coursey.stats =  coursey %>%
  mutate(Wind_cor = (WSpd * (10/4.5)^0.15)) %>%
  group_by(Date_2) %>%
  summarise(Avg_Temp = mean(Temp, na.rm = T), 
            Avg_ODO = mean(ODO_mgL, na.rm = T), 
            Avg_Sat = mean(ODO_sat, na.rm = T),
            Max_ODO = max(ODO_mgL),
            Min_ODO = min(ODO_mgL),
            Max_Sat = max(ODO_sat),
            Min_Sat = min(ODO_sat),
            Avg_chl = mean(Chl, na.rm = T), 
            Avg_BGA = mean(BGA, na.rm = T), 
            Max_PAR = max(TotPAR, na.rm = T),
            Avg_Per = mean(TotPrcp, na.rm = T),
            Avg_Wind = mean(Wind_cor, na.rm = T),
            Avg_Dir = mean(Wdir)) %>%
  mutate(Date = ymd(Date_2, tz = 'EST')) %>%
  select(-Date_2)

coursey.prod = inner_join(coursey.prod, coursey.stats)

coursey.out = coursey.prod %>% filter(R >= 0.05 | GPP <= -0.15)

coursey.prod.cleaned = coursey.prod %>% filter(!Date %in% coursey.out$Date)

coursey.box = coursey.prod %>% gather(Factor, Data, GPP:R) %>% select(Date = Date, Factor = Factor, Data = Data) %>% filter(Factor == "NEP" | Factor == "GPP" | Factor == "R") %>% distinct(Date, Factor, .keep_all = TRUE) %>% mutate(Month = month(Date), Year = year(Date))

coursey.box2 = coursey.prod %>% gather(Factor, Data, GPP:R) %>% select(Date = Date, Factor = Factor, Data = Data) %>% filter(Factor == "NEP" | Factor == "GPP" | Factor == "R") %>% distinct(Date, Factor, .keep_all = TRUE) %>% filter(Date != as.POSIXct("2014-10-01", tz = "EST", origin = "1970-01-01")) %>% mutate(Month = month(Date), Year = year(Date))

write.csv(coursey.box, file = "csv/boxplot.csv", row.names = F)
write.csv(coursey.prod.cleaned, file = "csv/CP_Production.csv", row.names = F)

write.csv(coursey.prod, file = 'csv/CP_all.csv', row.names = F)
write.csv(coursey.box2, file = 'csv/boxplot_all.csv', row.names = F)
