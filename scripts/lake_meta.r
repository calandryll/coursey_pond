source('scripts/fun.prod.new.r')

coursey = read.csv("csv/CP_cleaned.csv")
coursey = coursey %>% mutate(Date = ymd_hms(Date, tz = 'EST'))

coursey = coursey %>% mutate(Wind_cor = WSpd * (10 / 4.5)^0.15, k600 = k.crusius.base(Wind_cor, method = 'bilinear'), K_wind = k600.2.kGAS.base(k600, Temp, gas = 'O2'))


prod.ols = production(coursey, wind = 'Wind_cor', method = 'ols')
prod.mle = production(coursey, wind = 'Wind_cor', method = 'mle', error.type = 'OE')
prod.classic = production(coursey, wind = 'Wind_cor', method = 'classic')
prod.kal = production(coursey, wind = 'Wind_cor', method = 'kalman')
prod.bayes = production(coursey, wind = 'Wind_cor', method = 'bayesian')

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
            Avg_Wind = mean(Wind_cor, na.rm = T)) %>%
  mutate(Date = ymd(Date_2, tz = 'EST')) %>%
  select(-Date_2)

prod.ols = inner_join(prod.ols, coursey.stats)
prod.mle = inner_join(prod.mle, coursey.stats)
prod.classic = inner_join(prod.classic, coursey.stats)
prod.kal = inner_join(prod.kal, coursey.stats)
prod.bayes = inner_join(prod.bayes, coursey.stats)

write.csv(prod.ols, 'csv/prod_ols.csv', row.names = FALSE)
write.csv(prod.mle, 'csv/prod_mle.csv', row.names = FALSE)
write.csv(prod.classic, 'csv/prod_classic.csv', row.names = FALSE)
write.csv(prod.kal, 'csv/prod_kalman.csv', row.names = FALSE)
write.csv(prod.bayes, 'csv/prod_bayesian.csv', row.names = FALSE)
write.csv(coursey, 'csv/coursey_lakemeta.csv', row.names = FALSE)
