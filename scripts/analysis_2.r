library(tidyverse)
library(cowplot)
library(lubridate)

combined = read.csv('csv/combined_production.csv') %>% mutate(Date = ymd(Date, tz = 'EST'))
coursey = read.csv('csv/CP_cleaned.csv') %>% mutate(Date = ymd_hms(Date, tz = 'EST'))
flow = read.csv('csv/flow.csv') %>% mutate(Date = ymd(Date, tz = 'EST'))


coursey.stats = coursey %>%
  mutate(Wind_cor = (WSpd * (10/4.5)^0.15), Max_wind_cor = (MaxWSpd * (10/4.5)^0.15)) %>%
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
            Avg_PAR = mean(TotPAR, na.rm = T),
            Avg_Per = mean(TotPrcp, na.rm = T),
            Max_Wind = max(Max_wind_cor, na.rm = T),
            Avg_Wind = mean(Wind_cor, na.rm = T)) %>%
  mutate(Date = ymd(Date_2, tz = 'EST')) %>%
  select(-Date_2)

combined = inner_join(combined, coursey.stats) %>% inner_join(flow)
write.csv(combined, 'csv/combined_production.csv', row.names = FALSE)

combined.gpp = combined %>%
  select(Date, GPP_kal:GPP_mle, Max_PAR, Avg_PAR, Max_Wind, Avg_Wind, RT) %>%
  gather(Method, Data, GPP_kal:GPP_mle)

combined.r = combined %>%
  select(Date, R_kal:R_mle, Max_PAR, Avg_PAR, Max_Wind, Avg_Wind, RT) %>%
  gather(Method, Data, R_kal:R_mle)

gpp.max = combined.gpp %>%
  filter(Data <= 0) %>%
  ggplot(aes(Max_PAR, Data, color = Method)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 0.95) + 
  scale_y_continuous(limits = c(-20, 1)) + 
  theme(axis.title.y = element_blank()) + 
  xlab('Maximum PAR')

gpp.avg = combined.gpp %>%
  filter(Data <= 0) %>%
  ggplot(aes(Avg_PAR, Data, color = Method)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 0.95) + 
  scale_y_continuous(limits = c(-20, 1)) +
  theme(axis.title.y = element_blank()) + 
  xlab('Average PAR')

rt.gpp = combined.gpp %>%
  filter(Data <= 0) %>%
  ggplot(aes(RT, Data, color = Method)) + 
  geom_hline(yintercept = 0) +
  geom_point(size = 0.95) + 
  scale_y_continuous(limits = c(-20, 1)) + 
  theme(axis.title.y = element_blank()) + 
  xlab('Residence Time')

wind.max = combined.gpp %>%
  filter(Data <= 0) %>%
  ggplot(aes(Max_Wind, Data, color = Method)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 0.95) +
  scale_y_continuous(limits = c(-20, 1)) +
  theme(axis.title.y = element_blank()) + 
  xlab('Maximum Wind')

wind.avg = combined.gpp %>%
  filter(Data <= 0) %>%
  ggplot(aes(Avg_Wind, Data, color = Method)) +
  geom_hline(yintercept = 0) + 
  geom_point(size = 0.95) +
  scale_y_continuous(limits = c(-20, 1)) +
  theme(axis.title.y = element_blank()) + 
  xlab('Average Wind')
legend = get_legend(wind.avg + theme(legend.title = element_blank(), legend.position = c(0.5, 0.5)))
plot_grid(gpp.max + theme(legend.position = 'none'), gpp.avg + theme(legend.position = 'none'), wind.max + theme(legend.position = 'none'), wind.avg+ theme(legend.position = 'none'), rt.gpp + theme(legend.position = 'none'), legend)
ggsave('figures/gpp_factors.png', units = 'mm', dpi = 1200)


combined.gpp %>%
  filter(Data <= 0) %>%
  filter(RT <= 2 | Max_PAR >= 1400) %>%
  ggplot(aes(Date, Data, color = Method)) +
  geom_hline(yintercept = 0) +
  geom_point()

combined %>%
  filter(RT <= 2 | Max_PAR >= 1400) %>%
  filter(GPP_cp <= 0)
