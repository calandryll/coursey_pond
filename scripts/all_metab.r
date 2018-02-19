library(tidyverse)
library(cowplot)

kalman = read.csv('csv/prod_kalman.csv') %>% mutate(Date = ymd(Date))
bayes = read.csv('csv/prod_bayesian.csv') %>% mutate(Date = ymd(Date))
classic = read.csv('csv/prod_classic.csv') %>% mutate(Date = ymd(Date))
ols = read.csv('csv/prod_ols.csv') %>% mutate(Date = ymd(Date))
mle = read.csv('csv/prod_mle.csv') %>% mutate(Date = ymd(Date))
cp = read.csv('csv/Coursey_cleaned.csv') %>% mutate(Date = ymd(Date))

combined = full_join(kalman, cp, by = 'Date') %>%
  select(Date = Date, GPP_kal = GPP.x, GPP_cp = GPP.y, NEP_kal = NEP.x, NEP_cp = NEP.y, R_kal = R.x, R_cp = R.y) %>%
  full_join(bayes, by = 'Date') %>%
  select(Date, GPP_kal, GPP_cp, GPP_bayes = GPP, NEP_kal, NEP_cp, NEP_bayes = NEP, R_kal, R_cp, R_bayes = R) %>%
  full_join(classic, by = 'Date') %>%
  select(Date, GPP_kal, GPP_cp, GPP_bayes, GPP_classic = GPP, NEP_kal, NEP_cp, NEP_bayes, NEP_classic = NEP, R_kal, R_cp, R_bayes, R_classic = R) %>%
  full_join(ols, by = 'Date')

combined = combined %>% 
  select(Date, GPP_kal, GPP_cp, GPP_bayes, GPP_classic, GPP_ols = GPP, NEP_kal, NEP_cp, NEP_bayes, NEP_classic, NEP_ols = NEP, R_kal, R_cp, R_bayes, R_classic, R_ols = R) %>%
  full_join(mle, by = 'Date') %>%
  select(Date, GPP_kal, GPP_cp, GPP_bayes, GPP_classic, GPP_ols, GPP_mle = GPP, NEP_kal, NEP_cp, NEP_bayes, NEP_classic, NEP_ols, NEP_mle = NEP, R_kal, R_cp, R_bayes, R_classic, R_ols, R_mle = R)

write.csv(combined, 'csv/combined_production.csv', row.names = FALSE)

combined.gpp = combined %>%
  select(Date, GPP_kal:GPP_mle) %>%
  gather(Method, Data, GPP_kal:GPP_mle)

combined.nep = combined %>%
  select(Date, NEP_kal:NEP_mle) %>%
  gather(Method, Data, NEP_kal:NEP_mle)

combined.r = combined %>%
  select(Date, R_kal:R_mle) %>%
  gather(Method, Data, R_kal:R_mle)

theme_set(theme_bw())
# Values the same for all figures
RESO = 1200 # Set resolution
PS = 12 # Set point resolution

gpp.all = combined.gpp %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = 'bottom', legend.title = element_blank(), legend.background = element_blank()) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_x_date(date_breaks = 'month', date_labels = f_month) + scale_y_continuous(limits = c(-20, 20))
ggsave('figures/gpp_all.png', units = 'mm', dpi = RESO)

gpp.2014 = combined.gpp %>% mutate(Year = year(Date)) %>% filter(Year == 2014) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b') + scale_y_continuous(limits = c(-20, 20))

gpp.2015 = combined.gpp %>% mutate(Year = year(Date)) %>% filter(Year == 2015) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b', labels = element_blank()) + scale_y_continuous(limits = c(-20, 20))

gpp.2016 = combined.gpp %>% mutate(Year = year(Date)) %>% filter(Year == 2016) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b') + scale_y_continuous(limits = c(-20, 20))

legend = get_legend(gpp.2014 + theme(legend.background = element_blank(), legend.title = element_blank()))
pcol = plot_grid(gpp.2014 + theme(legend.position = 'none'), gpp.2015 + theme(legend.position = 'none'), gpp.2016 + theme(legend.position = 'none'), nrow = 3, ncol = 1)
p = plot_grid(pcol, legend, rel_widths  = c(3, .5))
ggsave('figures/gpp.png', units = 'mm', dpi = RESO)


r.2014 = combined.r %>% mutate(Year = year(Date)) %>% filter(Year == 2014) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b') + scale_y_continuous(limits = c(-20, 20))

r.2015 = combined.r %>% mutate(Year = year(Date)) %>% filter(Year == 2015) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b', labels = element_blank()) + scale_y_continuous(limits = c(-20, 20))

r.2016 = combined.r %>% mutate(Year = year(Date)) %>% filter(Year == 2016) %>% ggplot(aes(Date, Data, shape = Method, fill = Method, color = Method)) + geom_hline(yintercept = 0) + geom_jitter(size = 0.85) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = '%b') + scale_y_continuous(limits = c(-20, 20))
legend = get_legend(r.2014 + theme(legend.background = element_blank(), legend.title = element_blank()))
pcol = plot_grid(r.2014 + theme(legend.position = 'none'), r.2015 + theme(legend.position = 'none'), r.2016 + theme(legend.position = 'none'), nrow = 3, ncol = 1)
p = plot_grid(pcol, legend, rel_widths  = c(3, .5))
ggsave('figures/r.png', units = 'mm', dpi = RESO)
