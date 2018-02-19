library(tidyverse)
library(lubridate)
library(numform)

coursey = read.csv('csv/boxplot.csv')
coursey.all = read.csv('csv/boxplot_all.csv')

# Values the same for all figures
RESO = 1200 # Set resolution
PS = 12 # Set point resolution


theme_set(theme_bw())

coursey.nep = coursey %>% filter(Factor != 'NEP') %>% mutate(Date = ymd(Date)) %>% ggplot(aes(Date, Data, shape = Factor, color = Factor, fill = Factor)) + geom_hline(yintercept = 0) + geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.91), legend.title = element_blank(), legend.background = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = f_month) + scale_shape_manual(values = c(16, 25)) + scale_color_manual(values = c('black', 'black')) + scale_fill_manual(values = c('black', '#bdbcbd')) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1))
ggsave('figures/Figure_3.png', units = 'mm', dpi = RESO)

coursey.nep.cleaned = coursey.all %>% filter(Factor != 'NEP') %>% mutate(Date = ymd(Date)) %>% ggplot(aes(Date, Data, shape = Factor, color = Factor, fill = Factor)) + geom_hline(yintercept = 0) + geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.91), legend.title = element_blank(), legend.background = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = f_month) + scale_shape_manual(values = c(16, 25)) + scale_color_manual(values = c('black', 'black')) + scale_fill_manual(values = c('black', '#bdbcbd')) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1))
ggsave('figures/Figure_3a.png', units = 'mm', dpi = RESO)

coursey.box = coursey %>% mutate(Date = ymd(Date)) %>% ggplot(aes(Date, Data)) + geom_hline(yintercept = 0) + geom_boxplot(aes(color = Factor, fill = Factor, group = interaction(Factor, format(Date, format = '%b - %y'))), outlier.color = 'black') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.88), legend.title = element_blank(), legend.background = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = f_month) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_fill_manual(values = c('white', '#bdbcbd', 'grey27')) + scale_color_manual(values = c('black', 'black', 'black'))
ggsave('figures/Figure_4.png', units = 'mm', dpi = RESO)

coursey.box = coursey.all %>% mutate(Date = ymd(Date)) %>% ggplot(aes(Date, Data)) + geom_hline(yintercept = 0) + geom_boxplot(aes(color = Factor, fill = Factor, group = interaction(Factor, format(Date, format = '%b - %y'))), outlier.color = 'black') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.88), legend.title = element_blank(), legend.background = element_blank()) + scale_x_date(date_breaks = 'month', date_labels = f_month) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_fill_manual(values = c('white', '#bdbcbd', 'grey27')) + scale_color_manual(values = c('black', 'black', 'black'))
ggsave('figures/Figure_4a.png', units = 'mm', dpi = RESO)
