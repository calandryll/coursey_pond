library(tidyverse)
library(Hmisc)
library(ezsummary)


stats.rcorr <- function(data)
{
  # Run a Pearson
  prcorr <- rcorr(data, type="pearson")
  # Run the Spearman
  #srcorr <- rcorr(data, type="spearman")
  cat("Pearson's: \nr:\n")
  print(signif(prcorr$r, digits=4))
  cat("p-value:\n")
  print(signif(prcorr$P, digits=4))
  #cat("\nSpearman's: \nr:\n")
  #print(signif(srcorr$r, digits=6))
  #cat("p-value:\n")
  #print(signif(srcorr$P, digits=6))
  cat("\n")
}

coursey.prod = read.csv('csv/CP_all.csv')
coursey = read.csv('csv/CP_cleaned.csv')
coursey.out = coursey.prod %>% filter(R >= 0.05)

coursey.cleaned = coursey.prod %>% filter(!Date %in% coursey.out$Date)

ezsummary_quantitative(coursey.cleaned)

coursey.mat = as.matrix(coursey.cleaned[,2:16])
stats.rcorr(coursey.mat)

coursey.temp = coursey.cleaned %>% mutate(Temp2 = ifelse(Avg_Temp < 25, 1,2)) %>% filter(!is.na(GPP))
high.temp = coursey.cleaned %>% filter(Avg_Temp >= 25)
low.temp = coursey.cleaned %>% filter(Avg_Temp < 25)



theme_set(theme_bw())
temp.box = coursey.cleaned %>% gather(Factor, Data, GPP:R) %>% mutate(Temp2 = ifelse(Avg_Temp < 25, 1, 2)) %>% ggplot(aes(as.factor(Temp2), Data)) + geom_boxplot(aes(color = Factor, fill = Factor)) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_fill_manual(values = c('white', '#bdbcbd', 'grey27')) + scale_color_manual(values = c('black', 'black', 'black')) + scale_x_discrete(labels = c('< 25 ºC', expression(paste(''>=25, ' ºC')))) + xlab('Temperature') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.88), legend.title = element_blank())
ggsave('figures/Figure_5.png', units = 'mm', dpi = 1200)

temp = coursey.cleaned %>% mutate(Temp2 = ifelse(Avg_Temp < 25, 'High', 'Low')) %>% gather(Factor, Data, GPP:R) %>% aov(Data ~ Temp2 * Factor, data = .)
summary(temp)
TukeyHSD(temp)

chl.box = coursey.cleaned %>% gather(Factor, Data, GPP:R) %>% mutate(Temp2 = ifelse(Avg_chl < 30, 1, 2)) %>% filter(!is.na(Avg_chl)) %>% ggplot(aes(as.factor(Temp2), Data)) + geom_boxplot(aes(color = Factor, fill = Factor)) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_fill_manual(values = c('white', '#bdbcbd', 'grey27')) + scale_color_manual(values = c('black', 'black', 'black')) + scale_x_discrete(labels = c(expression(paste('< 30 µg l'^-1)), expression(paste(''>=30, ' µg l'^-1)))) + xlab(expression(paste('Chlorophyll ', italic('a')))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.88), legend.title = element_blank())
ggsave('figures/Figure_6.png', units = 'mm', dpi = 1200)

chl = coursey.cleaned %>% mutate(Chl = ifelse(Avg_chl < 30, 'High', 'Low')) %>% filter(!is.na(Avg_chl)) %>% gather(Factor, Data, GPP:R) %>% aov(Data ~ Chl * Factor, data = .)
summary(chl)
TukeyHSD(chl)

bga.box = coursey.cleaned %>% gather(Factor, Data, GPP:R) %>% mutate(Temp2 = ifelse(Avg_BGA < 2, 1, 2)) %>% filter(!is.na(Avg_BGA)) %>% ggplot(aes(as.factor(Temp2), Data)) + geom_boxplot(aes(color = Factor, fill = Factor)) + ylab(expression("mg O"[2]*" l"^-1*" d"^-1)) + scale_fill_manual(values = c('white', '#bdbcbd', 'grey27')) + scale_color_manual(values = c('black', 'black', 'black')) + scale_x_discrete(labels = c(expression(paste('< 2 µg l'^-1)), expression(paste(''>=2, ' µg l'^-1)))) + xlab('Phycocyanin') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black"), legend.position = c(0.91, 0.88), legend.title = element_blank())
ggsave('figures/Figure_7.png', units = 'mm', dpi = 1200)

bga = coursey.cleaned %>% mutate(BGA = ifelse(Avg_BGA < 2, 'High', 'Low')) %>% filter(!is.na(Avg_BGA)) %>% gather(Factor, Data, GPP:R) %>% aov(Data ~ BGA * Factor, data = .)
summary(bga)
TukeyHSD(bga)

