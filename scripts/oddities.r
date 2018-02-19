library(tidyverse)
library(lubridate)
library(ggforce)
library(gridExtra)
source('scripts/windrose.r')

coursey.prod = read.csv('csv/CP_all.csv')
coursey = read.csv('csv/CP_cleaned.csv')


odd_days = coursey.prod %>% filter(Slope_Day <= -1)
days = levels(as.factor(as_date(odd_days$Date)))
days_length = length(days)
day_one = coursey %>% filter(Date_2 %in% days)

pdf('windrose.pdf')
for(i in 1:length(days)){
  blarg = day_one %>% filter(Date_2 == days[i])
  plot.windrose(data = blarg, spd = 'WSpd', dir = 'Wdir')
}
dev.off()

high.par.days = day_one %>% group_by(Date_2) %>% summarise(Max_PAR = max(TotPAR)) %>% filter(Max_PAR >= 1200) %>% select(Date_2)

par.days = levels(as.factor(as_date(high.par.days$Date_2)))

pdf('high_par.pdf')
for(i in 1:length(par.days)){
  blarg = day_one %>% filter(Date_2 == par.days[i])
  plot.windrose(data = blarg, spd = 'WSpd', dir = 'Wdir')
}
dev.off()

high_par = day_one %>% filter(Date_2 %in% par.days)
summary(high_par$Wind_Dir)

test = plot.windrose(high_par, spd = 'WSpd', dir = 'Wdir')

page1 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 1)
page2 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 2)
page3 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 3)
page4 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 4)
page5 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 5)
page6 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 6)
page7 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 7)
page8 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 8)
page9 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 9)
page10 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 10)
page11 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 11)
page12 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 12)
page13 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 13)
page14 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 14)
page15 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 15)
page16 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 16)
page17 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 17)
page18 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 18)
page19 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 19)
page20 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 20)
page21 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 21)
page22 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 22)
page23 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 23)
page24 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 24)
page25 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 25)
page26 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 26)
page27 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 27)
page28 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 28)
page29 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 29)
page30 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 30)
page31 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 31)
page32 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 32)
page33 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 33)
page34 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 34)
page35 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 35)
page36 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 36)
page37 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 37)
page38 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 38)
page39 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 39)
page40 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 40)
page41 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 41)
page42 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 42)
page43 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 43)
page44 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 44)
page45 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 45)
page46 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 46)
page47 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 47)
page48 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 48)
page49 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 49)
page50 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 50)
page51 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 51)
page52 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 52)
page53 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 53)
page54 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 54)
page55 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 55)
page56 = test + facet_wrap_paginate(~Date_2, ncol = 2, nrow = 1, page = 56)

plots.list = list(page1, page2, page3, page4, page5, page6, page7, page8, page9, page10, page11, page12, page13, page14, page15, page16, page17, page18, page19, page20, page21, page22, page23, page24, page25, page26, page27, page28, page29, page30, page31, page32, page33, page34, page35, page36, page37, page38, page39, page40, page41, page42, page43, page44, page45, page46, page47, page48, page49, page50, page51, page52, page53, page54, page55, page56)



pdf('high_par.pdf', height = 2.5, width = 7)
plots.list
dev.off()

