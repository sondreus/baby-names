# Load packages
library(readxl)
library(tidyverse)
library(zoo)
library(stringr)

# Load data
no <- read_xlsx('source-data/10467_20240209-213906.xlsx', skip = 1)
no[, 1] <- na.locf(no[, 1])
colnames(no) <- c('name', 'year', 'percent')
no$year <- as.numeric(no$year)
no$percent <- as.numeric(no$percent)

# Features (change, and change in percentage points, last letter of name)
no <- no[order(no$year), ]
no$change <- ave(no$percent, no$name, FUN = function(x) {
 -1 + x/c(NA, x)[1:length(x)]
})
no$change_pp <- ave(no$percent, no$name, FUN = function(x) {
  x-c(NA, x)[1:length(x)]
})
no$last_letter <- str_sub(no$name,-1,-1)

# The most popular names:
ggplot(no[no$name %in% no$name[no$percent > 3], ],
       aes(x=year, y=percent, col=name))+geom_line()+facet_grid(.~name)

ggplot(no[no$year > 1980 & no$name %in% no$name[no$percent > 1 & no$year > 1980], ],
       aes(x=year, y=percent, col=name))+geom_line()+facet_wrap(.~name)+theme(strip.text = element_text(
         size = 20))+geom_vline(aes(xintercept = 2002, linetype = "Rachael's baby in Friends is named 'Emma'"))+theme(legend.title = element_blank())

ggplot(no[no$year > 1980 & no$name %in% no$name[no$percent > 0.25 & no$year > 1980], ],
       aes(x=year, y=percent, col=name))+geom_line()+facet_wrap(.~name)+theme(strip.text = element_text(
         size = 20))+theme(legend.title = element_blank())+xlab('')+guides(col='none')+ylab('')

ggplot(no[no$year > 1980 & no$name %in% no$name[no$percent > 0.5 & no$year > 1980] & no$last_letter == 'a', ],
       aes(x=year, y=change_pp, col=name))+geom_smooth(se = F, method = 'loess', span = 0.2)+facet_wrap(.~name)+theme(strip.text = element_text(
         size = 20))+geom_hline(aes(yintercept = 0))+theme(legend.title = element_blank())+ggtitle('Change in popularity')


ggplot(no[no$year > 1980 & no$name %in% c('Ada',
                                          'Diana',
                                          'Ea',
                                          'Ella',
                                          'Embla',
                                          'Luna',
                                          'Nora',
                                          "Saga",
                                          "Sofia",
                                          "Sonia",
                                          "Stella"), ],
       aes(x=year, y=percent, col=name))+geom_line()+facet_wrap(.~name)+theme(strip.text = element_text(
         size = 20))+theme(legend.title = element_blank())+xlab('')+guides(col='none')+ylab('')+ggtitle('Selected names, by % named in year, Norway')


