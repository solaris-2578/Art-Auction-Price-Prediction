setwd('G:/Duke/MIDS_F20/IDS702/Final Project/artwork_auction')
rm(list = ls())

library(gdata)


da <- read.xls('HK-2016-2020.xls',
               sheet = 1,
               header = TRUE,)
dim(da)
head(da)
str(da)
