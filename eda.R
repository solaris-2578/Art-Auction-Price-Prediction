rm(list = ls())
setwd(
  'G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578'
)

library(ggplot2)

mydata <- read.csv('./Data/02_CleanData/Auction_HK_2016-2020.csv', header = T)

dim(mydata)
head(mydata)
str(mydata)
summary(mydata)

sum(mydata$sold_dummy == 0)/nrow(mydata)
plot(mydata$auction_date, mydata$sales_price, ylim = c(0,2e+07)) # give up outliers



