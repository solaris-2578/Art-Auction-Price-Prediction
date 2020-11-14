rm(list = ls())
setwd('G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578')

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)

mydata <-
  read.csv('./Data/02_CleanData/Auction_HK_2016-2020.csv', header = T)

mydata <- mydata %>%
  mutate(across(c("sales_price", "estimate_range"), as.numeric)) %>%
  mutate(across(
    c(
      "auction_location",
      "artist_seal",
      "inscribed",
      "signed",
      "dated",
      "titled",
      "stamped",
      "auction_weekday",
      "canvas",
      "paper",
      "oil",
      "acrylic"
    ),
    as.factor
  ))

colnames(mydata)[15] <- "auction_month"

dim(mydata)
str(mydata)

# NAs
sum(mydata$sold_dummy == 0) / nrow(mydata)

sum(!complete.cases(mydata)) / nrow(mydata) # % of rows containing NAs

sapply(mydata, function(x)
  all(is.na(x))) # % of columns containing NAs

round(colSums(is.na(mydata)) / nrow(mydata), 2) # % of NA in each column

# Distribution
hist(mydata$sales_price)

hist(log(mydata$sales_price))
plot(density(mydata$sales_price_log))
ggplot(mydata, aes(x = sales_price_log)) +
  geom_histogram(
    aes(y = ..density..),
    color = "black",
    linetype = "dashed",
    fill = rainbow(32),
    binwidth = 0.4
  ) +
  geom_density(alpha = .25, fill = "lightblue") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Art Auction Prices", y = "Art Auction Prices") +
  theme_classic() + theme(legend.position = "none")

# most expensive
mydata[which(mydata$sales_price > 6e+07), ]
