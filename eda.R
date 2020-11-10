rm(list = ls())
setwd('G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578')

library(dpylr)
library(ggplot2)

mydata <-
  read.csv('./Data/02_CleanData/Auction_HK_2016-2020.csv', header = T)

mydata <- mydata %>%
  mutate(across(c(
    "sales_price", "low_estimate", "high_estimate"
  ), as.numeric)) %>%
  mutate(across(
    c(
      "sold_dummy",
      "auction_location",
      "auction_date",
      "signiture",
      "medium",
      "artist",
      "created",
      "auction_lot"
    ),
    as.factor
  )) %>%
  mutate(surface = height * width, .after = width) %>%
  mutate(sales_price_log = log(mydata$sales_price),
         .after = sales_price)

# levels(mydata$auction_date)
# str(mydata$auction_date)

dim(mydata)
head(mydata)
str(mydata)
summary(mydata)

sum(mydata$sold_dummy == 0) / nrow(mydata)

sum(!complete.cases(mydata)) # number of rows containing NAs
sapply(mydata, function(x)
  all(is.na(x))) # number of columns containing NAs
round(colSums(is.na(mydata)) / nrow(mydata), 2) # % of NA in each column

# plot(mydata$auction_date, mydata$sales_price)
# plot(mydata$auction_date, mydata$sales_price, ylim = c(0, 2e+07))
# plot(mydata$auction_date, mydata$sales_price, ylim = c(0,2e+07)) # give up outliers

ggplot(mydata, aes(x = sales_price_log)) +
  geom_histogram(
    aes(y = ..density..),
    color = "black",
    linetype = "dashed",
    fill = rainbow(12),
    binwidth = 12
  ) +
  geom_density(alpha = .25, fill = "lightblue") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Art Auction Prices", y = "Art Auction Prices") +
  theme_classic() + theme(legend.position = "none")

hist(mydata$sales_price)
hist(log(mydata$sales_price))

low_price <- mydata[which(mydata$sales_price < 2e+05),]
hist(low_price$sales_price)

hist(log(low_price$sales_price))

ind <- mydata$sales_price < 2e+01
hist(mydata[ind,])

# most expensive
mydata[which(mydata$sales_price > 6e+07),]
