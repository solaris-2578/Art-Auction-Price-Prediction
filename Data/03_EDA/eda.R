rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(viridis)

################################# Load data #################################
mydata <-
  read.csv(
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020.csv",
    header = T
  )

mydata <- mydata %>%
  mutate(across(c("sales_price", "estimate_range"), as.numeric)) %>%
  select(-c("artist", "title", "created")) %>%
  mutate(across(
    -c(
      "sales_price",
      "sales_price_log",
      "estimate_range",
      "surface"
    ),
    as.factor
  ))

levels(mydata$auction_location) <- c("Sotheby's", "Christie's")

# Exclude NAs rows
mydata <- mydata[complete.cases(mydata),]

mydata <- mydata[-which(mydata$sales_price==37210530),] # exclude outlier

mydata$estimate_range_log <- log(mydata$estimate_range)

dim(mydata) # 4193   25
str(mydata)
summary(mydata)

################################# Overall Distribution ###################################
ggplot(mydata, aes(x = sales_price)) +
  geom_histogram(
    aes(y = ..density..),
    color = "black",
    linetype = "dashed",
    fill = rainbow(2481),
    binwidth = 10000
  ) +
  geom_density(alpha = .25, fill = "lightblue") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Price of Paintings at Auctions)",
       y = "Price of Paintings at Auctions") +
  theme_classic() + theme(legend.position = "none")

hist(
  mydata$sales_price,
  xlab = "Art Auction Prices",
  main = "Distribution of Art Auction Prices",
  col = rainbow(31)
)

boxplot(mydata$sales_price_log)


# plot(density(mydata$sales_price_log))
ggplot(mydata, aes(x = sales_price_log)) +
  geom_histogram(
    aes(y = ..density..),
    color = "black",
    linetype = "dashed",
    fill = rainbow(31),
    binwidth = 0.4
  ) +
  geom_density(alpha = .25, fill = "lightblue") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Log Price of Paintings at Auctions",
       y = "Density") +
  theme_classic() + theme(legend.position = "none")
# title = "Distribution of Log Price of Paintings at Auctions)"

# Correlations: high correlation blw sales_price_log & estimate_range (same as EDA)
mydata_corr <-
  round(cor(mydata[, c("sales_price_log", "estimate_range", "surface")]), 2)
ggcorrplot(mydata_corr, method = "circle", type = "lower")

mydata[which(mydata$sales_price==37210530),] # most expensive

########################## Main Effects #######################################
# numerical variables
# 1. estimate_range
# ggplot(mydata,
#        aes(x = log(estimate_range), y = sales_price_log)) +
#   geom_point(alpha = .7, aes(color = auction_location)) +
#   geom_smooth(method = "lm") +
#   scale_color_viridis(discrete = TRUE, option = "D") +
#   scale_fill_viridis(discrete = TRUE) +
#   labs(title = "Log(Sales Price) vs Log(Range of Estimation)",
#        y = "Log(Sales Price)", x = "Log(Range of Estimation)")+
#   theme(legend.position = "bottom")

ggplot(mydata,
       aes(x = log(estimate_range), y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(
       y = "Log(Sales Price)", x = "Log(Range of Estimation)")
# title = "Log(Sales Price) vs Log(Range of Estimation)",
# 2. surface
plot.new()
ggplot(mydata,
       aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface", 
       y = "Log(Sales Price)", x = "Surface")


# categorical variables
# 1. auction_location: not that different
ggplot(mydata,
       aes(x = auction_location, 
           y = sales_price_log, 
           fill = auction_location)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House", 
       y = "Log(Sales Price)", x = "Auction House") +
  theme_classic() + theme(legend.position = "none")

# 2. artist_seal
ggplot(mydata,
       aes(x = artist_seal, y = sales_price_log, fill = artist_seal)) +
  geom_boxplot()
# 3. inscribed
ggplot(mydata,
       aes(x = inscribed, y = sales_price_log, fill = inscribed)) +
  geom_boxplot()
# 4. signed
ggplot(mydata,
       aes(x = signed, y = sales_price_log, fill = signed)) +
  geom_boxplot()
# 5. dated
ggplot(mydata,
       aes(x = dated, y = sales_price_log, fill = dated)) +
  geom_boxplot()
# 6. titled
ggplot(mydata,
       aes(x = titled, y = sales_price_log, fill = titled)) +
  geom_boxplot()
# 7. stamped
ggplot(mydata,
       aes(x = stamped, y = sales_price_log, fill = stamped)) +
  geom_boxplot()

mydata[which(mydata$stamped==1),]
# 8. auction_year
ggplot(mydata,
       aes(x = auction_year, y = sales_price_log, fill = auction_year)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction Year", y = "Log(Sales Price)", x = "Auction Year") +
  theme_classic() + theme(legend.position = "none")
# 9. auction_weekday
ggplot(mydata,
       aes(x = auction_weekday, y = sales_price_log, fill = auction_weekday)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction Weekday", y = "Log(Sales Price)", x = "Auction Weekday") +
  theme_classic() + theme(legend.position = "none")

# 10. auction_month
ggplot(mydata,
       aes(x = auction_month, y = sales_price_log, fill = auction_month)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Log(Sales Price) vs Auction Month", y = "Log(Sales Price)", x = "Auction Month") +
  theme(legend.position = "none")
# 11. canvas
ggplot(mydata,
       aes(x = canvas, y = sales_price_log, fill = canvas)) +
  geom_boxplot()
# 12. paper
ggplot(mydata,
       aes(x = paper, y = sales_price_log, fill = paper)) +
  geom_boxplot()
# 13. oil
ggplot(mydata,
       aes(x = oil, y = sales_price_log, fill = oil)) +
  geom_boxplot()
# 14. acrylic
ggplot(mydata,
       aes(x = acrylic, y = sales_price_log, fill = acrylic)) +
  geom_boxplot()
# 15. board
ggplot(mydata,
       aes(x = board, y = sales_price_log, fill = board)) +
  geom_boxplot()
# 16. panel
ggplot(mydata,
       aes(x = panel, y = sales_price_log, fill = panel)) +
  geom_boxplot()
# 17. masonite
ggplot(mydata,
       aes(x = masonite, y = sales_price_log, fill = masonite)) +
  geom_boxplot()
# 18. wood
ggplot(mydata,
       aes(x = wood, y = sales_price_log, fill = wood)) +
  geom_boxplot()
# 19.lacquerc
ggplot(mydata,
       aes(x = lacquer, y = sales_price_log, fill =lacquer)) +
  geom_boxplot()
# 20. ink
ggplot(mydata,
       aes(x = ink, y = sales_price_log, fill = ink)) +
  geom_boxplot()


##########################  Interaction Effects ##########################
#============================ continuous vs factor ================================
#vs estimate_range_log
ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ auction_location)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ auction_year)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ auction_month, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ auction_weekday, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ signed, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ artist_seal, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ inscribed, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ dated, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ titled, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = estimate_range_log, y = sales_price_log)) +
  geom_point(alpha = .5, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(itle = "Log(Sales Price) vs Log(Range of Estimation)",
       y = "Log(Sales Price)", x = "Log(Range of Estimation)") +
  facet_wrap(~ oil, nrow = 4, ncol = 4)


# vs surface
ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ auction_location)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ auction_year)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ auction_weekday)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ auction_month, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ signed, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ dated, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ titled, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ canvas, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ oil, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ board, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ paper, nrow = 4, ncol = 4)

ggplot(mydata, aes(x = surface, y = sales_price_log)) +
  geom_point(alpha = .7, colour = "blue4") +
  geom_smooth(method = "lm", col = "red3") +
  labs(title = "Log(Sales Price) vs Surface",
       y = "Log(Sales Price)", x = "Surface") +
  facet_wrap(~ acrylic, nrow = 4, ncol = 4)

covName_factor <- unlist(lapply(mydata, is.factor))
cov_factor <- mydata[,covName_factor]
sapply(cov_factor, table)

#============================ categorical vs categorical ================================
# vs location
ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ auction_year, ncol = 5)

ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ auction_weekday, nrow = 2, ncol = 4)

ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ auction_month, nrow = 3, ncol = 4)

ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ signed, nrow = 3, ncol = 4)

ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ oil, nrow = 3, ncol = 4)

ggplot(mydata,
       aes(x = auction_location,
           y = sales_price_log,
           fill = auction_location)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Auction House",
       y = "Log(Sales Price)", x = "Auction House") +
  theme(legend.position = "none") +
  facet_wrap(~ canvas, nrow = 3, ncol = 4)

# # location vs estimated range
# ggplot(mydata,aes(x = auction_location,
#                   y = estimate_range,
#                   fill=auction_location)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Blues") +
#   labs(title = "Log(Sales Price) vs Auction House",
#        y = "Log(Sales Price)", x = "Auction House") +
#   theme(legend.position="none") +
#   facet_wrap( ~ auction_year,ncol=5)
#
# ggplot(mydata,aes(x = auction_location,
#                   y = estimate_range,
#                   fill=auction_location)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Blues") +
#   labs(title = "Log(Sales Price) vs Auction House",
#        y = "Log(Sales Price)", x = "Auction House") +
#   theme(legend.position="none") +
#   facet_wrap( ~ auction_weekday,nrow = 2, ncol=4)
#
# ggplot(mydata,aes(x = auction_location,
#                   y = estimate_range,
#                   fill=auction_location)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Blues") +
#   labs(title = "Log(Sales Price) vs Auction House",
#        y = "Log(Sales Price)", x = "Auction House") +
#   theme(legend.position="none") +
#   facet_wrap( ~ auction_month,nrow = 3, ncol=4)

#============================ dummy vs dummy ============================
# signature
ggplot(mydata, aes(x = signed,
                   y = sales_price_log,
                   fill = signed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ inscribed)

ggplot(mydata, aes(x = signed,
                   y = sales_price_log,
                   fill = signed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ dated)

ggplot(mydata, aes(x = signed,
                   y = sales_price_log,
                   fill = signed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ titled)

ggplot(mydata, aes(x = signed,
                   y = sales_price_log,
                   fill = signed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ stamped)

ggplot(mydata, aes(x = signed,
                   y = sales_price_log,
                   fill = signed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ artist_seal)

ggplot(mydata, aes(x = artist_seal,
                   y = sales_price_log,
                   fill = artist_seal)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ inscribed)

ggplot(mydata, aes(x = inscribed,
                   y = sales_price_log,
                   fill = inscribed)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Signature",
       y = "Log(Sales Price)", x = "Signature") +
  theme(legend.position = "none") +
  facet_wrap(~ titled)

# medium
ggplot(mydata, aes(x = oil,
                   y = sales_price_log,
                   fill = oil)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ acrylic)

ggplot(mydata, aes(x = oil,
                   y = sales_price_log,
                   fill = oil)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ paper)

ggplot(mydata, aes(x = acrylic,
                   y = sales_price_log,
                   fill = acrylic)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ canvas)

ggplot(mydata, aes(x = acrylic,
                   y = sales_price_log,
                   fill = acrylic)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ paper)

# location vs medium
ggplot(mydata, aes(x = auction_location,
                   y = sales_price_log,
                   fill = auction_location)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ canvas)

ggplot(mydata, aes(x = auction_location,
                   y = sales_price_log,
                   fill = auction_location)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Log(Sales Price) vs Medium",
       y = "Log(Sales Price)", x = "Medium") +
  theme(legend.position = "none") +
  facet_wrap(~ acrylic)
