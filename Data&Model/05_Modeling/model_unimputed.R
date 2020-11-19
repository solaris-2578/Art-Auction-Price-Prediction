rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(rms) #for VIF
library(MASS)
library(sjPlot)

# Load data
mydata <-
  read.csv(
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data&Model/02_CleanData/Auction_HK_2016-2020.csv",
    header = T
  )

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
      "auction_year",
      "auction_weekday",
      "auction_month",
      "canvas",
      "paper",
      "oil",
      "acrylic"
    ),
    as.factor
  ))

levels(mydata$auction_location) <- c("Sotheby's", "Christie's")
levels(mydata$artist_seal) <- c("No Artist's Seal", "Artist's Seal")
levels(mydata$inscribed) <- c("Uninscribed", "Inscribed")
levels(mydata$signed) <- c("Unsigned", "Signed")
levels(mydata$dated) <- c("Undated", "Dated")
levels(mydata$titled) <- c("Untitled", "Titled")
levels(mydata$stamped) <- c("Unstamped", "Stamped")
levels(mydata$canvas) <- c("Otherwise", "Canvas")
levels(mydata$paper) <- c("Otherwise", "Paper")
levels(mydata$oil) <- c("Otherwise", "Oil")
levels(mydata$acrylic) <- c("Otherwise", "Acrylic")

# Exclude NAs rows
mydata <- mydata[complete.cases(mydata),]

# Mean centering
mydata$estimate_range_c <-
  mydata$estimate_range - mean(mydata$estimate_range)
mydata$surface_c <- mydata$surface - mean(mydata$surface)


colMeans(mydata[, c("estimate_range", "surface")]) # for interpretation

covName_factor <- unlist(lapply(mydata, is.factor))
cov_factor <- mydata[,covName_factor]
sapply(cov_factor, table)
# summary
dim(mydata) #4194   22
str(mydata)

################################### Model from EDA ###################################
cov_names <- names(mydata)
cov_names <-
  cov_names[!cov_names %in% c(
    "artist",
    "title",
    "sales_price",
    "sales_price_log",
    "estimate_range",
    "surface"
  )]

M1_formula <- as.formula(
  paste(
    "sales_price_log ~",
    paste(cov_names,
          collapse = " + "),
    " + (surface_c+auction_location)*(auction_year+auction_weekday+auction_month)+ (canvas + paper)*(oil+acrylic)"
  )
)

M1 <- lm(M1_formula, data = mydata)
summary(M1)

M2 <- lm(
  sales_price_log ~
    artist_seal + dated + auction_month + estimate_range_c +
    canvas * (oil + acrylic) +
    auction_location * auction_year,
  data = mydata
)
summary(M2)

M3 <- lm(
  sales_price_log ~
    auction_location + artist_seal + signed +
    auction_month + auction_weekday + auction_year +
    estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)
summary(M3) # good
# tab_model(M3)

M3_no_year <- lm(
  sales_price_log ~
    auction_location + artist_seal + signed +
    auction_month + auction_weekday +
    estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)

anova(M3_no_year, M3) # throw out year

M3_no_year_weekday <- lm(
  sales_price_log ~
    auction_location + artist_seal + signed +
    auction_month +
    estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)
anova(M3_no_year, M3_no_year_weekday) # keep weekday

M3_no_year_month <- lm(
  sales_price_log ~
    auction_location + artist_seal + signed +
    auction_weekday +
    estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)
anova(M3_no_year, M3_no_year_month) # keep month

M3_no_year <- lm(
  sales_price_log ~
    auction_location + artist_seal + signed +
    auction_month + auction_weekday +
    estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)

anova(M3_no_year, M3)

M4 <- lm(
  sales_price_log ~
    artist_seal + auction_month + auction_year + estimate_range_c +
    canvas * (oil + acrylic),
  data = mydata
)
summary(M4)
anova(M3, M4)


M5 <- lm(
  sales_price_log ~
    artist_seal + auction_month + auction_year + estimate_range_c,
  data = mydata
)
summary(M5)
anova(M3, M5)

# Model Assessment
ggplot(mydata, aes(x = estimate_range_c, y = M3$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept = 0, col = "red3") + theme_classic() +
  labs(title = "Residuals vs Log of Estimated Range (Centered)",
       x = "Log of Estimated Range (Centered)",
       y = "Residuals")

ggplot(mydata, aes(x = surface_c, y = M3$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept = 0, col = "red3") + theme_classic() +
  labs(title = "Residuals vs Surface (Centered)",
       x = "Surface (Centered)",
       y = "Residuals")

plot(M3, which = 1:5, col = c("blue4"))
vif(M3)

################################### Stepwise Regression ###################################
NullModel <- lm(sales_price_log ~ 1, data = mydata)