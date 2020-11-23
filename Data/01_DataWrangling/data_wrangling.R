rm(list = ls())
setwd("G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/01_DataWrangling/")
library(tidyverse)
library(lubridate)

#################### Sotheby's #############################
daS <-
  read.csv(
    "HK_Sothebys_2016-2020.csv",
    header = T,
    na.strings = c("", "Unavailable", "[not communicated]")
  )

# Format date: Only keep starting date
daS <- daS %>%
  separate(auction_date, c("auction_date", NA), "-")
daS$auction_date <-
  as.factor(parse_date_time(daS$auction_date, c("mdy", "Ymd")))
# levels(daS$auction_date)
# str(daS$auction_date)

# Filter rows
daS <- filter(daS, category == "Paintings") %>%
  droplevels()

# Split
daS <- daS %>%
  extract(size_cm,
          c("height", "width"),
          "([\\d.]+)[^\\d.]+([\\d.]+)",
          convert = TRUE)

# Drop cols
daS <-
  select (daS,-c(edition, foundry, online_dummy, category)) 

# Rename & Convert
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}
daS <- daS %>%
  rename(
    sales_price = sales_dollar,
    sold_dummy = dummy_sold,
    low_estimate = low_estimate_dollar,
    high_estimate = high_estimate_dollar,
    signiture = signed,
    medium = Medium
  )

daS$sales_price <- as.numeric(as.character(daS$sales_price))
daS$sold_dummy <- as.numeric(as.character(daS$sold_dummy))
daS$low_estimate <- as.numeric(as.character(daS$low_estimate))
daS$high_estimate <- as.numeric(as.character(daS$high_estimate))

daS$auction_location <- "Sotheby_HK"

# Display
# dim(daS) # 2922   14
# head(daS)
# str(daS)
#
# levels(daS$signiture)
# levels(daS$medium)

#################### Christie's #############################
daC <-
  read.csv(
    "HK_Christies_2016-2020.csv",
    header = T,
    na.strings = c("", "Unavailable", "n/a")
  )

# Filter rows
daC <- filter(daC, Medium_Clean == "Painting")  %>%
  droplevels()

# Split size to width, length
daC <- daC %>%
  separate(Size, c(NA, "size_cm"), sep = "\\s+\\s+") %>%
  extract(size_cm,
          c("height", "width"),
          "([\\d.]+)[^\\d.]+([\\d.]+)",
          convert = TRUE)

# Add/Drop cols
daC$sold_dummy <- as.numeric(!is.na(daC$Sales_Price_Dollar))
daC$auction_location <- "Christie_HK"
daC <- select (daC,-c(Edition, Foundry, Medium_Clean))

# Reorder & Rename & Convert
daC <- daC %>%
  relocate(sold_dummy, .after = Sales_Price_Dollar)
daC <- daC %>%
  rename_at(vars(colnames(daC)), ~ colnames(daS))

daC$sales_price <- as.numeric(as.character(daC$sales_price))
daC$sold_dummy <- as.numeric(as.character(daC$sold_dummy))
daC$low_estimate <- as.numeric(as.character(daC$low_estimate))
daC$high_estimate <- as.numeric(as.character(daC$high_estimate))

# Format date
daC$auction_date <-
  as.factor(parse_date_time(daC$auction_date, "mdY"))
# levels(daS$auction_date)
# str(daS$auction_date)

# Display
# dim(daC) # Raw: 2508   14
# head(daC)
# str(daC)

#################### Merge #############################
mydata <- bind_rows(daS, daC)

mydata$auction_location <- factor(
  mydata$auction_location,
  levels = c("Sotheby_HK", "Christie_HK"),
  labels = c(0, 1)
)

colnames(mydata)[1] <- "artist"

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
      "artist",
      "auction_location"
    ),
    as.factor
  )) %>%
  mutate(
    surface = height * width,
    sales_price_log = log(mydata$sales_price),
    estimate_range = high_estimate - low_estimate
  ) %>%
  select(-c(
    height,
    width,
    high_estimate,
    low_estimate,
    auction_lot,
    sold_dummy
  )) %>%
  relocate(c(sales_price_log, estimate_range), .after = sales_price)

# without imputation
mydata <- mydata[!is.na(mydata$sales_price),]

# signature
mydata$artist_seal <- 0
mydata$artist_seal[mydata$signiture == "Artist's Seal"] <- 1
mydata$artist_seal <- as.factor(mydata$artist_seal)
# table(mydata$artist_seal)

mydata$inscribed <- 0
mydata$inscribed[grep("Inscribed", mydata$signiture, fixed = TRUE)] <-
  1
mydata$inscribed <- as.factor(mydata$inscribed)
# table(mydata$inscribed)

mydata$signed <- 0
mydata$signed[grep("Signed", mydata$signiture, fixed = TRUE)] <- 1
mydata$signed <- as.factor(mydata$signed)
# table(mydata$signed)

mydata$dated <- 0
mydata$dated[grep("Dated", mydata$signiture, fixed = TRUE)] <- 1
mydata$dated <- as.factor(mydata$dated)
# table(mydata$dated) # Only somewhat balanced one

mydata$titled <- 0
mydata$titled[grep("Titled", mydata$signiture, fixed = TRUE)] <- 1
mydata$titled <- as.factor(mydata$titled)
# table(mydata$titled)

mydata$stamped <- 0
mydata$stamped[grep("Stamped", mydata$signiture, fixed = TRUE)] <- 1
mydata$stamped <- as.factor(mydata$stamped)
# table(mydata$stamped)

# auction_date
mydata$auction_date <- as.Date(mydata$auction_date, tz = "")
# table(mydata$auction_date)
mydata$auction_year <- year(mydata$auction_date)
mydata$auction_month <- month(mydata$auction_date)
mydata$auction_weekday <- weekdays(mydata$auction_date)

# medium
medium_pre <- mydata$medium
# write.csv(medium_pre, file = "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/01_DataWrangling/medium_pre.csv", row.names = FALSE)
medium_post <-
  read.csv(
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/01_DataWrangling/medium_post_nlp.csv",
    header = T
  )

# medium_post$substrate <- 0 #otherwise
# medium_post$substrate[medium_post$canvas == 1] <- 1 #canvas
# medium_post$substrate[medium_post$paper == 1] <- 2 #paper
# medium_post$substrate <- factor(medium_post$substrate, 
#                                 levels = c(0,1,2),
#                                 labels = c("Otherwise","Canvas","Paper"))
# # table(medium_post$substrate)
# 
# medium_post$medium <- 0 #otherwise
# medium_post$medium[medium_post$acrylic == 1] <- 2 #acrylic
# medium_post$medium[medium_post$oil == 1] <- 1 #oil
# medium_post$medium <- as.factor(medium_post$medium)
# table(medium_post$medium)

medium_post <- medium_post[, -c(1:4)]

mydata <-
  select (mydata,-c(signiture, medium, auction_date))


mydata <- bind_cols(mydata, medium_post)

dim(mydata) # 4397   20
head(mydata, 10)
str(mydata)
summary(mydata)

write.csv(mydata , file = "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020.csv", row.names = FALSE)
