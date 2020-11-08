rm(list = ls())
setwd(
  'G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/01_DataWrangling'
)

library(dplyr)
library(tidyr)

#################### Sotheby's #############################
daS <-
  read.csv(
    "HK_Sothebys_2016-2020.csv",
    header = T,
    na.strings = c("", "Unavailable", "[not communicated]")
  )

# Filter rows
daS <- filter(daS, category == "Paintings") %>%
  droplevels()

# Split
daS <- daS %>%
  extract(size_cm,
          c("height", "width"),
          "([\\d.]+)[^\\d.]+([\\d.]+)",
          convert = TRUE)

# daS <- daS %>%
#   separate(Medium,
#            c("A", "B"),
#            "/") %>%
#   mutate(across(c("A", "B"), as.factor))

# Drop cols
daS <-
  select (daS, -c(edition, foundry, online_dummy, category)) # 2922   13

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
dim(daS) # 2922   14
head(daS)
str(daS)

levels(daS$signiture)
levels(daS$medium)

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
daC <- select (daC, -c(Edition, Foundry, Medium_Clean))

# Reorder & Rename & Convert
daC <- daC %>%
  relocate(sold_dummy, .after = Sales_Price_Dollar)
daC <- daC %>%
  rename_at(vars(colnames(daC)), ~ colnames(daS))

daC$sales_price <- as.numeric(as.character(daC$sales_price))
daC$sold_dummy <- as.numeric(as.character(daC$sold_dummy))
daC$low_estimate <- as.numeric(as.character(daC$low_estimate))
daC$high_estimate <- as.numeric(as.character(daC$high_estimate))

# Display
dim(daC) # Raw: 2508   14
head(daC)
str(daC)

#################### Merge #############################
mydata <- bind_rows(daS, daC)
mydata$auction_location <- factor(
  mydata$auction_location,
  levels = c("Sotheby_HK", "Christie_HK"),
  labels = c(0, 1)
)
mydata <- mydata %>%
  mutate(surface = height * width) %>%
  relocate(surface, .after = width)

dim(mydata)
head(mydata)
str(mydata)

write.csv(mydata , file = "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020.csv", row.names = FALSE)

# mydata <- mydata %>%
#   mutate(auction_date = as.Date(auction_date, format="%m/%d/%Y")) %>%
#   arrange(desc(auction_date))