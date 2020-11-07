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
    na.strings = c("", "Unavailable")
  )
dim(daS) # Raw: 5180   17
head(daS)
str(daS)

# Filter rows & cols
daS <- select (daS, -c(edition, foundry)) # 5180   15
daS <- filter(daS, category == "Paintings") # 2922   15

# Split size to width, length
daS <- daS %>%
  extract(size_cm,
          c("height", "width"),
          "([\\d.]+)[^\\d.]+([\\d.]+)",
          convert = TRUE)

# 
levels(daS$signed)
levels(daS$Medium)

daS$sales_dollar <- as.numeric(daS$sales_dollar)

daS$size_cm[1]
length(daS$size_cm)
table(daS$category)

#################### Christie's #############################
daC <-
  read.csv(
    "HK_Christies_2016-2020.csv",
    header = T,
    na.strings = c("", "Unavailable")
  )
dim(daC) # Raw: 7067   14
head(daC)

# Drop useless cols
daC <- select (daC, -c(Edition, Foundry)) # 7067   12
daC <- filter(daC, Medium_Clean == "Painting") #2508   12

# Split size to width, length, and height
# sum(is.na(daC$Medium_Clean))
daS %>%
  separate(size_cm, c("height", "width", "depth"), "x")

# Add sold_dummy col
daC$sold_dummy <- as.numeric(!is.na(daC$Sales_Price_Dollar))

#
head(daC$Size)
df %>% tidyr::extract(Size, "B")