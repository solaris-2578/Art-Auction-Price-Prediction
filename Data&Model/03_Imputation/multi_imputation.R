rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(rms) #for VIF
library(MASS)
library(sjPlot)
library(mice)
library(VIM)
library(lattice)

################################# Load data #################################
mydata <-
  read.csv(
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data&Model/02_CleanData/Auction_HK_2016-2020.csv",
    header = T
  )

mydata <- mydata %>%
  mutate(across(c("sales_price", "estimate_range"), as.numeric)) %>%
  mutate(across(
    -c(
      "artist",
      "title",
      "sales_price",
      "sales_price_log",
      "estimate_range",
      "surface"
    ),
    as.factor
  ))

levels(mydata$auction_location) <- c("Sotheby's", "Christie's")
# levels(mydata$artist_seal) <- c("No Artist's Seal", "Artist's Seal")
# levels(mydata$inscribed) <- c("Uninscribed", "Inscribed")
# levels(mydata$signed) <- c("Unsigned", "Signed")
# levels(mydata$dated) <- c("Undated", "Dated")
# levels(mydata$titled) <- c("Untitled", "Titled")
# levels(mydata$stamped) <- c("Unstamped", "Stamped")
# levels(mydata$canvas) <- c("Otherwise", "Canvas")
# levels(mydata$paper) <- c("Otherwise", "Paper")
# levels(mydata$oil) <- c("Otherwise", "Oil")
# levels(mydata$acrylic) <- c("Otherwise", "Acrylic")


########################### Patterns of missing data ##########################
# round(colSums(is.na(mydata)) / nrow(mydata), 4) # % of NA in each column
cov_na <- mydata[,c("estimate_range", "surface")]

md.pattern(mydata, rotate.names = TRUE)
aggr(
  mydata,
  col = c("lightblue3", "darkred"),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(mydata),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion missing", "Missingness pattern")
)
marginplot(
  cov_na,
  xlab = "Range of Estimation",
  ylab = "Surface"
)

########################## Multiple Imputation ###############################
set.seed(123)
mydata_imp <- mice(
  mydata,
  m = 10,
  defaultMethod = c("norm", "logreg", "polyreg", "polr"),
  print = F
)
