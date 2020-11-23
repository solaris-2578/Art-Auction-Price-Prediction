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
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020.csv",
    header = T
  )

mydata <- mydata[-which(mydata$estimate_range > 2e+07),] # exclude outlier of range
mydata <- mydata[-which(mydata$surface > 2.5e+05),] # exclude outlier of surface

  
drops <- c("artist", "title", "created")

mydata <- mydata[, !(names(mydata) %in% drops)]

mydata <- mydata %>%
  mutate(across(c("estimate_range", "surface", "sales_price", "sales_price_log"), as.numeric)) %>%
  mutate(across(
    -c("estimate_range", "surface", "sales_price", "sales_price_log"),
    as.factor
  ))



dim(mydata) #4395   24
str(mydata)

########################### Patterns of missing data ##########################
cov_na <- mydata[,c("estimate_range_log", "surface")]

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
ini <- mice(mydata, maxit = 0)
meth <- ini$meth; meth
meth[c("estimate_range","surface")] <- c("norm", "cart"); meth
pred <- ini$pred; pred
pred[, c(1:2)] <- 0; pred
post <- ini$post
post["estimate_range"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(100, Inf))"
imp <- mice(mydata, meth = meth, pred = pred, post=post, m = 10, print = F)
# examine convergenve
plot(imp)
# diagnostic checking
stripplot(imp, estimate_range~.imp, pch = 20, cex = 1)
stripplot(imp, surface~.imp, pch = 20, cex = 1)
stripplot(imp, estimate_range ~ surface | .imp, pch = 20, cex = 1)
densityplot(imp)
# Check the first imputed dataset
complete(imp, 1)[1:15, c("estimate_range","surface")]
complete(imp, 2)[1:15, c("estimate_range","surface")]

for (i in 1:10){
  nam <- paste("comp", i, sep = "")
  assign(nam, complete(imp, i))
}

nrow(comp7[which(comp7$estimate_range<0),])

write.csv(comp7 , file = "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020_imputed.csv", row.names = FALSE)
