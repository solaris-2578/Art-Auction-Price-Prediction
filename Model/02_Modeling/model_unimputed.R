rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(rms) #for VIF
library(MASS)
library(sjPlot)
library(arm)
library(pROC)
library(e1071)
library(caret)

################################# Load data #################################
mydata <-
  read.csv(
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020.csv",
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

# Exclude NAs rows
mydata <- mydata[complete.cases(mydata),]

# Mean centering
mydata$estimate_range_c <-
  mydata$estimate_range - mean(mydata$estimate_range)
mydata$surface_c <- mydata$surface - mean(mydata$surface)


colMeans(mydata[, c("estimate_range", "surface")]) # for interpretation

# Check covariate balance
covName_factor <- unlist(lapply(mydata, is.factor))
cov_factor <- mydata[,covName_factor]
sapply(cov_factor, table)

# summary
dim(mydata)
str(mydata)
summary(mydata)

################################### Model from EDA ###################################
cov_names <- names(mydata); cov_names
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
          collapse = " + ")
  )
)

M1 <- lm(M1_formula, data = mydata)
summary(M1) # NA exist

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
summary(M3) 
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

summary(M3_no_year)
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

########################### Stepwise Regression #########################
da <- mydata[,-c(1,2,3,5,6)]
n <- nrow(da)

# dim(da)
# str(da)

mN <- lm(sales_price_log ~ 1, data = da)
summary(mN)
mF <- lm(sales_price_log ~ .^2, data = da)
summary(mF)

mfor <- step(mN, scope = formula(mF), direction="forward",trace=0)
mfor$call
summary(mfor)

mback <- step(mF, direction = "backward", trace = 0)
mback$call
summary(mback)

mbi <- step(mN, scope = formula(mF), direction = "both", trace = 0)
mbi$call
summary(mbi)

library(leaps)
mfor1 <- regsubsets(sales_price_log ~ .^2, data = da, method = "forward")
summary(mfor1)
########################### Non-parametric Methods ################################
# cart
library(tree)
m_cart <- tree(sales_price_log ~ ., data = da)
summary(m_cart)
plot(m_cart)
text(m_cart)
m_cart
head(predict(m_cart,type="class"))

# bagging
library(randomForest)
m_bagg <- randomForest(sales_price_log ~ ., data = da, mtry=4)
m_bagg

# Random forest
m_rf <- randomForest(sales_price_log ~ ., data = da, importance =TRUE)
m_rf
varImpPlot(m_rf)
pred_rf <- predict(m_rf, type="response")

# Boosting
library(gbm)
m_boost <-  gbm(sales_price_log ~ ., data = da, 
                  distribution="gaussian",n.trees=5000, interaction.depth=2)
summary(m_boost)
pred_boost <- predict(m_boost,n.trees=500,type="response")
################################### Prediction ###################################
confint(model_final, level = 0.95)



############################ cross-validation ####################################
set.seed(123) 
da <- mydata[,-c(1,2,3,5,6)]
# randomly re-shuffle the data
da <- da[sample(nrow(da)),]
# Define the number of folds 
K <- 10
# Define a matrix to save results into
RSME <- matrix(0,nrow=K,ncol=1)
# Split the row indexes into k equal parts
kth_fold <- cut(seq(1,nrow(da)),breaks=10,labels=FALSE)
# Now write the for loop for the k-fold cross validation
k = 1
for(k in 1:10){
  # Split data into the training and test datasets
  test_index <- which(kth_fold==k)
  train <- da[-test_index,]
  test <- da[test_index,]
  rf <- randomForest(sales_price_log ~ ., data = train)
  y_test_pred <- predict(rf, test)
  testMSE <- mean((log(test$sales_price)- y_test_pred)^2);
  RSME[k,] <- sqrt(testMSE)
}

#Calculate the average of all values in the RSME matrix 
rsme1 = mean(RSME)