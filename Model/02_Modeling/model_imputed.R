rm(list = ls())

library(tidyverse)
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
    "G:/Duke/MIDS_F20/IDS702/Final Project/final-project-solaris-2578/Data/02_CleanData/Auction_HK_2016-2020_imputed.csv",
    header = T
  )


mydata <- mydata %>%
  mutate(across(
    c("estimate_range", "surface", "sales_price", "sales_price_log"),
    as.numeric
  )) %>%
  mutate(across(
    -c("estimate_range", "surface", "sales_price", "sales_price_log"),
    as.factor
  ))

mydata$estimate_range_log <- log(mydata$estimate_range)

# Mean centering
mydata$estimate_range_log_c <-
  mydata$estimate_range_log - mean(mydata$estimate_range_log)
mydata$surface_c <- mydata$surface - mean(mydata$surface)

dim(mydata) #4395   27
str(mydata)
summary(mydata)

# discard outliers
mydata <-
  mydata[abs(stdres(mfinal)) <= 3, ] # less than 0.8% of the whole dataset
dim(mydata) #4358   27
################################ Model from EDA ###################################
m1 <- lm(
  sales_price_log ~ estimate_range_log_c +
    surface_c * (auction_weekday + titled) +
    signed +
    auction_month +
    auction_year * auction_location +
    canvas + paper + oil + acrylic + ink,
  data = mydata
)
summary(m1)

m2 <- lm(
  sales_price_log ~ estimate_range_log_c +
    auction_weekday +
    auction_month +
    auction_location +
    signed +
    canvas + paper + oil + acrylic,
  data = mydata
)
summary(m2)

m22 <- lm(
  sales_price_log ~ estimate_range_log_c +
    auction_weekday +
    auction_month +
    signed +
    canvas + paper + oil + acrylic +
    auction_year + auction_location,
  data = mydata
)
summary(m22)
anova(m2, m22) # year is not necessary --> m2

################################ Stepwise Regression ###################################
da <- mydata[, -c(1, 3, 5, 25)]
n <- nrow(da)

# dim(da) 4395   23
# str(da)

mN <- lm(sales_price_log ~ 1, data = da)
summary(mN)
mF <- lm(sales_price_log ~ . ^ 2, data = da)
summary(mF)
terms_mF <- terms(mF)
terms_mF
mF <- update(mF,
             formula = drop.terms(
               terms_mF,
               c(29:31,
                 44:48, 50:51, 55, 58, 61,
                 65:67, 69:70,
                 83:85,
                 101:102, 104),
               keep.response = TRUE
             ))

mF <- update(mF,
             formula = drop.terms(terms_mF,
                                  c(94, 96, 97, 104,
                                    111, 112, 119:120),
                                  keep.response = TRUE))

mF <- update(mF,
             formula = drop.terms(terms_mF,
                                  c(114, 117),
                                  keep.response = TRUE))
mF <- update(mF,
             formula = drop.terms(terms_mF,
                                  c(116),
                                  keep.response = TRUE))

mF <- update(mF,
             formula = drop.terms(
               terms_mF,
               c(128, 134:138,
                 146:150,
                 186, 195, 200, 204:206, 213),
               keep.response = TRUE
             ))
saveRDS(mF, "mF.rds")
mF <- readRDS("mF.rds")

mfor <- step(mN,
             scope = formula(mF),
             direction = "forward",
             trace = 0)
mfor$call
summary(mfor)

mback <- step(mF, direction = "backward", trace = 0)
mback$call
summary(mback)

mbi <- step(mN,
            scope = formula(mF),
            direction = "both",
            trace = 0)
mbi$call
summary(mbi)

mfor_1 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month + auction_weekday +
      paper + lacquer + oil + wood + canvas + titled +
      auction_location + oil:auction_location + auction_location:surface_c +
      surface_c + auction_weekday:surface_c + surface_c:canvas + surface_c:wood +
      estimate_range_log_c:lacquer +
      paper + oil:paper + oil:titled,
    data = mydata
  )
anova(mfor_1, mfor) # --> no auction_location:canvas
summary(mfor_1)

mfor_2 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month + auction_weekday +
      paper + lacquer + oil + wood + titled + canvas +
      auction_location + oil:auction_location + auction_location:surface_c +
      surface_c + auction_weekday:surface_c + surface_c:canvas + surface_c:wood +
      estimate_range_log_c:lacquer +
      paper + oil:paper + oil:titled,
    data = mydata
  )
anova(mfor_1, mfor_2)
summary(mfor_2)

mfor_3 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month + auction_weekday +
      paper + lacquer + oil + wood + titled + canvas +
      auction_location + oil:auction_location +
      surface_c + auction_weekday:surface_c + surface_c:canvas + surface_c:wood +
      estimate_range_log_c:lacquer +
      paper + oil:paper + oil:titled,
    data = mydata
  )
anova(mfor_2, mfor_3)  # no auction_location:surface_c +
summary(mfor_3)

mbi2 <-
  step(mN,
       scope = formula(mfor_3),
       direction = "both",
       trace = 0)
mbi2$call
summary(mbi2)

mbi22 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month +
      auction_weekday + paper + lacquer + oil +
      wood + surface_c + canvas + titled++oil:titled + estimate_range_log_c:lacquer +
      paper:oil + surface_c:canvas + wood:surface_c,
    data = da
  )
summary(mbi22)

mbi23 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month +
      auction_weekday + paper + lacquer + oil +
      wood + surface_c + canvas + titled++oil:titled +
      paper:oil + surface_c:canvas + ,
    data = da
  )
summary(mbi23)
anova(mbi23, mbi22) # no estimate_range_log_c:lacquer

mbi24 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month +
      auction_weekday + paper + lacquer + oil +
      wood + surface_c + canvas + titled++oil:titled +
      paper:oil + surface_c:canvas ,
    data = da
  )
anova(mbi23, mbi24) # no wood:surface_c
summary(mbi24)

mbi25 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month +
      auction_weekday + paper + lacquer + oil +
      wood + surface_c + canvas + titled++oil:titled +
      surface_c:canvas ,
    data = da
  )
anova(mbi25, mbi24) # no paper:oil
summary(mbi25)

mbi26 <-
  lm(
    sales_price_log ~ estimate_range_log_c + surface_c + auction_weekday + auction_month +
      paper + lacquer + oil +
      wood + canvas + titled++oil:titled,
    data = da
  )
anova(mbi25, mbi26) # no oil:titled
summary(mbi26)

mbi261 <-
  lm(
    sales_price_log ~ estimate_range_log_c + surface_c + auction_weekday + auction_month +
      paper + lacquer + oil +
      wood + canvas + oil:titled + titled + auction_location
    ,
    data = da
  )
anova(mbi261, mbi26) # add auction_location
summary(mbi261)

mbi262 <-
  lm(
    sales_price_log ~ estimate_range_log_c + surface_c + auction_weekday + auction_month +
      paper + lacquer +
      wood + canvas + titled + auction_location
    ,
    data = da
  )
anova(mbi261, mbi262) #-----back to mbi261

mbi27 <-
  lm(
    sales_price_log ~ estimate_range_log_c + auction_month +
      auction_weekday + paper + lacquer + oil +
      wood + surface_c + canvas + titled,
    data = da
  )
anova(mbi27, mbi26) # need oil:titled----> mbi26
mfinal <- mbi261


mbi3 <-
  step(mN,
       scope = formula(mbi261),
       direction = "both",
       trace = 0)
summary(mbi3) # converge to mbi261

############################## Model Assessment ##############################
ggplot(mydata, aes(x = estimate_range_log_c, y = mfinal$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept = 0, col = "red3") + theme_classic() +
  labs(title = "Residuals vs Log of Estimated Range (Centered)",
       x = "Log of Estimated Range (Centered)",
       y = "Residuals")

ggplot(mydata, aes(x = surface_c, y = mfinal$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept = 0, col = "red3") + theme_classic() +
  labs(title = "Residuals vs Surface (Centered)",
       x = "Surface (Centered)",
       y = "Residuals")

plot(mfinal, which = 1:5, col = c("blue4"))
vif(mfinal)
########################### Non-parametric Methods ################################
# cart
library(tree)
m_cart <- tree(sales_price_log ~ ., data = da)
summary(m_cart)
plot(m_cart)
text(m_cart)
m_cart
head(predict(m_cart, type = "class"))

# bagging
library(randomForest)
m_bagg <- randomForest(sales_price_log ~ ., data = da, mtry = 4)
m_bagg

# Random forest
m_rf <-
  randomForest(sales_price_log ~ ., data = da, importance = TRUE)
m_rf
varImpPlot(m_rf)
pred_rf <- predict(m_rf, type = "response")

# Boosting
library(gbm)
m_boost <-  gbm(
  sales_price_log ~ .,
  data = da,
  distribution = "gaussian",
  n.trees = 5000,
  interaction.depth = 2
)
summary(m_boost)
pred_boost <- predict(m_boost, n.trees = 500, type = "response")


############################ cross-validation ####################################
###### MLR
set.seed(123)
# randomly re-shuffle the data
da <- da[sample(nrow(da)), ]
# Define the number of folds
K <- 10
# Define a matrix to save results into
RSME <- matrix(0, nrow = K, ncol = 1)
# Split the row indexes into k equal parts
kth_fold <- cut(seq(1, nrow(da)), breaks = 10, labels = FALSE)
# Now write the for loop for the k-fold cross validation
k = 1
for (k in 1:10) {
  # Split data into the training and test datasets
  test_index <- which(kth_fold == k)
  train <- da[-test_index, ]
  test <- da[test_index, ]
  rf <-
    lm(
      sales_price_log ~ estimate_range_log_c + surface_c + auction_weekday + auction_month +
        paper + lacquer + oil +
        wood + canvas + oil:titled + titled + auction_location
      ,
      data = train
    )
  y_test_pred <- predict(rf, test)
  testMSE <- mean((log(test$sales_price) - y_test_pred) ^ 2)
  
  RSME[k, ] <- sqrt(testMSE)
}

#Calculate the average of all values in the RSME matrix
rsme1 <- mean(RSME); rsme1

###### rf
da <- da[sample(nrow(da)), ]
K <- 10
RSME <- matrix(0, nrow = K, ncol = 1)
kth_fold <- cut(seq(1, nrow(da)), breaks = 10, labels = FALSE)
for (k in 1:10) {
  test_index <- which(kth_fold == k)
  train <- da[-test_index, ]
  test <- da[test_index, ]
  rf <-
    randomForest(sales_price_log ~ ., data = train)
  y_test_pred <- predict(rf, test)
  testMSE <- mean((log(test$sales_price) - y_test_pred) ^ 2)
  RSME[k, ] <- sqrt(testMSE)
}
rsme2 <- mean(RSME); rsme2

###### boosting
da <- da[sample(nrow(da)), ]
K <- 10
RSME <- matrix(0, nrow = K, ncol = 1)
kth_fold <- cut(seq(1, nrow(da)), breaks = 10, labels = FALSE)
for (k in 1:10) {
  test_index <- which(kth_fold == k)
  train <- da[-test_index, ]
  test <- da[test_index, ]
  rf <-gbm(
    sales_price_log ~ .,
    data = train,
    distribution = "gaussian",
    n.trees = 5000,
    interaction.depth = 2
  )
  y_test_pred <- predict(rf, test)
  testMSE <- mean((log(test$sales_price) - y_test_pred) ^ 2)
  RSME[k, ] <- sqrt(testMSE)
}
rsme2 <- mean(RSME); rsme2

######################### Reassess imputation ##################################
reg_imp <- with(data = imp, lm(
  sales_price_log ~ estimate_range_log_c + surface_c + auction_weekday + auction_month +
    paper + lacquer + oil +
    wood + canvas + oil:titled + titled + auction_location))
reg <- pool(reg_imp)
summary(reg)