##########################################################################
# Tamir Frank
# Circa Winter of 2020
# Crypto Prediction Model - helper functions full of regressions and random forests
#                           to predict the T+1 price change of the asset
##########################################################################

##########################################################################
# set the working directory
setwd("~/Documents/")

# Load required packages
library('RobinHood')
library(quantmod)
library(dplyr)
library(infotheo)
library(caret)
library(MASS)
library(class)
library(randomForest)
library(leaps)
library(glmnet)
library(ROCR)

create_data_sheet <- function() {
  d <- data.frame(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), nrow = 1))
  colnames(d) <- c("time", "symbol", "ask_price", "bid_price", 
                   "market_price", "high_price", "low_price", 
                   "open_price", "volume", "price_direction", "momentum",
                   "constant_time", "bid_ask_spread", 
                   "prediction_price", "prediction_direction")
  write.csv(d, file = "data.csv", row.names = FALSE)
  print("Prediction Sheet Created...")
}

initialize_data_sheet <- function(asset.df.master) {
  data.sheet <- read.csv("data.csv")
  asset.df.master <- rbind(data.sheet, asset.df.master)
  asset.df.master <- asset.df.master[-c(1),]
  write.csv(asset.df.master, file = "data.csv", row.names = FALSE)
  print("Data Sheet Initialized")
}

update_data_sheet <- function(asset.df.master, i, delay) {
  #Read in the data.csv file, take the last row of the asset.df.master and add it to the full datasheet info
  data.sheet <- read.csv("data.csv")
  asset.df.master <- rbind(data.sheet, asset.df.master[nrow(asset.df.master),])
  
  #source the helper file Data Cleaning.R
  source('Data Cleaning - HEZ.R')
  
  #Calculate the prediction price column (if applicable)
  asset.df.master <- prediction_price(asset.df.master, i, delay)
  
  #Calculate the prediction direction column (if applicable)
  asset.df.master <- prediction_direction(asset.df.master, i, delay)
  
  #Calculate the previous change column
  asset.df.master <- price_direction(asset.df.master)
  
  #Calculate the momentum column
  asset.df.master <- momentum(asset.df.master)
  
  #Calculate the constant time column
  asset.df.master <- constant_time(asset.df.master)
  
  #Calculate the bid ask spread column
  asset.df.master <- bid_ask_spread(asset.df.master)
  
  #write to the data.csv file when complete
  write.csv(asset.df.master, file = "data.csv", row.names = FALSE)
}

update_prediction_model <- function(asset.df, num.points, prediction.list) {
  pred.sheet <- read.csv("data.csv")
  #create the model training sheet
  pred.sheet <- na.omit(pred.sheet)
  pred.sheet <- pred.sheet[c((nrow(pred.sheet)-num.points):nrow(pred.sheet)),]
  
  #set the current bid and ask prices
  curr.bid.price <- asset.df$bid_price[nrow(asset.df)]
  curr.ask.price <- asset.df$ask_price[nrow(asset.df)]
  
  #create the vector of the inputs we will use to predict the next price
  pred.values <- c()
  for (i in 10:13) {
    pred.values[i-9] <- pred.sheet[nrow(pred.sheet),i]
  }
  
  #Fit the model
  #linear regression
  model <- lm(pred.sheet$prediction_price ~ pred.sheet$price_direction + 
                pred.sheet$momentum + pred.sheet$constant_time + pred.sheet$bid_ask_spread, 
              data = pred.sheet)
  
  #random forest
  #fit <-randomForest(pred.sheet$prediction_price ~ pred.sheet$price_direction + 
  #                   pred.sheet$momentum + pred.sheet$constant_time + pred.sheet$bid_ask_spread, 
  #                   family = "binomial", ntree = 100, mtry=5, nmin = 5)
  
  #print(fit)
  
  #match the coef. to create a prediction
  pred <- model$coefficients[[1]]
  for (i in 1:length(pred.values)) {
    pred <- pred + model$coefficients[[i+1]]*pred.values[i]
  }
  
  if (curr.bid.price > pred) {
    accuracy.count <- accuracy.count + 1
  } else {
    accuracy.count <- 0
  }
  
  prediction.list <- c(prediction.list, pred)
  return(prediction.list)
}







































































