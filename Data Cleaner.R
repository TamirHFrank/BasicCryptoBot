##########################################################################
# Tamir Frank
# Circa Winter of 2020
# Crypto Data Cleaning - File of helper functions to help reduce clutter
#                        and clean data accordingly
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

prediction_price <- function(asset.df, i, delay) {
  #add the prediction price once the delay and current row are factored in
  if (i > delay) {
    asset.df$prediction_price[i-delay] <- asset.df$ask_price[nrow(asset.df)]
  }
  
  return(asset.df)
}

prediction_direction <- function(asset.df, i, delay) {
  #add the prediction price once the delay and current row are factored in
  if (i > delay) {
    if (asset.df$prediction_price[i-delay] > asset.df$ask_price[i-delay]) {
      asset.df$prediction_direction[i-delay] <- 1
    } else if (asset.df$prediction_price[i-delay] < asset.df$ask_price[i-delay]) {
      asset.df$prediction_direction[i-delay] <- -1
    } else if (asset.df$prediction_price[i-delay] == asset.df$ask_price[i-delay]) {
      asset.df$prediction_direction[i-delay] <- 0
    }
  }
  
  return(asset.df)
}

price_direction <- function(asset.df) {
  #1 for increase, -1 for decrease, 0 for the same
  if (asset.df$market_price[nrow(asset.df)] > asset.df$market_price[nrow(asset.df)-1]) {
    asset.df$price_direction[nrow(asset.df)] <- 1
  } else if (asset.df$market_price[nrow(asset.df)] < asset.df$market_price[nrow(asset.df)-1]) {
    asset.df$price_direction[nrow(asset.df)] <- -1
  } else if (asset.df$market_price[nrow(asset.df)] == asset.df$market_price[nrow(asset.df)-1]) {
    asset.df$price_direction[nrow(asset.df)] <- 0
  }
  
  return(asset.df)
}

momentum <- function(asset.df) {
  #first check the price direction to see if increase, decrease, or flat in the most recent row
  #then check to see what the previous momentum is the previous row (nrow - 1) and update the most recent row 
  if (is.na(asset.df$momentum[nrow(asset.df)-1])) {
    asset.df$momentum[nrow(asset.df)] <- asset.df$price_direction[nrow(asset.df)]
    return(asset.df)
  }
  if (asset.df$price_direction[nrow(asset.df)] == 1) {
    if (asset.df$momentum[nrow(asset.df)-1] >= 0) {
      asset.df$momentum[nrow(asset.df)] <- asset.df$momentum[nrow(asset.df)-1] + 1
    } else if (asset.df$momentum[nrow(asset.df)-1] < 0) {
      asset.df$momentum[nrow(asset.df)] <- 1
    }
  } else if (asset.df$price_direction[nrow(asset.df)] == -1) {
    if (asset.df$momentum[nrow(asset.df)-1] <= 0) {
      asset.df$momentum[nrow(asset.df)] <- asset.df$momentum[nrow(asset.df)-1] - 1
    } else if (asset.df$momentum[nrow(asset.df)-1] > 0) {
      asset.df$momentum[nrow(asset.df)] <- -1
    }
  } else if (asset.df$price_direction[nrow(asset.df)] == 0) {
    asset.df$momentum[nrow(asset.df)] <- asset.df$momentum[nrow(asset.df)-1]
  }
  
  return(asset.df)
}


constant_time <- function(asset.df) {
  if (is.na(asset.df$constant_time[nrow(asset.df)-1])) {
    asset.df$constant_time[nrow(asset.df)] <- 0
    return(asset.df)
  }
  
  if (asset.df$price_direction[nrow(asset.df)] == 0) {
    asset.df$constant_time[nrow(asset.df)] <- asset.df$constant_time[nrow(asset.df)-1] + 1
  } else {
    asset.df$constant_time[nrow(asset.df)] <- 0
  }
  
  return(asset.df)
}

bid_ask_spread <- function(asset.df) {
  current.ba.spread <- asset.df$ask_price[nrow(asset.df)] - asset.df$bid_price[nrow(asset.df)]
  previous.ba.spread <- asset.df$ask_price[nrow(asset.df)-1] - asset.df$bid_price[nrow(asset.df)-1]
  
  if (current.ba.spread > previous.ba.spread) {
    asset.df$bid_ask_spread[nrow(asset.df)] <- 1
  } else if (current.ba.spread < previous.ba.spread) {
    asset.df$bid_ask_spread[nrow(asset.df)] <- -1
  } else if (current.ba.spread == previous.ba.spread) {
    asset.df$bid_ask_spread[nrow(asset.df)] <- 0
  }
    
  
  return(asset.df)
}




























