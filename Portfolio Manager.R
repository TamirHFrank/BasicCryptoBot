##########################################################################
# Tamir Frank
# Circa Winter of 2020
# Crypto Trading Bot - Main file, scroll all the way down to get to the main function
##########################################################################

# MUST SOURCE THIS ONE FIRST AND SET THE PROPER WORKING DIRECTORY #
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


create_db <- function(RH, ticker, num.data.points, sleep) {
  #sets up the dataframe names and dimensions for asset1
  d <- data.frame(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), nrow = 1))
  colnames(d) <- c("time", "symbol", "ask_price", "bid_price", 
                   "market_price", "high_price", "low_price", 
                   "open_price", "volume", "price_direction", "momentum",
                   "constant_time", "bid_ask_spread",
                   "prediction_price", "prediction_direction")
  
  #auto populates these concurrently to allow for most accurate data collection without time lapse
  cutoff <- num.data.points/10
  j <- 1
  for (i in 1:num.data.points) {
    #Get the current time and quote
    obs.time <- Sys.time()
    asset.quote <- get_quote_crypto(RH, symbol = ticker)
    
    new.row <- nrow(d) + 1
    d[new.row,1] <- obs.time
    
    for (j in 1:8) {
      d[new.row,j+1] <- asset.quote[[j]]
    }
    
    if (i %% cutoff == 0) {
      cat(11 - i %/% cutoff, ' ')
    }
    
    Sys.sleep(sleep)
  }
  
  #clip the meaningless first row
  asset.df.master <- d[-c(1),]
  print("Database Created...")
  return("asset.df.master" = asset.df.master)
}

portfolio.update <- function(RH, order.info, portfolio) {
  #get the current order state (filled or unconfirmed)
  order.state <- get_order_status_crypto(RH, order.info$id)$state
  if (length(order.state) == 0) {
    return(list("status" = portfolio$status, "price" = portfolio$price, "quantity" = portfolio$quantity, "time" = portfolio$time))
  }
  
  #check to see if order state is filled and order went through - then update portfolio
  if (order.state == "filled") {
    if (portfolio$status == "bidding") {
      portfolio$price <- as.numeric(as.character(get_order_status_crypto(RH, order.info$id)$executions$effective_price))
      portfolio$quantity <- as.numeric(as.character(get_order_status_crypto(RH, order.info$id)$executions$quantity))
      portfolio$time <- get_order_status_crypto(RH, order.info$id)$executions$timestamp
      portfolio$status <- "full"
      print("Bought At:")
      print(portfolio$price)
    } else if (portfolio$status == "offering") {
      portfolio$price <- -1 * get_order_status_crypto(RH, order.info$id)$executions$effective_price
      portfolio$quantity <- get_order_status_crypto(RH, order.info$id)$executions$quantity
      portfolio$time <- get_order_status_crypto(RH, order.info$id)$executions$timestamp
      portfolio$status <- "none"
      print("Sold At:")
      print(portfolio$price)
    }
  }
  
  portfolio$status <- "testing"
  return(list("status" = portfolio$status, "price" = portfolio$price, "quantity" = portfolio$quantity, "time" = portfolio$time))
  
}

#This function calls the prediction piece, checks holdings, and tries to place buy/sell orders accordingly
decision <- function(RH, decision.info, order.info, asset.df.master, ticker, portfolio, prediction.list, investment.amount) {
  #setup
  action <- ""  #reset action from the last time
  price <- 1    #put the price to $1 every time for safety purposes to keep code in control
  cancel.url <- "null" #no order placed, so cancel.url should be null
  
  #breakdown the prediction.list to get the current prediction
  prediction <- prediction.list[length(prediction.list)]
  
  #set the current bid and ask price
  curr.bid.price <- asset.df.master$bid_price[nrow(asset.df.master)]
  curr.ask.price <- asset.df.master$ask_price[nrow(asset.df.master)]
  
  #update the portfolio components based on the 'filled' status of an order

  if (!is.null(order.info$state)) {
    portfolio <- portfolio.update(RH, order.info, portfolio)
  } else {
    #print("not running portfolio update")
  }
  
  #regardless of the above, decide what action to do and whether or not we should do the action
  #this pulls in the model prediction from the update_prediction_model in the Prediction Model - HEZ.R file
  #it compares the prediction (of the future bid price) to the current ask price and if there is overlap then action changes
  #when action changes, we set the price to buy in the order function equal to current bid price
  
  if (prediction != -1000) {
    #double check before trying to buy
    if (portfolio$status == "none" & prediction > curr.bid.price) {
      print("TRY TO BUY") #Let user know the bot is trying to place an order
      action <- "buy"
      price <- curr.ask.price  #buy at the market ask
      cancel.url <- order.info$cancel_url
    } 
    #double check before trying to sell for profit
    else if (portfolio$status == "full" & curr.bid.price > portfolio$price) {
      print("TRY TO SELL")
      action <- "sell"
      price <- curr.bid.price  #sell at the market bid
      cancel.url <- order.info$cancel_url
    } 
    #double check before trying to sell for controlled loss size (5% loss)
    else if (portfolio$status == "full" & curr.ask.price < portfolio$price * .95) {
      print("SELL - DROP IN VALUE")
      action <- "sell"
      price <- curr.bid.price
      cancel.url <- order.info$cancel_url
    }
    
    if (portfolio$status == "bidding" & curr.bid.price > prediction) {
      action <- "cancel"
      price <- curr.bid.price - 20
      cancel.url <- order.info$cancel_url
    } else if (portfolio$status == "offering" & curr.bid.price < portfolio$price) {
      action <- "cancel"
      price <- curr.bid.price - 20
      cancel.url <- order.info$cancel_url
    } 
  }
  
  #determine the order quantity based on buying or selling and how much user is investing
  if (decision.info$action == "buy") {
    investment.amount <- investment.amount/decision.info$price
  } else if (decision.info$action == "sell") {
    investment.amount <- portfolio$quantity
  }
  
  #perform the decision action from the function above (buy, sell, or cancel)
  if (decision.info$action == "cancel") {
    cancel_order_crypto(RH, decision.info$cancel_url)
    if (portfolio$status == "bidding") {
      portfolio$status <- "buy"
    } else if (portfolio$status == "offering") {
      portfolio$status <- "sell"
    }
  } else if (decision.info$action == "buy" | decision.info$action == "sell") {
    order.info <- place_order_crypto(RH = RH, symbol = ticker, type = "market", 
                                     time_in_force = "gtc", price = round(decision.info$price,2), 
                                     quantity = round(investment.amount,6), side = decision.info$action)
    
    #update the portfolio$status so we are good to go moving forward
    if (decision.info$action == "buy") {
      portfolio$status <- "bidding"
    } else if (decision.info$action == "sell") {
      portfolio$status <- "offering"
    }
  } else if (decision.info$action == "") { #this is to handle when the program is starting but no predictions have been made
    portfolio$status <- "none"
  }
  
  #end of this long function! Whew, just return the necessary pieces for the rest of the program
  return(list("action" = action, "price" = price, "cancel_url" = cancel.url, "portfolio" = portfolio, "order_info" = order.info))
}

order <- function(RH, decision.info, order.info, asset.df.master, ticker, investment.amount, portfolio) {
  print(decision.info$action)
  
  #perform the decision action from the function above (buy, sell, or cancel)
  if (decision.info$action == "cancel") {
    cancel_order_crypto(RH, decision.info$cancel_url)
    if (portfolio$status == "bidding") {
      portfolio$status <- "buy"
    } else if (portfolio$status == "offering") {
      portfolio$status <- "sell"
    }
  } else if (decision.info$action == "buy" | decision.info$action == "sell") {
    order.info <- place_order_crypto(RH = RH, symbol = ticker, type = "market", 
                                     time_in_force = "gtc", price = round(decision.info$price,2), 
                                     quantity = round(investment.amount,6), side = decision.info$action)

    if (decision.info$action == "buy") {
      portfolio$status <- "bidding"
    } else if (decision.info$action == "sell") {
      portfolio$status <- "offering"
    }
  } else if (decision.info$action == "") { #this is to handle when the program is starting but no predictions have been made
    portfolio$status <- "none"
  }
  
  return(list("order.info" = order.info, "portfolio" = portfolio))
}

#update the master dataframe to get the next time data point of our asset
refresh <- function(RH, asset.df.master, ticker) {
  #Get the time and quote
  obs.time <- Sys.time()
  asset.quote <- get_quote_crypto(RH, symbol = ticker)
  
  new.row <- nrow(asset.df.master) + 1
  asset.df.master[new.row,1] <- obs.time
  for (j in 1:8) {
    asset.df.master[new.row,j+1] <- asset.quote[[j]]
  }
  
  #clip the first row to maintain the same amount of data points
  asset.df.master <- asset.df.master[-c(1),]
  return("asset.df.master" = asset.df.master)
}

#Contol center where we define key variables and call all major functions
main <- function(ticker) {
  #Login
  RH <- RobinHood(username = "your.email@gmail.com", password = "password123")
  num.points <- 150             #how many data points will we collect before the program starts
  investment.amount <- 2       #how much are we investing
  portfolio.status <- "none"   #set the portfolio to empty as we start the program
  delay <- 150                  #how long to wait before predicting based off recent historical data
  sleep <- .1                  #time between each data point collection and order placement
  prediction <- -1000          #set our initial prediction as an integer - this will change later
  accuracy.count <- 0          #count for how accurate our "up" vs "down" predictions are
  prediction.list <- c(prediction)  #create list of predictions so we can reference after the program ends

  
  #Other lists for order keeping
  decision.info <- list("action" = "buy", "price" = 10, "cancel_url" = "")
  order.info <- list("state" = "beginning", "cancel_url" = "beginning")
  portfolio <- list("status" = "none", "price" = 0, "quantity" = 0, time = "null")
  
  #Setup the dataframes by calling the necessary functions
  print("Setting Up...")
  source("Prediction Model - HEZ.R")
  asset.df.master <- create_db(RH, ticker, num.points, sleep)
  create_data_sheet()
  initialize_data_sheet(asset.df.master)
  
  cutoff <- 500/100 #visual for the user to see that progress is being made in calculations
  for (i in 1:500) {  #this is how many data points will be collected before the program ends
    #make the decision, execute the decision / order, and refresh the df
    if (i > 2) {
      decision.info <- decision(RH, decision.info, order.info, asset.df.master, 
                                ticker, portfolio, prediction.list, investment.amount)
      portfolio <- decision.info$portfolio
      order.info <- decision.info$order_info
    }
    if (i == 2) {
      portfolio$status <- "none"
    }
    
    #plot to see a visual of the moving prices
    plot(c(1:num.points), asset.df.master$ask_price, type = "l", cex = .05, col = "green", xlab = "Time", ylab = "Ask Price")
    lines(c(1:num.points), asset.df.master$market_price, col = "gray")
    lines(c(1:num.points), rep(mean(asset.df.master$market_price), num.points), col = "red")
    
    #update the dataframe in this sheet and then the datasheet
    asset.df.master <- refresh(RH, asset.df.master, ticker)
    update_data_sheet(asset.df.master, i+num.points, delay)

    if (i > delay + num.points) {
      if (i == (delay + num.points + 1)) {
        print("Beginning prediction...")
      }
      prediction.list <- update_prediction_model(asset.df.master, num.points, prediction.list)
      lines(c(1:num.points), rep(prediction.list[length(prediction.list)],num.points), col="blue")
    }

    Sys.sleep(sleep)
    
    if (i %% cutoff == 0) {
      cat(101 - i %/% cutoff, ' ')
    }
  }
  View(asset.df.master)
  return(list("portfolio" = portfolio, "cancel_url" = order.info$cancel_url, "predictions" = prediction.list))
}

output <- main("ETH")

RH <- RobinHood(username = "frankyth133@gmail.com", password = "Tam0313ir!")
cancel_order_crypto(RH, output$cancel_url)
View(output$predictions)
