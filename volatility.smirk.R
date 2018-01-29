library("quantmod")
library("stringr")

#getSymbols("AAPL", src = "yahoo")
#write.csv(AAPL, file = "stock_price_Jan29.csv")

# Read data from csv files.
stock.price  <- read.csv("stock_price_Jan29.csv")
option.price <- read.csv("option_chain_Jan29.csv", skip = 2)

# Extract strike prices from dat frame.
strike.price <- option.price[, "Calls"]
strike.price <- str_split(strike.price, " ")
strike.price <- sapply(strike.price, function(x) x[3])
K <- as.numeric(strike.price)
K -> option.price[, "Strike"]

#
