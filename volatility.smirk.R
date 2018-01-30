library("quantmod")
library("stringr")
library("data.table")

#getSymbols("AAPL", src = "yahoo")
#write.csv(AAPL, file = "stock_price_Jan29.csv")

# Read data from csv files.
stock.price  <- read.csv("stock_price_Jan29.csv")
option.price <- read.csv("option_chain_Jan29.csv", skip = 2)

# Extract strike prices from dat frame.
strike.price <- option.price[, "Calls"]
strike.price <- str_split(strike.price, " ")
strike.price <- sapply(strike.price, function(x) x[3])
strike.price <- as.numeric(strike.price)
strike.price -> option.price[, "Strike"]
rm(strike.price)

# Initialize parameters. Calculate option prices.
label <- c(9249:9952) # Maturity: Jan 19, 2019
st    <- stock.price[nrow(stock.price), "AAPL.Adjusted"]
K     <- data.table("Strike" = option.price[label, "Strike"])

call  <- 0.5 * option.price[label, c(4, 16)] + 0.5 * option.price[label, c(5, 16)]
names(call) <- c("Call", "Strike")
call  <- data.table(call)
call  <- call[, mean(Call), by = "Strike"]
names(call) <- c("Strike", "Call")

put   <- 0.5 * option.price[label, c(11, 16)] + 0.5 * option.price[label, c(12, 16)]
names(put) <- c("Put", "Strike")
put   <- data.table(put) 
put   <- put[, mean(Put), by = "Strike"]
names(put) <- c("Strike", "Put")
