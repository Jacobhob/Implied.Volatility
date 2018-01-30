library("quantmod")
library("data.table")
library("stringr")

getSymbols("AAPL", src = "yahoo")
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
label    <- c(9249:9952) # Maturity: Jan 19, 2019
maturity <- as.Date("2019-01-19")
today    <- as.Date("2018-01-29")
st       <- stock.price[nrow(stock.price) -6, "AAPL.Adjusted"]
K        <- data.table("Strike" = option.price[label, "Strike"])
r        <- 0.0180
T        <- 1
# Call
call <- 0.5 * option.price[label, c(4, 16)] + 0.5 * option.price[label, c(5, 16)]
names(call) <- c("Call", "Strike")
call <- data.table(call)
call <- call[, mean(Call), by = "Strike"]
names(call) <- c("Strike", "Call")
# Put
put  <- 0.5 * option.price[label, c(11, 16)] + 0.5 * option.price[label, c(12, 16)]
names(put) <- c("Put", "Strike")
put  <- data.table(put) 
put  <- put[, mean(Put), by = "Strike"]
names(put) <- c("Strike", "Put")

# Calculate mplied volatility
BS.equation.Call <- function(sigma = sigma, 
                             K     = k,
                             Call  = call,
                             T     = T,
                             r     = r,
                             st    = st) {
  d1 <- (log(st / k) + (r + 0.5 * sigma^2) * T)/ (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  BS.equation.Call <- st * pnorm(d1) - k * exp(-r * T) * pnorm(d2) - call
}
BS.equation.Put  <- function(sigma = sigma, 
                             K     = k,
                             Put   = put,
                             T     = T,
                             r     = r,
                             st    = st) {
  d1 <- (log(st / k) + (r + 0.5 * sigma^2) * T)/ (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  BS.equation.Put <- k * exp(-r * T) * pnorm(-d2) - st * pnorm(-d1) - put 
}

