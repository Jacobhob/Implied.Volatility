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

# Calculate implied volatility
BS.equation.Call <- function(sigma, 
                             K,
                             Call) {
  d1 <- (log(st / K) + (r + 0.5 * sigma^2) * T)/ (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  BS.equation.Call <- st * pnorm(d1) - K * exp(-r * T) * pnorm(d2) - Call
}
BS.equation.Put  <- function(sigma, 
                             K,
                             Put) {
  d1 <- (log(st / K) + (r + 0.5 * sigma^2) * T)/ (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  BS.equation.Put <- K * exp(-r * T) * pnorm(-d2) - st * pnorm(-d1) - Put 
}

implied.volatility <- data.table()
for (i in c(1:10)){#nrow(call)) {
  implied.volatility[1, i] <- 
    optimise(BS.equation.Call, c(0, 1), K = as.numeric(call[i, "Strike"]), Call = as.numeric(call[i, "Call"]))
}

