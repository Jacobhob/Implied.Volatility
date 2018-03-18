
#######################################################################
## 
## This R script is for plotting the volatility smile of Apple
## Inc. with data from 2013-04-21 taken from the CBOE website. Except
## for plotting with `ggplot2`, we do not use any add-on libraries
## here.
## 
#######################################################################



DF <- read.delim("R_implied-vola-full-data.tsv", stringsAsFactors = FALSE)
DF <-                                   # Extract relevant columns.
    DF[, c("Last.Trade.", "Interest", "Strike.Price.", "Last.Trade..1", "Interest.1")]
colnames(DF) <- c("call", "c.interest", "strike", "put", "p.interest") # Rename columns.
for (i in seq_len(ncol(DF)))
    DF[, i] <- as.numeric(DF[, i])      # Convert columns to numeric.

## Separate calls and puts into different data.frames to work on.
DF.calls <- DF[which(DF$call > 0 & DF$c.interest > 0), ]
DF.puts <- DF[which(DF$put > 0 & DF$p.interest > 0), ]


## This function removes rows that have a duplicate strike price. We
## keep the row with the largest open interest.
get.rows.del <- function(DF, interest.col) {
    stopifnot(class(interest.col) == "character")
    
    rows <- which(duplicated(DF$strike))
    rows.del <- NULL
    for (i in seq_along(rows)) {
        pos <- which(DF$strike == DF$strike[rows[i]])
        stopifnot(length(pos) > 1)
        df.hlp <- DF[pos, ]
        df.hlp <- df.hlp[order(df.hlp[, interest.col], decreasing = TRUE), ]
        rows.del <-
            c(
                rows.del,
                intersect(
                    pos,
                    which(DF[, interest.col] != df.hlp[1, interest.col])))
    }
    return(sort(unique(rows.del)))
}

rows.del.call <- get.rows.del(DF.calls, "c.interest")
rows.del.put <- get.rows.del(DF.puts, "p.interest")

## Remove rows with duplicate strike price for calls and puts.
if (length(rows.del.call) > 0)
    DF.calls <- DF.calls[-rows.del.call, c("strike", "call")]
if (length(rows.del.put) > 0)
    DF.puts <- DF.puts[-rows.del.put, c("strike", "put")]







## Black-Scholes formula.
BS <- function(S, K, T, r, sig, type = "C") {
    if (T == 0 & type == "C") return(max(0, S - K))
    if (T == 0 & type == "P") return(max(0, K - S))
    
    d1 <- (log(S / K) + (r + sig^2 / 2) * T) / (sig * sqrt(T))
    d2 <- d1 - sig * sqrt(T)
    value <- S * pnorm(d1) - exp(-r * T) * K * pnorm(d2)
    if (type == "P") return(exp(-r * T) * K + value - S)
    return(value)
}


## Current stock price of Apple on 2013-04-21.
S <- 390.53

## ## Retain only in-the-money options.
## DF.calls <- DF.calls[which(DF.calls$strike < S), ]
## DF.puts <- DF.puts[which(DF.puts$strike >= S), ]

## Retain only out-of-the-money options.
DF.calls <- DF.calls[which(DF.calls$strike > S), ]
DF.puts <- DF.puts[which(DF.puts$strike <= S), ]

T <- 3/12
r <- 0.002

## Function to minimize for extracting implied volatility.
f <- function(sig, option.price, S, K, T, r, type) {
    (option.price - BS(S, K, T, r, sig, type))^2
}



## Function to extract the implied volatility from a data frame for a
## given strike price.
get.IV <- function(DF, K, type = "C") {
    stopifnot(type %in% c("C", "P"))
    pos <- which(DF$strike == K)
    stopifnot(length(pos) == 1)
    option.price <- if (type == "C") DF[pos, "call"] else DF[pos, "put"]
    return(optimize(f = f, interval = c(0, 1), option.price, S, K, T, r, type)$minimum)
}



## Bring both calls and puts together and calculate the implied
## volatility.
DF.IV <-
    data.frame(
        StrikePrice = c(DF.puts$strike, DF.calls$strike),
        ImpliedVolatility = NA_real_)
stopifnot(length(intersect(DF.calls$strike, DF.puts$strike)) == 0)
for (i in seq_len(nrow(DF.IV))) {
    K <- DF.IV$StrikePrice[i]
    if (K %in% DF.calls$strike)
        DF.IV[i, "ImpliedVolatility"] <- get.IV(DF.calls, K, type = "C")
    if (K %in% DF.puts$strike)
        DF.IV[i, "ImpliedVolatility"] <- get.IV(DF.puts, K, type = "P")
}

## New way of plotting.
library("ggplot2")
qplot(
    StrikePrice,
    ImpliedVolatility * 100,            # Plot it in percent.
    data = DF.IV,
    xlab = "K",
    ylab = "Implied Volatility (%)",
    main = "Volatility Smile")

## Old way of plotting.
plot(DF.IV, xlab = "K", ylab = "Implied Volatility", main = "Volatility Smile")

## ## In case you want to save the plot to a PDF file.
## dev.copy2pdf(file = "AAPL_VolaSmile.pdf", out.type = "pdf")
## dev.off()
