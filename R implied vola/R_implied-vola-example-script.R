
## This file contains example code on how to clean the raw options
## data from CBOE using string processing in R.

DF <- read.csv("R_implied-vola-example-data.csv", stringsAsFactors = FALSE)
( x <- DF[, "Calls"] ) # Extract column of interest for easier processing.
x <- strsplit(x, " ") # Split up by space, i.e. " ".
x <- sapply(x, function(z) z[3]) # Extract third element. Shortcut for `sapply(x,"[",3)`.
x <- as.numeric(x)     # Convert to numeric (from character).
DF[, "Strike"] <- x    # Assign back to data.frame `DF`.
DF                     # Inspect final result.

## That's it! You've extracted the strikes from the data using string
## processing in R.



## But I think the code above doesn't look very nice. That's why I'm
## providing a clearer alternative below, using the "magrittr"
## package. This package lets you pipe one operation after the other
## using the `%>%` function.
library("magrittr") # Install once with `install.packages("magrittr")`.
DF <- read.csv("R_implied-vola-example-data.csv", stringsAsFactors = FALSE)
DF[, "Strike"] <- 
    DF[, "Calls"] %>%
    strsplit(" ") %>%
    sapply("[", 3) %>%                  # Get third element.
    as.numeric
DF                                      # Inspect final result.
