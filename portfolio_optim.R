install.packages('quantmod')
library(quantmod)
library(magrittr)
library(tidyr)
library(dplyr)

symbols <- c("AAPL", "GOOG")

getSymbols(symbols, from = "2018-01-01", to = Sys.Date(), src =  "yahoo", adjust =  TRUE)

AAPL.ret <- dailyReturn(AAPL$AAPL.Close)
GOOG.ret <- dailyReturn(GOOG$GOOG.Close)
rets <- cbind(AAPL.ret$daily.returns, GOOG.ret$daily.returns)

mean.ret <- c(mean(AAPL.ret), mean(GOOG.ret))
cov.matrix <- cov(rets)

omega.star <- (cov.matrix[2,2] - cov.matrix[2,1])/(cov.matrix[1,1] + cov.matrix[2,2]
                                                   - 2*cov.matrix[2,1])

# Invest 41% in AAPL and 59% in GOOG
