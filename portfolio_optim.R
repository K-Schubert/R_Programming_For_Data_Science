install.packages('quantmod')
library(quantmod)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)

symbols <- c("AAPL", "GOOG")

getSymbols(symbols, from = "2018-01-01", to = Sys.Date(), src =  "yahoo", adjust =  TRUE)

AAPL.ret <- dailyReturn(na.omit(AAPL$AAPL.Close))
GOOG.ret <- dailyReturn(na.omit(GOOG$GOOG.Close))
rets <- cbind(AAPL.ret$daily.returns, GOOG.ret$daily.returns)

mean.ret <- c(mean(AAPL.ret), mean(GOOG.ret))
cov.matrix <- cov(rets)

omega.star <- (cov.matrix[2,2] - cov.matrix[2,1])/(cov.matrix[1,1] + cov.matrix[2,2]
                                                   - 2*cov.matrix[2,1])

omega.star

# Invest 41% in AAPL and 59% in GOOG

mean_investment <- omega.star*mean.ret[1] + (1 - omega.star)*mean.ret[2]

var_investment <- omega.star^2*cov.matrix[1,1] + (1 - omega.star)^2*cov.matrix[2,2] + 
  2*omega.star*(1 - omega.star)*cov.matrix[1,2]

investment_summary <- matrix(NA, 2, 3)
dimnames(investment_summary)[[1]] <- c("Expected value", "Variance")
dimnames(investment_summary)[[2]] <- c("Apple", "Google", "Investment")
investment_summary[1, ] <- c(mean.ret, mean_investment)
investment_summary[2, ] <- c(diag(cov.matrix), var_investment)
knitr::kable(investment_summary)

plot(sqrt(investment_summary[2,]), investment_summary[1,], col=c(1,2,3), pch=16, 
     xlab='St Dev of Returns', ylab='Mean Return')
legend('topleft', c('AAPL', 'GOOG', 'Investment'), cex=0.75, pch=16, col=c(1,2,3))
grid()

