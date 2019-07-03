# EXERCISE 1

for (i in 1:30){
  if ((i %% 3 == 0) && (i %% 5 == 0)){
    print('FizzBuzz')
  } else if (i %% 3 == 0){
    print('Fizz')
  } else if (i %% 5 == 0){
    print('Buzz')
  } else {
    print(i)
  }
  }
}

# EXERCISE 2

library(maps)
library(ggmap)
library(rvest)
library(magrittr)

# Define webpage
big10 = read_html("http://www.bigten.org/library/stats/fb-confsked.html#standings")

# Get uni names
big10 %>% 
  html_nodes(".b1gfbstats:nth-child(9) td:nth-child(1) , .b1gfbstats:nth-child(6) td:nth-child(1)") %>% 
  html_text() -> uni_name
uni_name = paste(uni_name,"University")

# Find uni locations
uni_coord = data.frame(geocode(uni_name))

# Get win rate
big10 %>% 
  html_nodes("td:nth-child(7)") %>%
  html_text() -> uni_wp
uni_coord$wp = 100*as.numeric(uni_wp[1:length(uni_name)])

# Get division
uni_coord$conf = rep(c("East Division","West Division"), each = length(uni_name)/2)

# EXERCISE 3
# a)

f <- function(u, a){
  if (u <= a[1]){
    X <- c(1, 0, 0)
  } else if (a[1] < u && u <= a[2]){
    X <- c(-1, 0, 0)
  } else if (a[2] < u && u <= a[3]){
    X <- c(0 ,1 ,0)
  } else if (a[3] < u && u <= a[4]){
    X <- c(0 ,-1 ,0)
  } else if (a[4] < u && u <= a[5]){
    X <- c(0 ,0 ,1)
  } else {
    X <- c(0 ,0 ,-1)
  }
  return(X)
}
  
B <- 10^4
a <- rep(0, 6)
for (i in 1:6){
  a[i] <- i/6
}

set.seed(1982)
Ut <- runif(B)
head(Ut)

X <- matrix(0, nrow=B+1, ncol=3)

for (t in 1:B){
  X[t+1, ] <- X[t, ] + f(Ut[t], a)
}
X %>% tail

#b)
B <- 10^5
for (i in 1:6){
  a[i] <- 0.99*(i/6)
}

set.seed(2000)
Ut <- runif(B)
head(Ut)

X <- matrix(0, nrow=B+1, ncol=3)

for (t in 1:B){
  X[t+1, ] <- X[t, ] + f(Ut[t], a)
}
X %>% tail

# EXERCISE 3
# Invest exactly 10^6 $
# Invest only in S&P500 stocks
# Do not spend more than 100$ on execution fees

library(quantmod)
library(rvest)
sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

sp500 %>% 
  html_nodes(".text") %>% 
  html_text() -> ticker_sp500

SP500_symbol <- ticker_sp500[(1:499)*2+1]
SP500_symbol[SP500_symbol == "BRK.B"] <- "BRK-B"
SP500_symbol[SP500_symbol == "BF.B"] <- "BF-B"

today <- Sys.Date()
three_year_ago <- seq(today, length = 2, by = "-3 year")[2]
stocks_tickers <- c("AAPL", "MSFT")
getSymbols(stocks_tickers, from = three_year_ago, to = today)

nb_ticker <- length(stocks_tickers)
var_stocks <- rep(NA, nb_ticker)
names(var_stocks) <- stocks_tickers

for (i in 1:nb_ticker){
  Xt = na.omit(ClCl(get(stocks_tickers[i])))
  stocks_tickers[i] = var(Xt)
}

stocks_tickers

# Execution fees
X <- 10^6
w <- c(0.1, 0.9)

if(apply(rbind(40, 0.0001*w*X), 2, max) %>% sum <= 100){
  C <- apply(rbind(40, 0.0001*w*X), 2, max) %>% sum
} else{
  
}


