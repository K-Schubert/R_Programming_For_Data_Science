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
library(plot3D)

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
X %>%
  tail

X %>%
  as.matrix -> X

lines3D(X[,1], X[,2], X[,3])

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

X <- 10^6
w1 <- seq(0, 1, 0.1)
w2 <- 1-w1
ind <- rep(0, length(w1))
C <- rep(0, length(w1))

# Execution fees constraint
for (i in 1:length(w1)){
  
  if(apply(rbind(40, 0.0001*c(w1[i], w2[i])*X), 2, max) %>% sum <= 100){
    ind[i] <- i
    C[i] <- apply(rbind(40, 0.0001*c(w1[i], w2[i])*X), 2, max) %>% sum
  } else{
    ind[i] <- 0
    C[i] <- apply(rbind(40, 0.0001*c(w1[i], w2[i])*X), 2, max) %>% sum
  }
}

ind

sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

sp500 %>% 
  html_nodes(".text") %>% 
  html_text() -> ticker_sp500

SP500_symbol <- ticker_sp500[(1:499)*2+1]
SP500_symbol[SP500_symbol == "BRK.B"] <- "BRK-B"
SP500_symbol[SP500_symbol == "BF.B"] <- "BF-B"

#symbols <- c("AAPL", "MSFT", "NFLX", "CS", "UBS")
symbols <- SP500_symbol[1:100]

Stocks = lapply(symbols, function(sym) {
  dailyReturn(na.omit(getSymbols(sym, from="2015-01-01", to=Sys.Date(), auto.assign=FALSE)))
})

rets <- do.call(merge, Stocks)
mean.ret <- apply(rets, 2, mean)
cov.matrix <- cov(rets)

# All possible portfolios of 2 stocks (fees < 100$)
comb <- combn(colnames(cov.matrix), 2)

i <- 1
mu_investment <- (w1[ind>0]*mean.ret[comb[i]] + (1 - w1[ind>0])*mean.ret[comb[i+1]])*X
var_investment <- (w1[ind>0]^2*cov.matrix[comb[i],comb[i]] + (1 - w1[ind>0])^2*cov.matrix[comb[i+1],comb[i+1]] + 
                     2*w1[ind>0]*(1 - w1[ind>0])*cov.matrix[comb[i],comb[i+1]])*X^2

for (i in seq(3, dim(comb)[2], 2)){
  mu <- (w1[ind>0]*mean.ret[comb[i]] + (1 - w1[ind>0])*mean.ret[comb[i+1]])*X
  var <- (w1[ind>0]^2*cov.matrix[comb[i],comb[i]] + (1 - w1[ind>0])^2*cov.matrix[comb[i+1],comb[i+1]] + 
  2*w1[ind>0]*(1 - w1[ind>0])*cov.matrix[comb[i],comb[i+1]])*X^2
  
  mu_investment <- c(mu_investment, mu)
  var_investment <- c(var_investment, var)
}

cbind(mu = mu_investment, var = var_investment) %>%
  as.data.frame -> to_plot

to_plot %>%
  ggplot() + 
  geom_point(aes(x=var, y=mu, colour='possible portfolios'), alpha=0.3) + 
  geom_point(aes(x=min(var), y=mu[which.min(var)], colour='min-var portfolio')) +
  scale_colour_manual(values=c('possible portfolios' = 'turquoise', 'min-var portfolio' = 'red')) +
  ggtitle('min-var portfolio')
  
