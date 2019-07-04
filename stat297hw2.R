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

X <- 10^6
w1 <- seq(0, 1, 0.001)
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

symbols <- c("AAPL", "MSFT")
getSymbols(symbols, from = "2015-01-01", to = Sys.Date(), src =  "yahoo", adjust =  TRUE)

AAPL.ret <- dailyReturn(na.omit(AAPL$AAPL.Close))
MSFT.ret <- dailyReturn(na.omit(MSFT$MSFT.Close))
rets <- cbind(AAPL.ret$daily.returns, MSFT.ret$daily.returns)

mean.ret <- c(mean(AAPL.ret), mean(MSFT.ret))
cov.matrix <- cov(rets)

# All possible portfolios (fees < 100$)
mu_investment <- (w1[ind>0]*mean.ret[1] + (1 - w1[ind>0])*mean.ret[2])*X
var_investment <- (w1[ind>0]^2*cov.matrix[1,1] + (1 - w1[ind>0])^2*cov.matrix[2,2] + 
  2*w1[ind>0]*(1 - w1[ind>0])*cov.matrix[1,2])*X^2

plot(var_investment, mu_investment)
points(min(var_investment), mu_investment[which.min(var_investment)], col='red', pch=16)

# All portfolios
mu_investment <- w1*mean.ret[1] + (1 - w1)*mean.ret[2]
var_investment <- w1^2*cov.matrix[1,1] + (1 - w1)^2*cov.matrix[2,2] + 
  2*w1*(1 - w1)*cov.matrix[1,2]

plot(var_investment, mu_investment)
points(min(var_investment), mu_investment[which.min(var_investment)], col='red', pch=16)

