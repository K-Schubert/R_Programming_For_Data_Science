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
