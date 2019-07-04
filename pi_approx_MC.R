library(plotrix)
library(magrittr)
library(ggplot2)
library(ggforce)

find_pi = function(B = 5000, seed = 10, make_plot = TRUE){
  # Control seed
  set.seed(seed)
  
  # Simulate B points
  matrix(runif(2*B, -1, 1), B, 2) %>%
    as.data.frame -> point
  colnames(point) <- c('x', 'y')
  d <- sqrt(point[, 1]^2 + point[, 2]^2)
  hat_pi <- 4*sum(d <= 1)/B
  
  # Circle
  circle <- data.frame(x0=0, y0=0, r=1)
  
  if (make_plot){

    point %>%
      ggplot() +
      geom_point(aes(x=x, y=y, col='turquoise'), alpha=0.3) +
      geom_circle(aes(x0=x0, y0=y0, r=r, col='red'), data=circle)
    
    
    #par(pty="s")
    #plot(point, ylim=c(-1.2, 1.2), xlim=c(-1.2, 1.2), xlab='x', ylab='y')
    #draw.circle(0, 0, 1, border='red', col=NA)
  }
  
  #hat_pi

}

find_pi(B=50, make_plot=TRUE)
