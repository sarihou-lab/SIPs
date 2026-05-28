#This is the Monte Carlo simulation of the Plan P2 which takes as input
#the number of simulation, the initial index, the estimated growth rate and 
#estimated volatility and output the approximated Mean Percentage of Return 
#(MPR) of P2.
P2 <- function(N, x0, mu, sigma){
  
  drift <- (mu - 0.5 * sigma^2) # Drift of GBM
  
  #Generate N random normal variables for each path
  Z1 <- rnorm(N) 
  Z2 <- rnorm(N)
  Z3 <- rnorm(N)
  Z4 <- rnorm(N)
  
  #Generate each path
  x4 <- x0 * exp(4 * drift + sigma * sqrt(4) * Z1)
  x5 <- x4 * exp(drift + sigma * Z2)
  x6 <- x5 * exp(drift + sigma * Z3)
  x7 <- x6 * exp(drift + sigma * Z4)
  
  #vector of N elements
  payoff <- numeric(N)
  
  #Year 4 trigger
  payoff[x4 >= x0] <- 4 * 0.056
  
  #Year 5 trigger
  idx2 <- (x4 < x0) & (x5 >= x0)
  payoff[idx2] <- 5 * 0.056
  
  #Year 6 trigger
  idx3 <- (x4 < x0) & (x5 < x0) & (x6 >= 0.95 * x0)
  payoff[idx3] <- 6 * 0.056
  
  #Year 7 trigger
  idx4 <- (x4 < x0) & (x5 < x0) & (x6 < 0.95 * x0) & (x7 >= 0.9 * x0)
  payoff[idx4] <- 7 * 0.056
  
  #averages all N simulated payouts to get the MPR
  mean(payoff)
}
N= 10^6
x0 <- 10379.10
mu <- 0.2233
sigma <- 0.1010

P2(N, x0, mu, sigma)

