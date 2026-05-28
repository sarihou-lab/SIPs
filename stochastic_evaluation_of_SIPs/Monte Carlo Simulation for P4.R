#This function take as input the initial index x_0, the estimated growth rate m
#the estimated sigma s, the quarterly return of the gilt 4, the number of 
#simulation and return the approximated MPR for plan P4.

P4 <- function(x0, m, s, rg,N) {
  h          <- 0.25            # one quarter in years (= 1/4)
  b          <- m - 0.5 * s^2  # GBM drift = mu - 0.5*sigma^2
  
  # Thresholds (all scaled so x0 = 1)
  alpha <- 0.85    # income barrier:  Index >= 85% of start
  beta  <- 1.05    # kick-out barrier: Index >= 105% of start
  gamma <- 0.65    # capital barrier:  Index < 65% => capital at risk
  
  iota  <- 0.016   # 1.6% quarterly income rate
  
  # Kick-out check quarters: end of Years 2,3,4,5,6,7
  ko_quarters  <- c(8, 12, 16, 20, 24, 28)
  max_quarters <- 32            # Year 8 = final maturity
  
  # create vectors of N elements
  mpi_vec  <- numeric(N)
  mpl_vec  <- numeric(N)
  
  #Monte Carlo loop
  for (i in 1:N) {
    Z <- rnorm(max_quarters)
    x    <- numeric(max_quarters)
    x[1] <- x0 * exp(b * h + s * sqrt(h) * Z[1])
    for (q in 2:max_quarters)
      x[q] <- x[q-1] * exp(b * h + s * sqrt(h) * Z[q])
    phi <- as.numeric(x >= alpha)  # vector of length 32
    ko_q <- NA  # NA = no kick-out (plan runs to maturity)
    for (kq in ko_quarters) {
      if (x[kq] >= beta) {
        ko_q <- kq   # first annual check where FTSE >= 1.05
        break        # stop at first kick-out
      }
    }
    
    # Termination quarter T 
    T_end <- ifelse(is.na(ko_q), max_quarters, ko_q)
    compound_weights <- (1 + rg)^((T_end - 1):0)  # length T_end
    gilt_proceeds    <- iota * sum(phi[1:T_end] * compound_weights)
    
    mpi_vec[i] <- gilt_proceeds
    
    ## 2g. Capital loss (only if no kick-out AND x(32) < gamma)
    if (is.na(ko_q) && x[max_quarters] < gamma) {
      ## Capital loss = (x0 - x[32])/x0 = 1 - x[32] (since x0=1)
      mpl_vec[i] <- 1 - x[max_quarters]
    }
    
  }  
  MPI <- mean(mpi_vec)
  MPL <- mean(mpl_vec)
  MPR <- MPI - MPL
  MPR
  
}

x0=1
m=0.2979
s= 0.1031
rg=0.011402
N=100000
P4(x0, m, s, rg,N)
