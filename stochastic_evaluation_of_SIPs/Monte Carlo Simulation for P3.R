#This function takes as input the initial index x0, the estimated growth rate
# m, the estimated volatility s and the number of simulation N. As output,
# it returns the approximated Mean Percentage of Return of plan P2.
P3 <- function(x0, m, s, N) {
  h        <- 0.5 # time step
  b        <- m - 0.5*s^2 # drift term
  kick_ret <- (3:10) * 0.04275   # returns at 8 kick-out dates
  mpr      <- numeric(N) # vector of N elements
  
  #Generate 8 paths N times
  for (i in 1:N) {
    Z    <- rnorm(8)
    x    <- numeric(8)
    x[1] <- exp(b*1.5 + s*sqrt(1.5)*Z[1])
    for (k in 2:8)
      x[k] <- x[k-1] * exp(b*h + s*sqrt(h)*Z[k])
    
    ko <- which(x >= 1)[1]#find the first observation where the Index is at or above its starting level
    # if ko is missing
    if (!is.na(ko)) {
      mpr[i] <- kick_ret[ko]
    } else {
      if (x[8] < 0.6) mpr[i] <- x[8] - 1 #Ckeck index level for last observation date if below barrier protection.
    }
  }
  mean(mpr)
}

x0=10233.10
m=0.1918
s=0.1069
N=100000
P3(x0, m, s, N)
