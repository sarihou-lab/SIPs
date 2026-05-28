#Monte Carlo approximate MPR for P4
P4 <- function(x0, m, s, rg,N) {
  h          <- 0.25           # one quarter in years (= 1/4)
  b          <- m - 0.5 * s^2  # GBM drift = mu - 0.5*sigma^2
  
  # Thresholds (all scaled so x0 = 1)
  alpha <- 0.85    # income barrier:  FTSE >= 85% of start
  beta  <- 1.05    # kick-out barrier: FTSE >= 105% of start
  gamma <- 0.65    # capital barrier:  FTSE < 65% => capital at risk
  
  iota  <- 0.016   # 1.6% quarterly income rate
  
  # Kick-out check quarters: end of Years 2,3,4,5,6,7
  ko_quarters  <- c(8, 12, 16, 20, 24, 28)
  max_quarters <- 32            # Year 8 = final maturity
  
  # Vector of N elements
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
    
    #Termination quarter T
    T_end <- ifelse(is.na(ko_q), max_quarters, ko_q)
    compound_weights <- (1 + rg)^((T_end - 1):0)  # length T_end
    gilt_proceeds    <- iota * sum(phi[1:T_end] * compound_weights)
    
    mpi_vec[i] <- gilt_proceeds
    
    #Capital loss (only if no kick-out AND x(32) < gamma)
    if (is.na(ko_q) && x[max_quarters] < gamma) {
      #Capital loss = (x0 - x[32])/x0 = 1 - x[32] (since x0=1)
      mpl_vec[i] <- 1 - x[max_quarters]
    }
    
  }  
  MPI <- mean(mpi_vec)
  MPL <- mean(mpl_vec)
  MPR <- MPI - MPL
  MPR
  
}
rg <-  0.011402 # quarterly return of Gilt 4 
x0 <- 1

# Parameters estimated from historical FTSE 100 data
mu_base    <- 0.2979   # baseline growth rate
sigma_base <-  0.1031   # baseline volatility
benchmark  <-  0.430826 # total return of Gilt 4
Annual_YTM  <-   0.046394      # annual yield to maturity



#Sensitivity to growth rate μ (σ fixed)

# We sweep μ from -0.1 to 0.6, holding σ at its base value.
# This tells us the critical μ below which the plan is unattractive.

mu_seq  <- seq(-0.1, 0.6, by = 0.005)
MPR_mu  <- sapply(mu_seq, function(m) P4(x0, m, sigma_base, rg, 50000))

# Find the critical threshold: smallest μ where MPR >= benchmark
critical_mu_idx <- which(MPR_mu >= benchmark)[1]
critical_mu     <- mu_seq[critical_mu_idx]
cat("Critical μ threshold:", round(critical_mu, 4), "\n")

# Sensitivity to volatility σ (μ fixed)

# We sweep σ from 0.05 to 1.0, holding μ at its base value.
# This tells us the maximum σ before the plan falls below benchmark.

sigma_seq  <- seq(0.05, 1.0, by = 0.005)
MPR_sigma  <- sapply(sigma_seq, function(s) P4(x0,mu_base, s, rg, 50000))

# Find critical sigma: largest σ where MPR is still above benchmark
above_idx     <- which(MPR_sigma >= benchmark)
critical_sigma <- if (length(above_idx) > 0) sigma_seq[max(above_idx)] else NA
cat("Critical σ threshold:", round(critical_sigma, 4), "\n")

# 2D Heatmap of MPR over (μ, σ) grid

mu_grid    <- seq(-0.3, 0.5, length.out = 60)
sigma_grid <- seq(0.05, 0.5, length.out = 60)
MPR_grid <- outer(mu_grid, sigma_grid,
                  Vectorize(function(m, s) P4(x0, m, s,rg, 1000)))

#Monte Carlo – Sharpe ratio scatter plot (10 simulations)
# Each run uses N=5000 paths. Variability across runs shows
# the stochastic nature of the plan's risk-return profile.

set.seed(42)
N_runs  <- 10   # number of Monte Carlo simulation runs
N_paths <- 50000 # paths per run (small enough to show variability)

yearly_mean   <- numeric(N_runs)
yearly_sharpe <- numeric(N_runs)

for (i in 1:N_runs) {
  
  h          <- 0.25            # one quarter in years (= 1/4)
  b          <- mu_base - 0.5 * sigma_base^2  # GBM drift = mu - 0.5*sigma^2
  
  # Thresholds (all scaled so x0 = 1)
  alpha <- 0.85    # income barrier:  FTSE >= 85% of start
  beta  <- 1.05    # kick-out barrier: FTSE >= 105% of start
  gamma <- 0.65    # capital barrier:  FTSE < 65% => capital at risk
  
  iota  <- 0.016   # 1.6% quarterly income rate
  
  # Kick-out check quarters: end of Years 2,3,4,5,6,7
  ko_quarters  <- c(8, 12, 16, 20, 24, 28)
  max_quarters <- 32            # Year 8 = final maturity
  
  # Vector of N elements
  mpi_vec  <- numeric(N_paths)
  mpl_vec  <- numeric(N_paths)
  
  # Monte Carlo loop 
  for (j in 1:N_paths) {
    Z <- rnorm(max_quarters)
    x    <- numeric(max_quarters)
    x[1] <- x0 * exp(b * h + sigma_base * sqrt(h) * Z[1])
    for (q in 2:max_quarters)
      x[q] <- x[q-1] * exp(b * h + sigma_base * sqrt(h) * Z[q])
    phi <- as.numeric(x >= alpha)  # vector of length 32
    ko_q <- NA  # NA = no kick-out (plan runs to maturity)
    for (kq in ko_quarters) {
      if (x[kq] >= beta) {
        ko_q <- kq   # first annual check where FTSE >= 1.05
        break        # stop at first kick-out
      }
    }
    
    #Termination quarter T
    T_end <- ifelse(is.na(ko_q), max_quarters, ko_q)
    compound_weights <- (1 + rg)^((T_end - 1):0)  # length T_end
    gilt_proceeds    <- iota * sum(phi[1:T_end] * compound_weights)
    
    mpi_vec[j] <- gilt_proceeds
    
    # Capital loss (only if no kick-out AND x(32) < gamma)
    if (is.na(ko_q) && x[max_quarters] < gamma) {
      # Capital loss = (x0 - x[32])/x0 = 1 - x[32] (since x0=1)
      mpl_vec[j] <- 1 - x[max_quarters]
    }
    
  }  
  
  MPR <- mpi_vec - mpl_vec
  
  
  # Annualize
  annual_returns  <- MPR / 8
  yearly_mean[i]  <- mean(annual_returns)
  # Sharpe ratio = (mean annual return - risk-free) / std dev
  yearly_sharpe[i] <- (mean(annual_returns) - Annual_YTM) / sd(annual_returns)
}

#PLOTS
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Plot 1: MPR vs μ
plot(mu_seq, MPR_mu, type = "l", col = "#1D9E75", lwd = 2,
     xlab = expression(mu ~ "(growth rate)"), ylab = "MPR",
     main = "Sensitivity of MPR to Growth Rate μ")
abline(h  = benchmark,   col = "#D85A30", lty = 2, lwd = 1.5)
abline(v  = critical_mu, col = "#7F77DD", lty = 3, lwd = 1.5)
abline(v  = mu_base,     col = "#888780", lty = 2)
legend("topleft", bty = "n", cex = 0.8,
       legend = c("MPR", "Benchmark", sprintf("Critical μ=%.3f", critical_mu), "Base μ"),
       col    = c("#1D9E75", "#D85A30", "#7F77DD", "#888780"),
       lty    = c(1, 2, 3, 2), lwd = c(2, 1.5, 1.5, 1.5))

# Plot 2: MPR vs σ
plot(sigma_seq, MPR_sigma, type = "l", col = "#378ADD", lwd = 2,
     xlab = expression(sigma ~ "(volatility)"), ylab = "MPR",
     main = "Sensitivity of MPR to Volatility σ")
abline(h = benchmark,     col = "#D85A30", lty = 2, lwd = 1.5)
abline(v = sigma_base,    col = "#888780", lty = 2)
if (!is.na(critical_sigma)) abline(v = critical_sigma, col = "#7F77DD", lty = 3, lwd = 1.5)
legend("topright", bty = "n", cex = 0.8,
       legend = c("MPR", "Benchmark", "Base σ"),
       col    = c("#378ADD", "#D85A30", "#888780"),
       lty    = c(1, 2, 2), lwd = c(2, 1.5, 1.5))

# Plot 3: 2D heatmap
image(mu_grid, sigma_grid, MPR_grid,
      col  = hcl.colors(20, "YlGnBu", rev = TRUE),
      xlab = expression(mu), ylab = expression(sigma),
      main = "MPR Heatmap over (μ, σ) grid")
contour(mu_grid, sigma_grid, MPR_grid,
        levels = benchmark, col = "#D85A30", lwd = 2,
        labels = "Benchmark", add = TRUE)
points(mu_base, sigma_base, pch = 4, cex = 2, lwd = 2, col = "white")

# Plot 4: Yearly mean return vs Yearly Sharpe ratio (10 simulations)
plot(yearly_mean, yearly_sharpe,
     col  = "#1D9E75", pch = 1, lwd = 2, cex = 1.5,
     xlab = "Yearly mean return",
     ylab = "Yearly Sharpe ratio",
     main = "Plan P4: Yearly mean return vs yearly Sharpe ratio")
abline(h = 0, col = "#D85A30", lty = 2)
text(yearly_mean, yearly_sharpe,
     labels = 1:N_runs, pos = 3, cex = 0.7,
     col = "#5F5E5A")

