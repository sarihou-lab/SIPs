#Monte Carlo approximated MPR for plan P2
P2 <- function(N, x0, mu, sigma){
  
  drift <- (mu - 0.5 * sigma^2)
  
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  Z3 <- rnorm(N)
  Z4 <- rnorm(N)
  
  x4 <- x0 * exp(4 * drift + sigma * sqrt(4) * Z1)
  x5 <- x4 * exp(drift + sigma * Z2)
  x6 <- x5 * exp(drift + sigma * Z3)
  x7 <- x6 * exp(drift + sigma * Z4)
  
  payoff <- numeric(N)
  
  payoff[x4 >= x0] <- 4 * 0.056
  
  idx2 <- (x4 < x0) & (x5 >= x0)
  payoff[idx2] <- 5 * 0.056
  
  idx3 <- (x4 < x0) & (x5 < x0) & (x6 >= 0.95 * x0)
  payoff[idx3] <- 6 * 0.056
  
  idx4 <- (x4 < x0) & (x5 < x0) & (x6 < 0.95 * x0) & (x7 >= 0.9 * x0)
  payoff[idx4] <- 7 * 0.056
  
  mean(payoff)
}

x0 <- 10379.10


# Parameters estimated from historical FTSE 100 data
mu_base    <- 0.2233   # baseline growth rate
sigma_base <-  0.1010   # baseline volatility
benchmark  <-  0.37144 # total return of Gilt 2
Annual_YTM  <-   0.046514     # annual yield to maturity


# Sensitivity to growth rate μ (σ fixed)

# We sweep μ from 0.0 to 0.8, holding σ at its base value.
# This tells us the critical μ below which the plan is unattractive.

mu_seq  <- seq(0.0, 0.8, by = 0.005)
MPR_mu  <- sapply(mu_seq, function(m) P2(10^6,x0,m, sigma_base))

# Find the critical threshold: smallest μ where MPR >= benchmark
critical_mu_idx <- which(MPR_mu >= benchmark)[1]
critical_mu     <- mu_seq[critical_mu_idx]
cat("Critical μ threshold:", round(critical_mu, 4), "\n")

#Sensitivity to volatility σ (μ fixed)

# We sweep σ from 0.05 to 1.0, holding μ at its base value.
# This tells us the maximum σ before the plan falls below benchmark.

sigma_seq  <- seq(0.05, 1.0, by = 0.005)
MPR_sigma  <- sapply(sigma_seq, function(s) P2(10^6,x0,mu_base, s))

# Find critical sigma: largest σ where MPR is still above benchmark
above_idx     <- which(MPR_sigma >= benchmark)
critical_sigma <- if (length(above_idx) > 0) sigma_seq[max(above_idx)] else NA
cat("Critical σ threshold:", round(critical_sigma, 4), "\n")

#2D Heatmap of MPR over (μ, σ) grid

mu_grid    <- seq(-0.3, 0.3, length.out = 60)
sigma_grid <- seq(0.05, 0.5, length.out = 60)
MPR_grid <- outer(mu_grid, sigma_grid,
                  Vectorize(function(m, s) P2(50000, x0, m, s)))

# Monte Carlo – Sharpe ratio scatter plot (10 simulations)
# Each run uses N=100000 paths. Variability across runs shows
# the stochastic nature of the plan's risk-return profile.

set.seed(42)
N_runs  <- 10   # number of Monte Carlo simulation runs
N_paths <- 10^6 # paths per run (small enough to show variability)

yearly_mean   <- numeric(N_runs)
yearly_sharpe <- numeric(N_runs)

for (i in 1:N_runs) {
  
  drift <- (mu_base - 0.5 * sigma_base^2)
  
  Z1 <- rnorm(N_paths)
  Z2 <- rnorm(N_paths)
  Z3 <- rnorm(N_paths)
  Z4 <- rnorm(N_paths)
  
  x4 <- x0 * exp(4 * drift + sigma_base * sqrt(4) * Z1)
  x5 <- x4 * exp(drift + sigma_base * Z2)
  x6 <- x5 * exp(drift + sigma_base * Z3)
  x7 <- x6 * exp(drift + sigma_base * Z4)
  
  payoff <- numeric(N_paths)
  
  payoff[x4 >= x0] <- 4 * 0.056
  
  idx2 <- (x4 < x0) & (x5 >= x0)
  payoff[idx2] <- 5 * 0.056
  
  idx3 <- (x4 < x0) & (x5 < x0) & (x6 >= 0.95 * x0)
  payoff[idx3] <- 6 * 0.056
  
  idx4 <- (x4 < x0) & (x5 < x0) & (x6 < 0.95 * x0) & (x7 >= 0.9 * x0)
  payoff[idx4] <- 7 * 0.056
  
  # Annualize: divide 7-year total return by 7
  annual_returns  <- payoff / 7
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
     main = "Plan P2: Yearly mean return vs yearly Sharpe ratio")
abline(h = 0, col = "#D85A30", lty = 2)
text(yearly_mean, yearly_sharpe,
     labels = 1:N_runs, pos = 3, cex = 0.7,
     col = "#5F5E5A")

