#Load required library
library(ggplot2)

simulate_gbm <- function(x0, mu, sigma, n_days = 253) {
  # Time parameters
  dt <- 1 / 253 # Annualized daily time step
  t  <- 0:n_days
  
  # Generate random normal innovations
  set.seed(42) # Set seed for reproducible path generation
  Z <- rnorm(n_days, mean = 0, sd = 1)
  
  # Calculate the Brownian motion path
  # dW = Z * sqrt(dt)
  W <- c(0, cumsum(Z * sqrt(dt)))
  
  # GBM analytical solution formula: x(t) = x0 * exp((mu - 0.5 * sigma^2)*t + sigma*W(t))
  path <- x0 * exp((mu - 0.5 * sigma^2) * (t * dt) + sigma * W)
  
  # Structure data into a data frame for plotting
  df <- data.frame(
    Day   = t,
    Price = path
  )
  
  # Generate the line plot using ggplot2
  p <- ggplot(df, aes(x = Day, y = Price)) +
    geom_line(color = "#0073C2", size = 1) +
    theme_minimal() +
    labs(
      #title = "Geometric Brownian Motion (GBM) Simulation",
      subtitle = sprintf("Inputs: X0 = %s | Mu = %s | Sigma = %s", x0, mu, sigma),
      x = "Trading Days",
      y = "Index Level"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 11)
    )
  
  # Display the plot
  print(p)
  
  # Return data frame invisibly
  return(invisible(df))
}

# Simulate GBM for x0 = 6889.1, mu = -0.1205, sigma = 0.1276, n_days = 253
simulate_gbm(x0 = 6889.1, mu = -0.1205, sigma = 0.1276, n_days = 253)

