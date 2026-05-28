required_packages <- c("quantmod", "tseries", "moments",
                       "ggplot2", "gridExtra", "nortest",
                       "forecast", "MASS", "fitdistrplus")

# Load required library
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}

#Parameter function
Parameter <- function(stdate, endate){
  # Get the data of the year before the plan started
  getSymbols("^FTSE",
             src     = "yahoo",
             from    = stdate,
             to      = endate,
             auto.assign = TRUE)
  
  # Filter the FTSE time-series dataset to extract and store only 
  #the data that falls between the specified start and end dates
  ftse_data <- FTSE[paste0(stdate, "/", endate)]
  
  #Convert a time-series object (ftse_data) into a standard R
  # data frame (ftse_df) while extracting and renaming 
  #the date, opening, closing, highest, lowest and volume trading values.
  ftse_df   <- data.frame(
    Date       = index(ftse_data),
    Open       = as.numeric(Op(ftse_data)),
    Close      = as.numeric(Cl(ftse_data)),
    High       = as.numeric(Hi(ftse_data)),
    Low        = as.numeric(Lo(ftse_data)),
    Volume     = as.numeric(Vo(ftse_data))
  )
  
  # Remove any rows with NA (cleaning the data)
  ftse_df <- na.omit(ftse_df)
  
  # Daily log returns:  ξ_i = log(x_i / x_{i-1})
  log_returns <- diff(log(ftse_df$Close))
  
  # Annualised parameter estimates 
  h      <- 1 / length(log_returns)  # step size
  xi_bar <- mean(log_returns)         # sample mean  = (mu - 0.5*sigma^2)*h
  s2_xi  <- var(log_returns)    # sample var   = sigma^2 * h
  #Number of log returns
  n_days <- length(log_returns)
  #Estimated mu
  mu_hat    <- n_days * (xi_bar + 0.5 * s2_xi)
  #Estimated sigma
  sigma_hat <- sqrt(n_days * s2_xi)
  
  cat(sprintf("Estimated mu    (growth rate) : %.4f\n", mu_hat))
  cat(sprintf("Estimated sigma (volatility)  : %.4f\n", sigma_hat))
}

#Parameter estimation for each plan 
#Plan P1
#Parameter("2018-01-02", "2018-12-31") 
#Plan P2
Parameter("2025-04-24", "2026-04-24")
#Plan P3
#Parameter("2025-05-08", "2026-05-08")
#Plan P4
#Parameter("2025-04-10", "2026-04-10")
