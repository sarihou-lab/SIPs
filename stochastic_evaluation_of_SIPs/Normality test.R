
#Load required packages 
required_packages <- c("quantmod", "tseries", "moments",
                       "ggplot2", "gridExtra", "nortest",
                       "forecast", "MASS", "fitdistrplus")
library(tseries)

for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }

# Download FTSE 100 data for 2018
getSymbols("^FTSE",
           src     = "yahoo",
           from    = "2018-01-01",
           to      = "2019-01-01",
           auto.assign = TRUE)

# Extract relevant columns
ftse_data <- FTSE["2018"]
ftse_df   <- data.frame(
  Date       = index(ftse_data),
  Open       = as.numeric(Op(ftse_data)),
  Close      = as.numeric(Cl(ftse_data)),
  High       = as.numeric(Hi(ftse_data)),
  Low        = as.numeric(Lo(ftse_data)),
  Volume     = as.numeric(Vo(ftse_data))
)

# Remove any rows with NA
ftse_df <- na.omit(ftse_df)



# FTSE 100 INDEX 2018 data summary
cat(sprintf("Number of trading days : %d\n",   nrow(ftse_df)))
cat(sprintf("Start date             : %s\n",   as.character(min(ftse_df$Date))))
cat(sprintf("End date               : %s\n\n", as.character(max(ftse_df$Date))))


#Close Price Statistics
cat(sprintf("Opening level (Jan 2) : %.2f\n",  ftse_df$Close[1]))
cat(sprintf("Closing level (Dec 31): %.2f\n",  ftse_df$Close[nrow(ftse_df)]))
cat(sprintf("Minimum close         : %.2f\n",  min(ftse_df$Close)))
cat(sprintf("Maximum close         : %.2f\n",  max(ftse_df$Close)))
cat(sprintf("Mean close            : %.2f\n",  mean(ftse_df$Close)))
cat(sprintf("Std dev (close)       : %.2f\n",  sd(ftse_df$Close)))
cat(sprintf("Annual return         : %.2f%%\n",
            (ftse_df$Close[nrow(ftse_df)] / ftse_df$Close[1] - 1) * 100))


# Plot of Close Price Time Series 
plot(ftse_df$Date, ftse_df$Close,
     type = "l", col = "#185FA5", lwd = 1.5,
     # main = "FTSE 100 Close Price 2018",
     xlab = "Date", ylab = "Index Level",
     las  = 1)
grid(col = "lightgray", lty = 2)




# Daily log returns:  ξ_i = log(x_i / x_{i-1})
log_returns <- diff(log(ftse_df$Close))

#Log Return Statistics
cat(sprintf("Number of log returns  : %d\n",   length(log_returns)))
cat(sprintf("Mean log return        : %.6f\n",  mean(log_returns)))
cat(sprintf("Std dev log return     : %.6f\n",  sd(log_returns)))
cat(sprintf("Skewness               : %.4f\n",  skewness(log_returns)))
cat(sprintf("Kurtosis (excess)      : %.4f\n",  kurtosis(log_returns) - 3))
cat(sprintf("Min log return         : %.6f\n",  min(log_returns)))
cat(sprintf("Max log return         : %.6f\n",  max(log_returns)))

# Plot of Log Returns Time Series
plot(ftse_df$Date[-1], log_returns,
     type = "l", col = "#1D9E75", lwd = 1,
     #main = "FTSE 100 Daily Log Returns — 2018",
     xlab = "Date", ylab = "Log Return",
     las  = 1)
abline(h = 0, col = "red", lty = 2)
grid(col = "lightgray", lty = 2)


#Normality test on log return

#Augmented Dickey-Fuller(ADF)
adf_result <- adf.test(log_returns)
print(adf_result)

# ACF of Log Returns 
acf(log_returns,
    lag.max = 30,
    main    = " ",
    col     = "#185FA5",
    lwd     = 2,
    las     = 1)


#Shapiro-Wilk test
sw_lr  <- shapiro.test(log_returns)
cat(sprintf("  Shapiro-Wilk        : W = %.4f, p-value = %.6f\n",
            sw_lr$statistic, sw_lr$p.value))

# QQ Plot — Log Returns
qqnorm(log_returns,
       main = " ",
       col  = "#1D9E75", pch = 16, cex = 0.6,
       las  = 1)
qqline(log_returns, col = "red", lwd = 2)
grid(col = "lightgray", lty = 2)


#CDF of log returns vs Normal CDF
plot(ecdf(log_returns),
     main = " ",
     xlab = "Log Return",
     ylab = "Cumulative Probability",
     col  = "#1D9E75",
     lwd  = 2,
     las  = 1)
curve(pnorm(x, mean = mean(log_returns), sd = sd(log_returns)),
      col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topleft",
       legend = c("Empirical CDF", "Normal CDF"),
       col    = c("#1D9E75", "red"),
       lty    = c(1, 2),
       lwd    = c(2, 2),
       bty    = "n")
grid(col = "lightgray", lty = 2)

# Histogram of Log Returns vs Normal 
hist(log_returns,
     freq    = FALSE,
     breaks  = 25,
     col     = "#9FE1CB",
     border  = "#0F6E56",
     main    = " ",
     xlab    = "Log Return",
     las     = 1)
curve(dnorm(x, mean = mean(log_returns), sd = sd(log_returns)),
      col = "red", lwd = 2, add = TRUE)
# Add t-distribution fit for comparison
fit_t  <- fitdistr(log_returns, "t")
curve(dt((x - fit_t$estimate["m"]) / fit_t$estimate["s"],
         df = fit_t$estimate["df"]) / fit_t$estimate["s"],
      col = "blue", lwd = 2, lty = 2, add = TRUE)
legend("topright",
       legend = c("Empirical", "Normal fit", "t-dist fit"),
       fill   = c("#9FE1CB", NA, NA),
       border = c("#0F6E56", NA, NA),
       lty    = c(NA, 1, 2),
       col    = c(NA, "red", "blue"),
       lwd    = c(NA, 2, 2),
       bty    = "n")


