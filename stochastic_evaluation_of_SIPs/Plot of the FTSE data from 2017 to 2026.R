#Load necessary libraries

library(quantmod)
library(ggplot2)
library(dplyr)

# Download FTSE 100 historical data from Yahoo Finance
# The ticker for FTSE 100 index is "^FTSE"
ftse_symbol <- "^FTSE"
start_date  <- "2017-01-01"
end_date    <- "2026-05-12"

getSymbols(ftse_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Clean and convert the xts object to a standard data frame
ftse_df <- data.frame(Date = index(FTSE), coredata(FTSE)) %>%
  rename(Close = FTSE.Close) %>%
  select(Date, Close) %>%
  filter(!is.na(Close)) # Remove non-trading days/nulls

# 4. Generate the time-series line plot
ftse_plot <- ggplot(ftse_df, aes(x = Date, y = Close)) +
  geom_line(color = "#005A9C", size = 0.8) +  # Classic financial blue theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    #title = "FTSE 100 Index Performance (2017 - 2026)",
    subtitle = "Daily Closing Prices in GBP",
    x = "Timeline",
    y = "Index Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Display the plot in the R environment
print(ftse_plot)

