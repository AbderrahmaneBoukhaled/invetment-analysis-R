# Install packages (only run once)
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tidyverse")
install.packages("openxlsx")

# Load libraries
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(openxlsx)

# Define proper time period (2 years)
start_date <- as.Date("2022-07-01")
end_date <- as.Date("2024-07-01")

# Define stock tickers
symbols <- c("HSBA.L", "BARC.L", "LLOY.L", "^FTSE", "^GSPC")

# Get prices
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)

# Merge Adjusted Prices (FTSE & GSPC have names like FTSE and GSPC in memory)
prices <- merge(
  Ad(`HSBA.L`),
  Ad(`BARC.L`),
  Ad(`LLOY.L`),
  Ad(FTSE),
  Ad(GSPC)
)

# Rename columns
colnames(prices) <- c("HSBC", "Barclays", "Lloyds", "FTSE100", "SP500")

# Remove NA values
prices <- na.omit(prices)

# View first rows
head(prices)


# Calculate monthly returns for each column separately
monthly_returns <- do.call(merge, lapply(as.list(prices), monthlyReturn))
colnames(monthly_returns) <- colnames(prices)

# Remove NA values
monthly_returns <- na.omit(monthly_returns)

# Plot cumulative monthly returns
chart.CumReturns(
  monthly_returns,
  legend.loc = "topleft",
  wealth.index = TRUE,
  main = "Cumulative Returns: UK Bank Stocks vs Benchmarks"
)

# Calculate annualised Sharpe ratios (assuming risk-free rate = 0)
sharpe_ratios <- SharpeRatio.annualized(monthly_returns, Rf = 0, scale = 12)
print("Annualized Sharpe Ratios:")
print(sharpe_ratios)

# Calculate standard deviation (monthly volatility)
volatility_monthly <- apply(monthly_returns, 2, sd)

# Convert to annualized volatility
volatility_annualized <- volatility_monthly * sqrt(12)
print("Annualized Volatility:")
print(volatility_annualized)

# Correlation matrix between all assets
cor_matrix <- cor(monthly_returns)
print("Correlation Matrix:")
print(cor_matrix)

# Optional: Heatmap of correlation matrix
heatmap(cor_matrix, main = "Correlation Matrix: UK Banks & Indices", col = colorRampPalette(c("red", "white", "blue"))(20))

# Export all data to Excel
write.xlsx(as.data.frame(prices), file = "stock_prices.xlsx", sheetName = "Prices", rowNames = TRUE)
write.xlsx(as.data.frame(monthly_returns), file = "monthly_returns.xlsx", sheetName = "Monthly Returns", rowNames = TRUE)
write.xlsx(as.data.frame(sharpe_ratios), file = "sharpe_ratios.xlsx", sheetName = "Sharpe Ratios", rowNames = TRUE)
write.xlsx(as.data.frame(volatility_annualized), file = "volatility.xlsx", sheetName = "Annualized Volatility", rowNames = TRUE)
write.xlsx(as.data.frame(cor_matrix), file = "correlation_matrix.xlsx", sheetName = "Correlation Matrix", rowNames = TRUE)

getwd()

# Step 3: Portfolio Construction & Risk-Return Comparison


# Ensure monthly_returns is already created and cleaned
monthly_matrix <- coredata(monthly_returns)  # Extract numeric matrix

# Define portfolio weights
weights_conservative <- c(0.15, 0.15, 0.10, 0.40, 0.20)  # HSBC, Barclays, Lloyds, FTSE100, S&P500
weights_aggressive   <- c(0.25, 0.25, 0.20, 0.10, 0.20)


# 1. Calculate Portfolio Monthly Returns
portfolio_returns_conservative <- as.numeric(monthly_matrix %*% weights_conservative)
portfolio_returns_aggressive   <- as.numeric(monthly_matrix %*% weights_aggressive)

# 2. Annualised Return
annual_return_conservative <- mean(portfolio_returns_conservative) * 12
annual_return_aggressive   <- mean(portfolio_returns_aggressive) * 12

# 3. Annualised Volatility
volatility_conservative <- sd(portfolio_returns_conservative) * sqrt(12)
volatility_aggressive <- sd(portfolio_returns_aggressive) * sqrt(12)

# 4. Sharpe Ratio (Assume Risk-Free Rate = 0)
sharpe_conservative <- annual_return_conservative / volatility_conservative
sharpe_aggressive   <- annual_return_aggressive / volatility_aggressive

# 5. Create summary table
portfolio_summary <- data.frame(
  Portfolio = c("Conservative", "Aggressive"),
  Annual_Return = round(c(annual_return_conservative, annual_return_aggressive), 4),
  Annual_Volatility = round(c(volatility_conservative, volatility_aggressive), 4),
  Sharpe_Ratio = round(c(sharpe_conservative, sharpe_aggressive), 4)
)

# 6. Display summary
print(portfolio_summary)

# 7. Export to Excel
write.xlsx(portfolio_summary, file = "portfolio_summary_step3.xlsx", rowNames = FALSE)
write.xlsx(monthly_matrix, file = "portfolio_summary_step4.xlsx", rowNames = FALSE)

