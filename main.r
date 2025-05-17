pacman::p_load("ggplot2", "quantmod", "glue")


# Setup for the analysis
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-12-31")

ticker_name <- "PETR4.SA"

getSymbols(ticker_name, from = start_date, to = end_date)
ticker <- get(ticker_name) # Ticker data

# Exploratory
## Line plot of ticker pricing
options(repr.plot.width = 14, repr.plot.height = 8)
chartSeries(ticker, theme = chartTheme("black"), type = "line")

## Plots for return value
daily_returns <- dailyReturn(Cl(ticker))
return_mu <- mean(daily_returns)
return_sd <- sd(daily_returns)

normal_fit_x <- seq(min(daily_returns), max(daily_returns), length.out = 100)
normal_fit_y <- dnorm(normal_fit_x, return_mu, return_sd)

par(mfrow = c(3, 1))
plot(daily_returns, main = glue("Daily Returns of {ticker_name}"))
hist(daily_returns, main = glue("Daily Returns of {ticker_name}"))
plot(density(daily_returns), type = "n") # invisible plot to separate lines
lines(density(daily_returns), col = "black", lwd = 2, )
lines(normal_fit_x, normal_fit_y, col = "red", lwd = 2)
par(mfrow = c(1, 1))

## Table visualization
head(ticker)
tail(ticker)