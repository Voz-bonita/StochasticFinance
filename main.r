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

## Line plot for closing price
daily_returns <- dailyReturn(Cl(ticker))
plot(daily_returns, main = glue("Daily Returns of {ticker_name}"))

## Table visualization
head(ticker)
tail(ticker)