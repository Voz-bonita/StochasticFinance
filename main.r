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


# Helper Functions
## Random Geometric Brownian Motion
rgbm <- function(n, mu, sigma, s0 = 1, limit = 1) {
  # Uses SDE Solution
  t <- seq(0, 1, length.out = n)
  st <- s0 * exp((mu - sigma^2 / 2) * t + sigma * rnorm(n))
  return(st)
}

## Maximum Likelihood Estimators
estimators <- function(s, dt) {
  s <- as.numeric(s)
  sigma2_est <- (1 / dt) * var(diff(log(s)))
  mu_est <- (1 / dt) * mean(diff(log(s))) + (1 / 2) * sigma2_est^2
  return(c(mu_est, sigma2_est))
}

## Monte Carlo
### Validation data
start_val_date <- end_date + 1
end_val_date <- Sys.Date()
ticker_val <- getSymbols(
  ticker_name,
  from = start_val_date, to = end_val_date,
  auto.assign = FALSE
)

n_obs <- dim(ticker_val)[1]
num_simulations <- 100

### Model Params
par_est <- estimators(Cl(ticker), dt)
(mu_est <- par_est[1])
(sigma_est <- par_est[2])
last_known_value <- xts::last(Cl(ticker))[[1]]


### Simulate path
sim_path_example <- rgbm(n_obs, mu_est, sigma_est, s0 = last_known_value)
sim_path_example_xts <- xts::xts(sim_path_example, order.by = index(ticker_val))

### Plot true path vs simmulated path
plots_title <- glue("Simmulation path for {ticker_name}")
plots_ylim <- c(
  min(sim_path_example, Cl(ticker_val), Cl(ticker)),
  max(sim_path_example, Cl(ticker_val), Cl(ticker))
)
plots_xlim <- c(index(ticker)[1], xts::last(index(ticker_val)))
n_train_obs <- dim(ticker)[1]
blank_xts <- xts::xts(
  1:(n_train_obs + n_obs),
  order.by = c(index(ticker), index(ticker_val))
)
plot(blank_xts, ylim = plots_ylim, type = "n", main = plots_title)
lines(Cl(ticker))
lines(sim_path_example_xts, col = "red", lwd = 2)
lines(Cl(ticker_val), col = "royalblue", lwd = 4)