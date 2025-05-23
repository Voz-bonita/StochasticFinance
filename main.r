pacman::p_load("ggplot2", "quantmod", "glue")


# Setup for the analysis
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-12-31")

ticker_name <- "PETR4.SA"

getSymbols(ticker_name, from = start_date, to = end_date)
ticker <- get(ticker_name) # Ticker data

### Validation data
start_val_date <- end_date + 1
end_val_date <- Sys.Date()
ticker_val <- getSymbols(
  ticker_name,
  from = start_val_date, to = end_val_date,
  auto.assign = FALSE
)

# Exploratory
## Line plot of ticker pricing
options(repr.plot.width = 14, repr.plot.height = 8)
chartSeries(ticker, theme = chartTheme("black"), type = "line")
chartSeries(ticker_val, theme = chartTheme("black"), type = "line")

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
  t <- seq(0, limit, length.out = n)
  st <- s0 * exp((mu - sigma^2 / 2) * t + sigma * rnorm(n))
  return(st)
}

## Maximum Likelihood Estimators
estimators <- function(s, dt) {
  s <- as.numeric(s)
  x <- diff(log(s))
  sigma2_est <- (1 / dt) * var(x)
  mu_est <- (1 / dt) * mean(x) + (1 / 2) * sigma2_est^2
  return(c(mu_est, sigma2_est))
}

## Monte Carlo
n_train_obs <- dim(ticker)[1]
n_obs <- dim(ticker_val)[1]
num_simulations <- 100

### Model Params
par_est <- estimators(Cl(ticker), 1 / n_train_obs)
(mu_est <- par_est[1])
(sigma2_est <- par_est[2])
sigma_est <- sqrt(sigma2_est)
last_known_value <- xts::last(Cl(ticker))[[1]]

### Simulate paths
simmulations <- matrix(NA, nrow = n_obs, ncol = num_simulations)
for (i in 1:num_simulations) {
  sim_i <- rgbm(n_obs, mu_est, sigma_est, s0 = last_known_value)
  simmulations[, i] <- sim_i
}

### Mean path with intervals by quantiles
means <- rowMeans(simmulations)
lower_quantile <- apply(simmulations, 1, quantile, probs = 0.05)
upper_quantile <- apply(simmulations, 1, quantile, probs = 0.95)

means_xts <- xts::xts(means, order.by = index(ticker_val))
lower_xts <- xts::xts(lower_quantile, order.by = index(ticker_val))
upper_xts <- xts::xts(upper_quantile, order.by = index(ticker_val))
blank_xts <- xts::xts(
  1:(n_train_obs + n_obs),
  order.by = c(index(ticker), index(ticker_val))
)

### Plot true path vs simmulated path
plots_title <- glue("Simmulation path for {ticker_name}")
plots_ylim <- c(
  min(means, Cl(ticker_val), Cl(ticker), lower_quantile, upper_quantile),
  max(means, Cl(ticker_val), Cl(ticker), lower_quantile, upper_quantile)
)
plots_xlim <- c(index(ticker)[1], xts::last(index(ticker_val)))

plot(blank_xts, ylim = plots_ylim, type = "n", main = plots_title)
lines(Cl(ticker))
lines(means_xts, col = "red", lwd = 2)
lines(Cl(ticker_val), col = "royalblue", lwd = 2)
lines(lower_xts, col = "#ff5959")
lines(upper_xts, col = "#ff5959")

# Fitted Model Analsysis
par(mfrow = c(2, 1))
acf(
  dailyReturn(Cl(ticker_val)),
  main = "Autocorrelação dos Preços",
  lag.max = 500
)
acf(
  dailyReturn(means_xts),
  main = "Autocorrelação do modelo ajustado",
  lag.max = 500
)
par(mfrow = c(1, 1))


### Residual analysis
residuals <- Cl(ticker_val) - means_xts
names(residuals) <- c("Residuals")
residuals$abline <- 0

summary(residuals)
sd(residuals)
e1071::skewness(residuals)
e1071::kurtosis(residuals)


par(mfrow = c(2, 2))

boxplot(
  residuals[, "Residuals"],
  ylab = "Residuals",
  main = "Boxplot of Residuals"
)

hist(
  residuals[, "Residuals"],
  breaks = 20,
  main = "Histogram of Residuals",
  xlab = "Residuals"
)

plot.zoo(
  residuals[, "Residuals"],
  type = "l",
  xlab = "Date", ylab = "Residuals",
  main = "Fitted Model Residuals"
)
lines(zoo(residuals[, "abline"]), col = "red", lwd = 2)

ecostats::qqenvelope(
  as.numeric(residuals[, "Residuals"]),
  ylab = "Randomized Quantile Residuals",
  main = "QQPlot Residuals"
)
qqline(residuals[, "Residuals"], col = 2)
par(mfrow = c(1, 1))

# Daily estimation
## Fitting
full_series <- rbind(Cl(ticker), Cl(ticker_val))
means_dbd <- numeric(n_obs)
lower_quantile_dbd <- numeric(n_obs)
upper_quantile_dbd <- numeric(n_obs)
for (i in 0:(n_obs - 1)) {
  last_value <- full_series[n_train_obs + i, ]
  partial_series <- full_series[1:(n_train_obs + i), ]
  partial_parameters <- estimators(partial_series, 1 / nrow(partial_series))
  partial_mu <- partial_parameters[1]
  partial_sigma2 <- partial_parameters[2]
  partial_sigma <- sqrt(partial_sigma2)
  next_day_sim <- purrr::map_vec(
    1:num_simulations,
    ~ rgbm(1, partial_mu, partial_sigma, last_value)
  )

  means_dbd[i + 1] <- mean(next_day_sim)
  lower_quantile_dbd[i + 1] <- quantile(next_day_sim, probs = c(0.05))
  upper_quantile_dbd[i + 1] <- quantile(next_day_sim, probs = c(0.95))
}

means_dbd_xts <- xts::xts(means_dbd, order.by = index(ticker_val))
lower_dbd_xts <- xts::xts(lower_quantile_dbd, order.by = index(ticker_val))
upper_dbd_xts <- xts::xts(upper_quantile_dbd, order.by = index(ticker_val))

### Plot true path vs simmulated path
plots_title <- glue("Simmulation path for {ticker_name}")
plots_ylim <- c(
  min(means_dbd, Cl(full_series), lower_quantile_dbd, upper_quantile_dbd),
  max(means_dbd, Cl(full_series), lower_quantile_dbd, upper_quantile_dbd)
)
plots_xlim <- c(index(ticker)[1], xts::last(index(ticker_val)))

plot(blank_xts, ylim = plots_ylim, type = "n", main = plots_title)
lines(Cl(ticker))
lines(means_dbd_xts, col = "red", lwd = 2)
lines(Cl(ticker_val), col = "royalblue", lwd = 2)
lines(lower_dbd_xts, col = "#ff5959")
lines(upper_dbd_xts, col = "#ff5959")

## Residuals
residuals_dbd <- Cl(ticker_val) - means_dbd_xts
residuals_dbd <- residuals_dbd - mean(residuals_dbd)
names(residuals_dbd) <- c("Residuals")
residuals_dbd$abline <- 0


par(mfrow = c(2, 2))

boxplot(
  residuals_dbd[, "Residuals"],
  ylab = "Residuals",
  main = "Boxplot of Residuals"
)

hist(
  residuals_dbd[, "Residuals"],
  breaks = 20,
  main = "Histogram of Residuals",
  xlab = "Residuals"
)

plot.zoo(
  residuals_dbd[, "Residuals"],
  type = "l",
  xlab = "Date", ylab = "Residuals",
  main = "Fitted Model Residuals"
)
lines(zoo(residuals_dbd[, "abline"]), col = "red", lwd = 2)

ecostats::qqenvelope(
  as.numeric(residuals_dbd[, "Residuals"]),
  ylab = "Randomized Quantile Residuals",
  main = "QQPlot Residuals"
)
qqline(residuals_dbd[, "Residuals"], col = 2)
par(mfrow = c(1, 1))