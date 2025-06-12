# option_pricing.R

# Black-Scholes option pricing functions
call.value <- function(S, X, t, r, v) {
  d1 <- (log(S/X) + (r + 0.5 * v^2) * t) / (v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  S * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
}

put.value <- function(S, X, t, r, v) {
  d1 <- (log(S/X) + (r + 0.5 * v^2) * t) / (v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  X * exp(-r * t) * pnorm(-d2) - S * pnorm(-d1)
}
