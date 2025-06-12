# option_greeks.R

# Greeks calculation functions for options
d1_d2 <- function(S, X, t, r, v) {
  d1 <- (log(S/X) + (r + 0.5 * v^2) * t) / (v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  list(d1 = d1, d2 = d2)
}

delta <- function(type, S, X, t, r, v) {
  d <- d1_d2(S, X, t, r, v)$d1
  if (type > 0) pnorm(d) - 1 else pnorm(d)
}

gamma <- function(S, X, t, r, v) {
  d <- d1_d2(S, X, t, r, v)$d1
  exp(-d^2 / 2) / (S * v * sqrt(2 * pi * t))
}

vega <- function(S, X, t, r, v) {
  d <- d1_d2(S, X, t, r, v)$d1
  S * sqrt(t) * exp(-d^2 / 2) / sqrt(2 * pi)
}

theta <- function(type, S, X, t, r, v) {
  d <- d1_d2(S, X, t, r, v)
  p1 <- -S * exp(-d$d1^2 / 2) * v / (2 * sqrt(2 * pi * t))
  p2 <- r * X * exp(-r * t) * pnorm(if (type > 0) -d$d2 else d$d2)
  if (type > 0) p1 + p2 else p1 - p2
}

rho <- function(type, S, X, t, r, v) {
  d <- d1_d2(S, X, t, r, v)$d2
  r_term <- X * t * exp(-r * t)
  if (type > 0) -r_term * pnorm(-d) else r_term * pnorm(d)
}

greeks <- function(type, fnc, S, X, t, r, v) {
  switch(fnc,
         delta = delta(type, S, X, t, r, v),
         gamma = gamma(S, X, t, r, v),
         vega = vega(S, X, t, r, v),
         theta = theta(type, S, X, t, r, v),
         rho = rho(type, S, X, t, r, v))
}
