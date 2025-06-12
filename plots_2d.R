# plots_2d.R

plotOptionVsUnderlying <- function(type, func, S, X, t, r, v) {
  option <- ifelse(type == 0, "Call", "Put")

  variable <- switch(func,
                     spot = "Spot",
                     strike = "Strike",
                     volatility = "Volatility",
                     time = "Time",
                     rate = "Interest Rate")

  x <- switch(func,
              spot = seq(from = 50, to = 150, length = 100),
              strike = seq(from = 50, to = 150, length = 100),
              volatility = seq(from = 0.1, to = 0.5, length = 100),
              time = seq(from = 0, to = 1, length = 100),
              rate = seq(from = 0.0, to = 0.25, length = 100))

  optionValue <- switch(func,
                        spot = if (type == 0) call.value(x, X, t, r, v) else put.value(x, X, t, r, v),
                        strike = if (type == 0) call.value(S, x, t, r, v) else put.value(S, x, t, r, v),
                        volatility = if (type == 0) call.value(S, X, t, r, x) else put.value(S, X, t, r, x),
                        time = if (type == 0) call.value(S, X, x, r, v) else put.value(S, X, x, r, v),
                        rate = if (type == 0) call.value(S, X, t, x, v) else put.value(S, X, t, x, v))

  plot(x, optionValue, type = "l", xlab = simpleCap(variable),
       ylab = "Option Value",
       main = paste(option, "Value vs.", simpleCap(variable)))
}

plot2DGreeks <- function(type, func, S, X, t, r, v) {
  option <- ifelse(type == 0, "Call", "Put")
  greek <- greeks(type, func, S, X, t, r, v)
  plot(S, greek, type = "l", xlab = "Spot Price", ylab = simpleCap(func),
       main = paste(option, simpleCap(func), "(T =", round(t * 365), "days)"))
}
