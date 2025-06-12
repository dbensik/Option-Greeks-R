# main.R

# Load all modules
source("option_pricing.R")
source("option_greeks.R")
source("utils.R")
source("plots_2d.R")
source("plots_3d.R")

# Demo parameters
S_vals <- seq(75, 125, length = 40)
t_vals <- seq(1/52, 1, length = 40)
X <- 100
r <- 0.1
v <- 0.4

# Generate sample plots
CallOption3DPlot <- function() {
  persp3d <- plot3DGreeks(0, "delta", S = S_vals, X = X, t = t_vals, r = r, v = v)
}

Call2DGreeks <- function() {
  plot2DGreeks(0, "vega", S = seq(60, 140, length = 100), X = X, t = 0.5, r = r, v = v)
}

UnderlyingPlot <- function() {
  plotOptionVsUnderlying(0, "spot", S = 100, X = X, t = 0.5, r = r, v = v)
}

# Run demos
CallOption3DPlot()
Call2DGreeks()
UnderlyingPlot()
