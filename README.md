# Option Greeks: Black-Scholes Analysis Toolkit in R

This R project provides a comprehensive suite for computing and visualizing the sensitivities (Greeks) of European options using the Black-Scholes model. Modularized for clarity, it includes functions for pricing, sensitivity analysis, and both 2D and 3D plotting of results.

## 💡 Features

- Black-Scholes pricing for European Call and Put options
- Calculation of all five primary Greeks: Delta, Gamma, Vega, Theta, Rho
- Modular design across:
  - `option_pricing.R` — pricing functions
  - `option_greeks.R` — Greeks and core logic
  - `plots_2d.R` — line plots of option value vs. parameters
  - `plots_3d.R` — surface plots for dynamic visualization
  - `utils.R` — helper formatting tools
  - `main.R` — entry point demo script

## 📦 Dependencies

This project relies on base R functions and the `stats` package (for `pnorm`) and `graphics` for plotting—no external packages required.

## 🚀 Getting Started

1. Clone this repository.
2. Run `main.R` in your R environment.
3. Explore 2D and 3D visualizations of options and their sensitivities.

## 🛠 File Structure
```bash
option_pricing.R # Black-Scholes pricing
option_greeks.R # Delta, Gamma, Theta, Rho, Vega
plots_2d.R # Line plots for sensitivity analysis
plots_3d.R # 3D surface plots
utils.R # Capitalization helpers, formatting
main.R # Demos and execution
```

## 📈 Sample Use Cases

- Visualize how volatility impacts option value over time.
- Examine sensitivity of a Put option's Theta across strike prices.
- Overlay Delta shifts for different expiration horizons.

## 🧠 Learn More

If you're studying for the CFA, this tool supports understanding of:
- Option pricing mechanics
- Risk sensitivities
- Real-time parameter impact
