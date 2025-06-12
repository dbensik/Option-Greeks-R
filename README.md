# 📈 Option Greeks Visualizer (Shiny App)

A modular R application to visualize and explore Black-Scholes option pricing and Greek sensitivities through interactive 2D and 3D plots.

## 🔧 Features

- 📊 **2D Greek plots**: Visualize delta, gamma, theta, rho, and vega across a range of spot prices.
- 🧊 **3D Greek surfaces**: Explore how each Greek evolves across time and spot price dimensions.
- 🕵️‍♀️ **Hover Annotations**: Mouse over any 2D plot to get precise value readouts.
- 🌓 **Modern UI**: Comes with a clean, themed interface via `shinythemes`.

## 🧠 Technologies

- **R**
- **Shiny** for interactivity
- **Custom modules**: 
  - `option_pricing.R`
  - `option_greeks.R`
  - `plots_2d.R`
  - `plots_3d.R`
  - `utils.R`

## 🚀 Getting Started

1. Clone the repo:
   ```bash
   git clone https://github.com/YOUR_USERNAME/Option-Greeks-R.git
   cd Option-Greeks-R
   ```
Open app.R in RStudio.

Click Run App, or:

```r
shiny::runApp("app.R")
```

## 🧪 Dependencies
Install required packages if missing:

```r
install.packages(c("shiny", "shinythemes"))
```
## 💡 Next Steps
Add tab for side-by-side call vs put comparison.

Integrate implied volatility surfaces.

Package it with devtools and submit to CRAN (if you’re feeling spicy).

## 🙃 Disclaimer
This tool is for educational/demonstration use only. If you use it to price real options and lose all your money, that’s on you.

Made by someone who knows the Greeks aren't just a vacation destination.
