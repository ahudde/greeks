---
output:
  html_document: default
---

## Greeks 1.2.0

`Greeks_UI()` Added arithmetic Asian Option prices and Greeks

# Greeks 1.1.0

`BS_Geometric_Asian_Greeks()` now computes prices and sensitivities of geometric
Asian options.

`Greeks_UI()` also displays Geometric Asian Options.

# Greeks 1.0.0

`Greeks_UI()`: Added American Options and Greeks.

`Implied_Volatility()`: Improved performance for European Options by implementing
Halley's Method.

# Greeks 0.8.1

Fixed installation problems.

# Greeks 0.8.0

Added function `Greeks_UI()` which starts an interactive shiny app to display
option prices and Greeks.

# Greeks 0.7.0

`BS_European_Greeks()`: Added the Greeks `zomma`, `color`, and `ultima`.

# Greeks 0.6.0

`BS_European_Greeks()`: Added `cash_or_nothing` and `asset_or_nothing` payoff
function and the Greek `vera`.

Improved performance of `Implied_Volatility()` for European options in the Black
Scholes model.

# Greeks 0.5.0

Added function `Implied_Volatility()` to compute implied probabilities of
various options.

# Greeks 0.4.1

Removed dependency from `MatrixStats` and improved performance of
`Malliavin_Asian_Greeks()`.

# Greeks 0.4.0

Added function `Greeks()` which is a wrapper to compute any option value or
Greek which is implemented in the package `greeks`.

`BS_European_Greeks()`: Added Greeks `charm`, `vomma`, `veta`, `speed`.

# Greeks 0.3.0

Added function `Binomial_American_Greeks()` which computes American Option
prices and Greeks in the binomial options pricing model.

Improved performance of `Malliavin_Asian_Greeks()`.

# Greeks 0.2.0

New function `Malliavin_European_Greeks()` which computes fair value and Greeks
for American Options in the Black Scholes and an Jump-Diffusion Model

Improvements in `Malliavin_Asian_Greeks()`:

  - Added Greeks Vega and Gamma
  - Implemented alternative Jump-Diffusion Model
  - performance improvements


# Greeks 0.0.1

Initial Version of the package. Computes Sensitivities of Prices of Financial
Options for European and Asian Options in the Black Scholes model.

