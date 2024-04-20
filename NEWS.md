---
output:
  html_document: default
---

# greeks 1.4.0

`greeks` is now published in JOSS:
[![status](https://joss.theoj.org/papers/61ae139acab91fdb0664b33a23e43c93/status.svg)](https://joss.theoj.org/papers/61ae139acab91fdb0664b33a23e43c93)

Hudde, A., (2024). greeks: Sensitivities of Prices of Financial Options and Implied Volatilities. Journal of Open Source Software, 9(95), 5987, https://doi.org/10.21105/joss.05987

# greeks 1.2.0

`Greeks_UI()` Added arithmetic Asian Option prices and Greeks

# greeks 1.1.0

`BS_Geometric_Asian_Greeks()` now computes prices and sensitivities of geometric
Asian options.

`Greeks_UI()` also displays Geometric Asian Options.

# greeks 1.0.0

`Greeks_UI()`: Added American Options and Greeks.

`Implied_Volatility()`: Improved performance for European Options by implementing
Halley's Method.

# greeks 0.8.1

Fixed installation problems.

# greeks 0.8.0

Added function `Greeks_UI()` which starts an interactive shiny app to display
option prices and Greeks.

# greeks 0.7.0

`BS_European_Greeks()`: Added the Greeks `zomma`, `color`, and `ultima`.

# greeks 0.6.0

`BS_European_Greeks()`: Added `cash_or_nothing` and `asset_or_nothing` payoff
function and the Greek `vera`.

Improved performance of `Implied_Volatility()` for European options in the Black
Scholes model.

# greeks 0.5.0

Added function `Implied_Volatility()` to compute implied probabilities of
various options.

# greeks 0.4.1

Removed dependency from `MatrixStats` and improved performance of
`Malliavin_Asian_Greeks()`.

# greeks 0.4.0

Added function `Greeks()` which is a wrapper to compute any option value or
Greek which is implemented in the package `greeks`.

`BS_European_Greeks()`: Added Greeks `charm`, `vomma`, `veta`, `speed`.

# greeks 0.3.0

Added function `Binomial_American_Greeks()` which computes American Option
prices and Greeks in the binomial options pricing model.

Improved performance of `Malliavin_Asian_Greeks()`.

# greeks 0.2.0

New function `Malliavin_European_Greeks()` which computes fair value and Greeks
for American Options in the Black Scholes and an Jump-Diffusion Model

Improvements in `Malliavin_Asian_Greeks()`:

  - Added Greeks Vega and Gamma
  - Implemented alternative Jump-Diffusion Model
  - performance improvements


# greeks 0.0.1

Initial Version of the package. Computes Sensitivities of Prices of Financial
Options for European and Asian Options in the Black Scholes model.

