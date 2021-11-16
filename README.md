# greeks
![](https://cranlogs.r-pkg.org/badges/greeks?color=brightgreen)
![](https://cranlogs.r-pkg.org/badges/grand-total/greeks?color=brightgreen)
![](https://www.r-pkg.org/badges/version-ago/greeks)

The package greeks provides functions to calculate sensitivities of financial option prices for European, Asian, American and Digital options in the Black Scholes model, and in more general jump diffusion models.
Classical formulas are implemented for European options in the Black Scholes Model. 
Furthermore, functions to calculate Malliavin Monte Carlo Greeks are given, as presented e.g., in Hudde, A. & Rüschendorf, L. (2016). 
European and Asian Greeks for exponential Lévy processes (https://arxiv.org/abs/1603.00920).
These functions work for classical payoff functions, as well as for any custom square integrable function provided by the user.
Additionally, these calculations are not restricted to the Black Scholes model, but work for more general Lévy Jump diffusion model, which is also customizable by the user.

## Installation
```{r }
# The cran version can be installed by 
install.packages("greeks")
# The development version can be installed by
install.packages("devtools")
library("devtools")
devtools::install_github("anselmhudde/greeks")
```

## How to start

Most of the Greeks can easily can calculated with the function Greeks.

    # Load package
    library(greeks)
    # Option price and most common Greeks of a call option on a share with price 100
    # and with exercise price 120, riskless interest rate of 1%, and volatility of
    # 30%.

    Greeks(initial_price = 100,
           exercise_price = 120,
           r = 0.01,
           time_to_maturity = 5,
           volatility = 0.30,
           payoff = "call")

    ##    fair_value         delta          vega         theta           rho 
    ##  21.577149923   0.554941778  88.358901748  -2.989937331 169.585139380 
    ##         gamma 
    ##   0.005890593
    
    
