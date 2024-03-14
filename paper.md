---
title: 'greeks: Sensitivities of Prices of Financial Options and Implied Volatilities'
tags:
- R
- Option Pricing
- Financial Mathematics
- Malliavin Calculus
bibliography: paper.bib
affiliations:
- name: University of Applied Sciences Koblenz, Germany
  index: 1
authors:
- name: Anselm Hudde
  orcid: "0000-0002-5652-2815"
  affiliation: 1
---

# Summary

The `greeks` R package leverages the Black-Scholes model and more general jump
diffusion models to compute sensitivities of financial option prices for
European, geometric and arithmetic Asian, as well as  American options, with
various payoff functions (for a treatment see @Hull:2022, and @Angus:1999
for the case of geometric Asian options).
The Black-Scholes model is the standard approach for modelling stock prices,
while jump diffusion models aim to offer a more realistic representation of
market movements, see @Kou:2002.
Furthermore, methods to compute implied volatilities are provided for a wide
range of option types and custom payoff functions.
Classical formulas are implemented for European options in the Black-Scholes
model, as is presented in @Hull:2022.
In the case of Asian options, Malliavin Monte Carlo Greeks are implemented, see
@Hudde:2023, or @Lyuu:2019.
For American options, the Binomial Tree method is implemented, as is presented
in @Hull:2022.
`greeks` includes a Shiny app to interactively plot the results.

# Statement of need

The accurate pricing of financial options and the computation of the Greeks,
i.e., the sensitivities of option prices with respect to the input parameters,
is of great theoretical and practical interest in finance.
For instance, the Greek $\Delta$ (Delta) measures how the price of an option
changes with a minor change in the underlying asset's price, while $\Gamma$
(Gamma) measures how $\Delta$ itself changes as the price of the underlying
shifts.
Both Greeks are important for hedging an option.
Several software packages exist, but a comprehensive framework including both
exotic options and an interactive visualization tool is still missing.

For example, a widely known program to compute Greeks is the Excel add-in
`DerivaGem` which accompanies `Options, Futures and other Derivatives`
[@Hull:2022].
`DerivaGem` computes option prices, Greeks and implied volatilities and displays
them interactively.
The model selection is restrained to the Black-Scholes and the Binomial Tree
models.
The framework `Quantlib` [@Quantlib], which is ported to R via `RQuantLib`
[@RQuantLib], provides option prices and Greeks for American options in the
Binomial Tree model, and for European and geometric Asian options in the 
Black-Scholes model.
Yet, arithmetic Asian options are not considered.

Further packages on CRAN include `derivmkts` [@derivmkts] and `OptionPricing`
[@OptionPricing].
`derivmkts` only computes Greeks for Binomial and European options.
`OptionPricing` implements very efficient algorithms for arithmetic Asian call
options, but not for put options, and only the Greeks $\Delta$ and $\Gamma$.

`greeks` is the most comprehensive R package for the computation of Greeks,
i.e., the only one for European, American, and geometric as well as arithmetic
Asian Greeks.
In addition, Asian Greeks with digital payoff functions and second-order Greeks
are computed.
Also, it is the only R package for the computation of Greeks in jump diffusion
models.

`greeks` has been applied to investigate the performance of Monte Carlo Greeks
for jump diffusion models from @Hudde:2023.
Furthermore, `greeks` can be used in (under-)graduate courses in financial
mathematics to provide a better understanding of option prices and Greeks by
interactive visualizations.
`greeks` is also suited for financial risk management purposes.

# How to run the Shiny app

The interactive Shiny app is started with `greeks::Greeks_UI()`:
\begin{center}
\includegraphics[width=0.5\textwidth]{./man/figures/GreeksUI.png}
\end{center}
This works for European options, American options, geometric Asian options, as
well as Asian options.
Due to the computational complexity required by the jump diffusion model, making
it impractical for interactive exploration, the Shiny app only supports the much
quicker-to-compute Black-Scholes model.
For the same reason, the set of Greeks is limited depending on the option type.
On the y-axis, the option value and the values of the Greeks are displayed.
For the x-axis, several parameters like `initial_price` or `time_to_maturity`
are possible choices for visualization.

# Acknowledgements

I would like to thank the Editor Charlotte Soneson and the reviewers Oskar Laverny
and Ba Hung for their continuous support in improving the package and the paper.

# References
