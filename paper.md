---
title: 'greeks: Sensitivities of Prices of Financial Options and Implied Volatilities'
tags:
- R
- Option Pricing
- Financial Mathematics
- Malliavin Calculus
date: "27 July 2023"
authors:
- name: Anselm Hudde
  orcid: "0000-0002-5652-2815"
  equal-contrib: yes
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: University of Applied Sciences Koblenz, Germany
  index: 1
---

# Summary

`greeks` is an R package for calculating sensitivities of financial option prices for European,
geometric and arithmetic Asian, and American options, with various
payoff functions in the Black Scholes model, and in more general jump diffusion
models.
It includes a Shiny app to interactively plot the results.
Furthermore, methods to compute implied volatilities are provided for a wide
range of option types and custom payoff functions.
Classical formulas are implemented for European options in the Black Scholes
Model, as is presented in [@Hull:2022].
In the case of Asian options, Malliavin Monte Carlo Greeks are implemented, see
[@Hudde:2023].
For American options, the Binomial Tree Method is implemented, as is presented
in [@Hull:2022].

# Statement of need

The accurate pricing of financial options and the computation of the Greeks,
i.e., the sensitivities of option prices with respect to the input parameters,
is of great theoretical and practical interest in finance.
Several software packages exist, but a comprehensive framework including both
exotic options and an interactive visualization tool is still missing.

For example, a widely known program to compute Greeks is the Excel Add-in
`DerivaGem` which accompanies `Options, Futures and other Derivatives`
[@Hull:2022].
`DerivaGem` computes option prices, Greeks and implied volatilities and displays
them interactively.
The model selection is restrained to the Black Scholes and the Binomial Tree
model.
The framework `Quantlib` [@Quantlib], which is ported to R via `RQuantLib`
[@RQuantLib], provides option prices and Greeks for American options in the
Binomial Tree Model, and for European and geometric Asian options in the Black
Scholes model.
Yet, arithmetic Asian options are not considered, and no interactive
visualization tool is provided.

Further packages on CRAN include `derivmkts` [@derivmkts] and `OptionPricing`
[@OptionPricing], both without visualizations.
`derivmkts` only computes Greeks for Binomial and European options.
`OptionPricing` implements very efficient algorithms for arithmetic Asian call
options, but not for put options, and only the Greeks $\Delta$ and $\Gamma$.

`greeks` is the most comprehensive R package for the computation of Greeks,
i.e., the only one for European, American, and geometric as well as arithmetic
Asian Greeks.
In addition, Asian Greeks with digital payoff functions and second-order Greeks
are computed.
Also, it is the only R package with included interactive visualization in a
Shiny app, and with the computation of Greeks in jump diffusion models.

`greeks` has been applied to investigate the performance of Monte Carlo Greeks
for jump diffusion Models from [@Hudde:2023].
Furthermore, `greeks` is used in graduate courses in financial mathematics to
provide a better understanding of option prices and Greeks by interactive
visualizations.
`greeks` is also suited for financial risk management purposes.

# References
