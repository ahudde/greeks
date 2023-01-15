pkgname <- "greeks"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "greeks-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('greeks')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BS_European_Greeks")
### * BS_European_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BS_European_Greeks
### Title: Computes the Greeks of an European call- or put-option, or of
###   digital options in the Black Scholes model
### Aliases: BS_European_Greeks

### ** Examples

BS_European_Greeks(initial_price = 120, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "gamma"), payoff = "put")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BS_European_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BS_Implied_Volatility")
### * BS_Implied_Volatility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BS_Implied_Volatility
### Title: Computes the implied volatility for European-, American- and
###   Asian options.
### Aliases: BS_Implied_Volatility

### ** Examples

BS_Implied_Volatility(option_price = 27, initial_price = 100,
exercise_price = 100, r = 0.03, time_to_maturity = 5, dividend_yield = 0.015,
payoff = "call")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BS_Implied_Volatility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Binomial_American_Greeks")
### * Binomial_American_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Binomial_American_Greeks
### Title: Computes the Greeks of an American call- or put-option with the
###   Binomial options pricing model
### Aliases: Binomial_American_Greeks

### ** Examples

Binomial_American_Greeks(initial_price = 100, exercise_price = 100,
r = 0, time_to_maturity = 1, volatility = 0.3, dividend_yield = 0,
payoff = "call", greek = c("fair_value", "delta", "vega", "theta", "rho",
"epsilon", "gamma"), steps = 20)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Binomial_American_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Implied_Volatility")
### * Implied_Volatility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Implied_Volatility
### Title: Computes the implied volatility for various options via Newton's
###   method
### Aliases: Implied_Volatility

### ** Examples

Implied_Volatility(15, r = 0.05, option_type = "Asian",
payoff = "call")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Implied_Volatility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Malliavin_Asian_Greeks")
### * Malliavin_Asian_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Malliavin_Asian_Greeks
### Title: Computes the Greeks of an Asian option with the Malliavin Monte
###   Carlo Method in the Black Scholes model
### Aliases: Malliavin_Asian_Greeks

### ** Examples

Malliavin_Asian_Greeks(initial_price = 110, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "rho"), payoff = "put")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Malliavin_Asian_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Malliavin_Asian_Greeks_Black_Scholes")
### * Malliavin_Asian_Greeks_Black_Scholes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Malliavin_Asian_Greeks_Black_Scholes
### Title: Computes the Greeks of an Asian option with the Malliavin Monte
###   Carlo Method in the Black Scholes model
### Aliases: Malliavin_Asian_Greeks_Black_Scholes

### ** Examples

Malliavin_Asian_Greeks(initial_price = 110, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "rho"), payoff = "put")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Malliavin_Asian_Greeks_Black_Scholes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Malliavin_European_Greeks")
### * Malliavin_European_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Malliavin_European_Greeks
### Title: Computes the Greeks of an European option with the Malliavin
###   Monte Carlo Method in the Black Scholes model
### Aliases: Malliavin_European_Greeks

### ** Examples

Malliavin_European_Greeks(initial_price = 110, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "rho"), payoff = "put")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Malliavin_European_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
