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
### Title: Calculates the greeks of an European call- or put-option in the
###   Black Scholes model.
### Aliases: BS_European_Greeks

### ** Examples

BS_European_Greeks(initial_price = 120, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "gamma"), payoff = "put")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BS_European_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Malliavin_Asian_Greeks")
### * Malliavin_Asian_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Malliavin_Asian_Greeks
### Title: This function calculates the fair value of an European option by
###   with the Malliavin Monte Carlo Method in the Black Scholes model.
### Aliases: Malliavin_Asian_Greeks

### ** Examples

Malliavin_Asian_Greeks(initial_price = 110, exercise_price = 100,
r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
greek = c("fair_value", "delta", "rho"), payoff = "put")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Malliavin_Asian_Greeks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Malliavin_European_Greeks")
### * Malliavin_European_Greeks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Malliavin_European_Greeks
### Title: This function calculates the fair value of an European option by
###   with the Malliavin Monte Carlo Method in the Black Scholes model.
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
