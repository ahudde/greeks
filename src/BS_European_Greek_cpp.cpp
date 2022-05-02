#include <Rcpp.h>
#include <string>

using std::string;

using namespace Rcpp;

//using R::dnorm;
// using R::pnorm;

// NumericVector BS_European_Greeks(
//     double initial_price = 100,
//     double exercise_price = 100,
//     double r = 0,
//     double time_to_maturity = 1,
//     double volatility = 0.3,
//     double dividend_yield = 0,
//     std::string payoff = "call",
//     CharacterVector greeks = {"fair_value", "delta", "vega", "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma", "veta", "speed"}) {
//
//     NumericVector result =
//       NumericVector::create(_["fair_value"], _["delta"], _["vega"], _["theta"], _["rho"], _["epsilon"], _["lambda"], _["gamma"], _["vanna"], _["charm"], _["vomma"], _["veta"], _["speed"]));


//
//     names(result) <- greek
//
//     d1 <- (log(initial_price/exercise_price) +
//       (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
//         (volatility * sqrt(time_to_maturity))
//
//     d2 <- d1 - volatility * sqrt(time_to_maturity)
//
//     if (payoff == "call") {
//
// // option-value
//
//       if ("fair_value" %in% greek) {
//         result["fair_value"] <-
//           initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
//           exp(-r*time_to_maturity) * exercise_price * pnorm(d2)
//       }
//
// // first-order Greeks
//
//       if ("delta" %in% greek) {
//         result["delta"] <- exp(-dividend_yield*time_to_maturity) * pnorm(d1)
//       }
//       if ("vega" %in% greek) {
//         result["vega"] <-
//           initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity)
//       }
//       if ("theta" %in% greek) {
//         result["theta"] <-
//           -initial_price*dnorm(d1)*volatility*
//           exp(-dividend_yield*time_to_maturity)/(2*sqrt(time_to_maturity)) +
//           dividend_yield*initial_price*pnorm(d1)*
//           exp(-dividend_yield*time_to_maturity) -
//           r*exercise_price*exp(-r*time_to_maturity)*pnorm(d2)
//       }
//       if ("rho" %in% greek) {
//         result["rho"] <-
//           exercise_price*time_to_maturity*exp(-r*time_to_maturity)*pnorm(d2)
//       }
//       if ("epsilon" %in% greek) {
//         result["epsilon"] <-
//           -initial_price*time_to_maturity*exp(-dividend_yield*time_to_maturity)*
//           pnorm(d1)
//       }
//       if ("lambda" %in% greek) {
//         result["lambda"] <-
//           initial_price * (exp(-dividend_yield*time_to_maturity) * pnorm(d1)) /
//             (initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
//               exp(-r*time_to_maturity) * exercise_price * pnorm(d2))
//       }
//
// // second-order Greeks
//
//       if ("gamma" %in% greek) {
//         result["gamma"] <-
//           dnorm(d1)*exp(-dividend_yield*time_to_maturity)/
//             (initial_price*volatility*sqrt(time_to_maturity))
//       }
//       if ("vanna" %in% greek) {
//         result["vanna"] <-
//           -exp(-dividend_yield*time_to_maturity)*dnorm(d1)*d2/volatility
//       }
//       if ("charm" %in% greek) {
//         result["charm"] <-
//           (dividend_yield * exp(-dividend_yield * time_to_maturity) * pnorm(d1)) -
//           (exp(-dividend_yield * time_to_maturity) * dnorm(d1) *
//           (2 * (r - dividend_yield) * time_to_maturity -
//           d2 * volatility * sqrt(time_to_maturity))/(2 * time_to_maturity * volatility * sqrt(time_to_maturity)))
//       }
//       if ("vomma" %in% greek) {
//         result["vomma"] <-
//           initial_price * exp(-dividend_yield  * time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity) * d1 * d2 / volatility
//       }
//       if ("veta" %in% greek) {
//         result["veta"] <-
//           -initial_price * exp(-dividend_yield * time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity) *
//           (dividend_yield + ((r - dividend_yield) * d1) / (volatility * sqrt(time_to_maturity))
//              - ((1 + d1 * d2) / (2 * T)))
//       }
//
// // third-order Greeks
//
//       if("speed" %in% greek) {
//         result["speed"] <-  -exp(-dividend_yield  * time_to_maturity) * dnorm(d1) /
//           (initial_price^2  * volatility * sqrt(time_to_maturity)) *
//             (d1 / (volatility * sqrt(time_to_maturity)) + 1)
//       }
//
//       return(result)
//
//     }
//     if(payoff == "put") {
//
// // option-value
//
//       if("fair_value" %in% greek) {
//         result["fair_value"] <-
//           exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
//           initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)
//       }
//
// // first-order Greeks
//
//       if("delta" %in% greek) {
//         result["delta"] <- -exp(-dividend_yield*time_to_maturity) * pnorm(-d1)
//       }
//       if("vega" %in% greek) {
//         result["vega"] <-
//           initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity)
//       }
//       if("theta" %in% greek) {
//         result["theta"] <-
//           -initial_price*dnorm(d1)*volatility*
//           exp(-dividend_yield*time_to_maturity)/(2*sqrt(time_to_maturity)) -
//           dividend_yield*initial_price*pnorm(-d1)*
//           exp(-dividend_yield*time_to_maturity) +
//           r*exercise_price*exp(-r*time_to_maturity)*pnorm(-d2)
//       }
//       if("rho" %in% greek) {
//         result["rho"] <-
//           -exercise_price*time_to_maturity*exp(-r*time_to_maturity)*pnorm(-d2)
//       }
//       if("epsilon" %in% greek) {
//         result["epsilon"] <-
//           initial_price*time_to_maturity*exp(-dividend_yield*time_to_maturity)*
//           pnorm(-d1)
//       }
//       if("lambda" %in% greek) {
//         result["lambda"] <-
//           initial_price * (exp(-dividend_yield*time_to_maturity) * pnorm(d1)) /
//             (exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
//               initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1))
//       }
//
// // second-order Greeks
//
//       if("gamma" %in% greek) {
//         result["gamma"] <-
//           dnorm(d1)*exp(-dividend_yield*time_to_maturity)/
//             (initial_price*volatility*sqrt(time_to_maturity))
//       }
//       if("vanna" %in% greek) {
//         result["vanna"] <-
//           -exp(-dividend_yield*time_to_maturity)*dnorm(d1)*d2/volatility
//       }
//       if("charm" %in% greek) {
//         result["charm"] <-
//           (-dividend_yield * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)) -
//           (exp(-dividend_yield * time_to_maturity) * dnorm(d1) *
//           (2 * (r - dividend_yield) * time_to_maturity -
//           d2 * volatility * sqrt(time_to_maturity))/(2 * time_to_maturity * volatility * sqrt(time_to_maturity)))
//       }
//       if("vomma" %in% greek) {
//         result["vomma"] <-
//           initial_price * exp(-dividend_yield  * time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity) * d1 * d2 / volatility
//       }
//       if("veta" %in% greek) {
//         result["veta"] <-
//           -initial_price * exp(-dividend_yield * time_to_maturity) * dnorm(d1) *
//           sqrt(time_to_maturity) *
//           (dividend_yield + ((r - dividend_yield) * d1) / (volatility * sqrt(time_to_maturity))
//              - ((1 + d1 * d2) / (2 * time_to_maturity)))
//       }
//
// // third-order Greeks
//
//       if("speed" %in% greek) {
//         result["speed"] <-  -exp(-dividend_yield  * time_to_maturity) * dnorm(d1) /
//           (initial_price^2  * volatility * sqrt(time_to_maturity)) *
//             (d1 / (volatility * sqrt(time_to_maturity)) + 1)
//       }
//
//       return(result)
//
//     }
//
//     if(payoff %in% c("cash_or_nothing_call", "cash_or_nothing_put",
//                      "asset_or_nothing_call", "asset_or_nothing_put")) {
//
//       if(payoff == "cash_or_nothing_call") {
//         fair_value <-
//           expression(
//             exp(-r * time_to_maturity) *
//               pnorm((
//                   log(initial_price / exercise_price) +
//                     (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
//               ) / (volatility * sqrt(time_to_maturity)) - volatility * sqrt(time_to_maturity)))
//       }
//
//       if(payoff == "cash_or_nothing_put") {
//         fair_value <-
//           expression(
//             exp(-r * time_to_maturity) *
//               pnorm(-(
//                   log(initial_price / exercise_price) +
//                     (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
//               ) / (volatility * sqrt(time_to_maturity)) - volatility * sqrt(time_to_maturity)))
//       }
//
//       if(payoff == "asset_or_nothing_call") {
//         fair_value <-
//           expression(
//             initial_price *
//               exp(-dividend_yield * time_to_maturity) *
//               pnorm(pnorm((log(initial_price/exercise_price) +
//               (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
//                 (volatility * sqrt(time_to_maturity)))))
//       }
//
//       if(payoff == "asset_or_nothing_put") {
//         fair_value <-
//           expression(
//             initial_price *
//               exp(-r * time_to_maturity) *
//               pnorm(-pnorm((log(initial_price/exercise_price) +
//               (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
//                 (volatility * sqrt(time_to_maturity)))))
//       }
//
// // option-value
//
//       if("fair_value" %in% greek) {
//         result["fair_value"] <-
//           eval(fair_value)
//       }
//
// // first-order Greeks
//
//       if("delta" %in% greek) {
//         result["delta"] <-
//           eval(D(fair_value, "initial_price"))
//       }
//
//       if("vega" %in% greek) {
//         result["vega"] <-
//           eval(D(fair_value, "volatility"))
//       }
//
//       if("theta" %in% greek) {
//         result["theta"] <-
//           -eval(D(fair_value, "time_to_maturity"))
//       }
//
//       if("rho" %in% greek) {
//         result["rho"] <-
//           eval(D(fair_value, "r"))
//       }
//
//       if("epsilon" %in% greek) {
//         result["epsilon"] <-
//           eval(D(fair_value, "dividend_yield"))
//       }
//
//       if("lambda" %in% greek) {
//         result["lambda"] <-
//           eval(D(fair_value, "initial_price")) * initial_price / eval(fair_value)
//       }
//
// // second-order Greeks
//
//       if("gamma" %in% greek) {
//         result["gamma"] <-
//           eval(D(D(fair_value, "initial_price"), "initial_price"))
//       }
//
//       if("vanna" %in% greek) {
//         result["vanna"] <-
//           eval(D(D(fair_value, "volatility"), "initial_price"))
//       }
//
//       if("charm" %in% greek) {
//         result["charm"] <-
//           eval(D(D(fair_value, "time_to_maturity"), "initial_price"))
//       }
//
//       if("vomma" %in% greek) {
//         result["vomma"] <-
//           eval(D(D(fair_value, "volatility"), "volatility"))
//       }
//
//       if("veta" %in% greek) {
//         result["veta"] <-
//           eval(D(D(fair_value, "time_to_maturity"), "volatility"))
//       }
//
// // third-order Greeks
//
//       if("speed" %in% greek) {
//         result["speed"] <-
//           eval(D(D(D(fair_value, "initial_price"), "initial_price"), "initial_price"))
//       }
//
//
//
//     }
// return result;
//   }
