#' Opens a shiny app to plot option prices and Greeks
#'
#' @export
#'
#' @import shiny
#' @import ggplot2
#' @import tibble
#' @import plotly
#' @import tidyr

Greeks_UI <- function() {

  ui <- fluidPage(
    selectInput(
      inputId = "payoff",
      label = "Payoff",
      choices = c("call", "put", "cash_or_nothing_call", "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put"),
      selected = "call",
      multiple = FALSE),
    selectInput(
      inputId = "greek",
      label = "Greek",
      choices = c("fair_value", "delta", "vega", "theta", "rho", "epsilon", "lambda",
                  "gamma", "vanna", "charm", "vomma", "veta", "speed"),
      selected = c("fair_value", "delta"),
      multiple = TRUE),
    sliderInput(
      inputId = "initial_price",
      label = "initial price",
      min = 0,
      max = 200,
      value = c(0, 200)),
    sliderInput(
      inputId = "exercise_price",
      label = "Exercise Price",
      min = 0,
      max = 200,
      value = 100),
    sliderInput(
      inputId = "r",
      label = "riskless intereset rate",
      min = -0.1,
      max = 1,
      value = 0),
    sliderInput(
      inputId = "time_to_maturity",
      label = "Time to Maturity",
      min = 0,
      max = 20,
      value = 1),
    sliderInput(
      inputId = "volatility",
      label = "Volatility",
      min = 0,
      max = 1,
      value = 0.3),
    sliderInput(
      inputId = "dividend_yield",
      label = "Dividend Yield",
      min = 0,
      max = 1,
      value = 0.02),

    plotlyOutput(outputId = "plot")
    #plotOutput("plot")
  )

  server <- function(input, output) {

    output$plot <- renderPlotly(
      {
        initial_price <- seq(
          input$initial_price[1],
          input$initial_price[2],
          by = round(max(0.01, (input$initial_price[2] - input$initial_price[1])/100), 2))

        FUN = function(x) {
          Greeks(
            initial_price = x,
            exercise_price = input$exercise_price,
            r = input$r,
            time_to_maturity = input$time_to_maturity,
            volatility = input$volatility,
            dividend_yield = input$dividend_yield,
            payoff = input$payoff,
            greek = input$greek) |>
            round(2)
        }

        if (length(input$greek) == 1) {
          Option_price <-
            tibble(
              Value = sapply(
                X = initial_price,
                FUN = FUN),
              initial_price = initial_price,
              Greek = greek)
        } else {
          Option_price <-
            sapply(
              X = matrix(initial_price),
              FUN = FUN
            ) %>%
            t() %>%
            as_tibble() %>%
            add_column(initial_price) %>%
            pivot_longer(cols = -initial_price,
                         names_to = "Greek",
                         values_to = "Value")
        }

        plot <-
          Option_price |>
          ggplot() +
          geom_line(mapping = aes(x = initial_price,
                                  y = Value,
                                  color = Greek)) +
          theme_minimal() +
          xlab("Initial Price")

        ggplotly(plot)

      }
    )
  }


  shinyApp(ui = ui, server = server, options = list(height = 1000))

}

Greeks_UI()
