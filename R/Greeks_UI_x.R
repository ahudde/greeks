#' Opens a shiny app to plot option prices and Greeks
#'
#' @export
#'
#' @import "shiny"
#' @import "ggplot2"
#' @import "tibble"
#' @import "tidyr"
#' @importFrom "plotly" "renderPlotly" "ggplotly" "plotlyOutput"
#'

Greeks_UI_x <- function() {

  ui <- fluidPage(
    fluidRow(
      # x-axis
      column(
        width = 3,
        selectInput(
          inputId = "x_axis",
          label = "x_axis",
          choices = c(
            "Initial Price", "Exercise Price", "Riskless Interest Rate",
            "Time to Maturity", "Volatility", "Dividend Yield"),
          selected = "Initial Price",
          multiple = FALSE)
      ),
      # payoff
      column(
        width = 3,
        selectInput(
          inputId = "payoff",
          label = "Payoff",
          choices = c("call", "put", "cash_or_nothing_call", "cash_or_nothing_put",
                      "asset_or_nothing_call", "asset_or_nothing_put"),
          selected = "call",
          multiple = FALSE)
      ),
      # greek
      column(
        width = 9,
        selectInput(
          inputId = "greek",
          label = "Greek",
          choices = c("fair_value", "delta", "vega", "theta", "rho", "epsilon",
                      "lambda", "gamma", "vanna", "charm", "vomma", "veta", "speed"),
          selected = c("fair_value", "delta"),
          multiple = TRUE)
      )
    ),

    ############################################################################
    ### first row of slider panels with 'Initial Price' and 'Exercise Price' ###
    ############################################################################

    fluidRow(

      # Initial Price
      conditionalPanel(
        condition = ("input.x_axis != 'Initial Price'"),
        column(
          width = 6,
          sliderInput(
            inputId = "initial_price_1",
            label = "Initial Price",
            min = 0,
            max = 200,
            value = 100
          )
        )
      ), # condionalPanel
      # Initial Price
      conditionalPanel(
        condition = ("input.x_axis == 'Initial Price'"),
        column(
          width = 6,
          sliderInput(
            inputId = "initial_price_2",
            label = "Initial Price",
            min = 0,
            max = 200,
            value = c(0, 200)
          )
        )
      ), # condionalPanel

      # Exercise Price
      conditionalPanel(
        condition = ("input.x_axis != 'Exercise Price'"),
        column(
          width = 6,
          sliderInput(
            inputId = "exercise_price_1",
            label = "Exercise Price",
            min = 0,
            max = 200,
            value = 100
          )
        )
      ), # condionalPanel
      # Exercise Price
      conditionalPanel(
        condition = ("input.x_axis == 'Exercise Price'"),
        column(
          width = 6,
          sliderInput(
            inputId = "exercise_price_2",
            label = "Exercise Price",
            min = 0,
            max = 200,
            value = c(0, 200)
          )
        )
      ), # condionalPanel
    ), # fluidRow

    ############################################################################
    ######### second row of slider panels with 'Riskless Interest Rate' ########
    ########################## and 'Time to Maturity' ##########################
    ############################################################################

    fluidRow(

      # Riskless Interest Rate
      conditionalPanel(
        condition = ("input.x_axis != 'Riskless Interest Rate'"),
        column(
          width = 6,
          sliderInput(
            inputId = "riskless_interest_rate_1",
            label = "Riskless Interest Rate",
            min = -0.1,
            max = 1,
            value = 0
          )
        )
      ), # conditionalPanel
      # Riskless Interest Rate
      conditionalPanel(
        condition = ("input.x_axis == 'Riskless Interest Rate'"),
        column(
          width = 6,
          sliderInput(
            inputId = "riskless_interest_rate_2",
            label = "Riskless Interest Rate",
            min = -0.1,
            max = 1,
            value = c(-0.1, 1)
          )
        )
      ), # condionalPanel

      # Time to Maturity
      conditionalPanel(
        condition = ("input.x_axis != 'Time to Maturity'"),
        column(
          width = 6,
          sliderInput(
            inputId = "time_to_maturity_1",
            label = "Time to Maturity",
            min = 0,
            max = 20,
            value = 1,
            step = 0.1
          )
        )
      ), # condionalPanel
      # Time to Maturity
      conditionalPanel(
        condition = ("input.x_axis == 'Time to Maturity'"),
        column(
          width = 6,
          sliderInput(
            inputId = "time_to_maturity_2",
            label = "Time to Maturity",
            min = 0,
            max = 20,
            value = c(0, 20),
            step = 0.1
          )
        )
      ) # conditionalPanel
    ), # fluidRow

    ############################################################################
    ###### third row of slider panels with 'Volatility' and 'Dividend Yield ####
    ############################################################################

    fluidRow(

      # Volatility
      conditionalPanel(
        condition = ("input.x_axis != 'Volatility'"),
        column(
          width = 6,
          sliderInput(
            inputId = "volatility_1",
            label = "Volatility",
            min = 0,
            max = 1,
            value = 0.3
          )
        )
      ), # condionalPanel
      # Volatility
      conditionalPanel(
        condition = ("input.x_axis == 'Volatility'"),
        column(
          width = 6,
          sliderInput(
            inputId = "volatility_2",
            label = "Volatility",
            min = 0.01,
            max = 1,
            value = c(0.01, 1)
          )
        )
      ), # conditionalPanel

      # Dividend Yield
      conditionalPanel(
        condition = ("input.x_axis != 'Dividend Yield'"),
        column(
          width = 6,
          sliderInput(
            inputId = "dividend_yield_1",
            label = "Dividend Yield",
            min = 0,
            max = 1,
            value = 0
          )
        )
      ), # condionalPanel
      # Dividend Yield
      conditionalPanel(
        condition = ("input.x_axis == 'Dividend Yield'"),
        column(
          width = 6,
          sliderInput(
            inputId = "dividend_yield_2",
            label = "Dividend Yield",
            min = 0,
            max = 1,
            value = c(0, 1)
          )
        )
      ) # conditionalPanel
      ), # fluidRow

    plotlyOutput(outputId = "plot")
    #plotOutput("plot")
  )

  server <- function(input, output) {

    output$plot <- renderPlotly(
      {

        if(input$x_axis == "Initial Price") {
          initial_price <- seq(
            input$initial_price_2[1],
            input$initial_price_2[2],
            by = round(max(0.01, (input$initial_price_2[2] - input$initial_price_2[1])/100), 2))
        } else {
          initial_price <- input$initial_price_1
        }

        if(input$x_axis == "Exercise Price") {
          exercise_price <- seq(
            input$exercise_price_2[1],
            input$exercise_price_2[2],
            by = round(max(0.01, (input$exercise_price_2[2] - input$exercise_price_2[1])/100), 2))
        } else {
          exercise_price <- input$exercise_price_1
        }

        if(input$x_axis == "Riskless Interest Rate") {
          r <- seq(
            input$riskless_interest_rate_2[1],
            input$riskless_interest_rate_2[2],
            by = round(max(0.01, (input$riskless_interest_rate_2[2] - input$riskless_interest_rate_2[1])/100), 2))
        } else {
          r <- input$riskless_interest_rate_1
        }

        if(input$x_axis == "Time to Maturity") {
          time_to_maturity <- seq(
            input$time_to_maturity_2[1],
            input$time_to_maturity_2[2],
            by = round(max(0.01, (input$time_to_maturity_2[2] - input$time_to_maturity_2[1])/100), 2))
        } else {
          time_to_maturity <- input$time_to_maturity_1
        }

        if(input$x_axis == "Volatility") {
          volatility <- seq(
            input$volatility_2[1],
            input$volatility_2[2],
            length.out = 100)
        } else {
          volatility <- input$volatility_1
        }

        if(input$x_axis == "Dividend Yield") {
          dividend_yield <- seq(
            input$dividend_yield_2[1],
            input$dividend_yield_2[2],
            length.out = 100)
        } else {
          dividend_yield <- input$dividend_yield_1
        }

        FUN = function(x) {
          if (input$x_axis == "Initial Price") {
            initial_price <- x
          } else if (input$x_axis == "Exercise Price") {
            exercise_price <- x
          } else if (input$x_axis == "Riskless Interest Rate") {
            r <- x
          } else if (input$x_axis == "Time to Maturity") {
            time_to_maturity <- x
          } else if (input$x_axis == "Volatility") {
            volatility <- x
          } else if (input$x_axis == "Dividend Yield") {
            dividend_yield <- x
          }

          Greeks(
            initial_price = initial_price,
            exercise_price = exercise_price,
            r = r,
            time_to_maturity = time_to_maturity,
            volatility = volatility,
            dividend_yield = dividend_yield,
            payoff = input$payoff,
            greek = input$greek) %>%
            round(4)
        }

        if (input$x_axis == "Initial Price") {
          x <- initial_price
        } else if (input$x_axis == "Exercise Price") {
          x <- exercise_price
        } else if (input$x_axis == "Riskless Interest Rate") {
          x <- r
        } else if (input$x_axis == "Time to Maturity") {
          x <- time_to_maturity
        } else if (input$x_axis == "Volatility") {
          x <- volatility
        } else if (input$x_axis == "Dividend Yield") {
          x <- dividend_yield
        }

        if (length(input$greek) == 1) {
          Option_price <-
            tibble(
              Value = sapply(
                X = x,
                FUN = FUN),
              x,
              Greek = input$greek)
        } else {
          Option_price <-
            sapply(
              X = matrix(x),
              FUN = FUN
            ) %>%
            t() %>%
            as_tibble() %>%
            add_column(x) %>%
            pivot_longer(cols = -x,
                         names_to = "Greek",
                         values_to = "Value")
        }

        plot <-
          Option_price %>%
          ggplot() +
          geom_line(mapping = aes(x = .data$x,
                                  y = .data$Value,
                                  color = .data$Greek)) +
          theme_minimal() +
          xlab(input$x_axis) +
          ggtitle("Prices and Sensitivites of European Options")

        ggplotly(plot)

      }
    )
  }

  shinyApp(ui = ui, server = server, options = list(height = 1000))

}

Greeks_UI_x()
