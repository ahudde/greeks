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

Greeks_UI <- function() {

  ui <- fluidPage(
    fluidRow(
      # x-axis
      column(
        width = 3,
        selectInput(
          inputId = "x_axis",
          label = "x_axis",
          choices = list(
            "Initial Price" = "initial_price",
            "Exercise Price" = "exercise_price",
            "Riskless Interest Rate" = "r",
            "Time to Maturity" = "time_to_maturity",
            "Volatility" = "volatility",
            "Dividend Yield" = "dividend_yield"),
          selected = "initial_price",
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
        condition = ("input.x_axis != 'initial_price'"),
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
        condition = ("input.x_axis == 'initial_price'"),
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
        condition = ("input.x_axis != 'exercise_price'"),
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
        condition = ("input.x_axis == 'exercise_price'"),
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
        condition = ("input.x_axis != 'r'"),
        column(
          width = 6,
          sliderInput(
            inputId = "r_1",
            label = "Riskless Interest Rate",
            min = -0.1,
            max = 1,
            value = 0
          )
        )
      ), # conditionalPanel
      # Riskless Interest Rate
      conditionalPanel(
        condition = ("input.x_axis == 'r'"),
        column(
          width = 6,
          sliderInput(
            inputId = "r_2",
            label = "Riskless Interest Rate",
            min = -0.1,
            max = 1,
            value = c(-0.1, 1)
          )
        )
      ), # condionalPanel

      # Time to Maturity
      conditionalPanel(
        condition = ("input.x_axis != 'time_to_maturity'"),
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
        condition = ("input.x_axis == 'time_to_maturity'"),
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
        condition = ("input.x_axis != 'volatility'"),
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
        condition = ("input.x_axis == 'volatility'"),
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
        condition = ("input.x_axis != 'dividend_yield'"),
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
        condition = ("input.x_axis == 'dividend_yield'"),
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

        if(input$x_axis == "initial_price") {
          initial_price <- seq(
            input$initial_price_2[1],
            input$initial_price_2[2],
            by = round(max(0.01, (input$initial_price_2[2] - input$initial_price_2[1])/100), 2))
        } else {
          initial_price <- input$initial_price_1
        }

        if(input$x_axis == "exercise_price") {
          exercise_price <- seq(
            input$exercise_price_2[1],
            input$exercise_price_2[2],
            by = round(max(0.01, (input$exercise_price_2[2] - input$exercise_price_2[1])/100), 2))
        } else {
          exercise_price <- input$exercise_price_1
        }

        if(input$x_axis == "r") {
          r <- seq(
            input$r_2[1],
            input$r_2[2],
            by = round(max(0.01, (input$r_2[2] - input$r_2[1])/100), 2))
        } else {
          r <- input$r_1
        }

        if(input$x_axis == "time_to_maturity") {
          time_to_maturity <- seq(
            input$time_to_maturity_2[1],
            input$time_to_maturity_2[2],
            by = round(max(0.01, (input$time_to_maturity_2[2] - input$time_to_maturity_2[1])/100), 2))
        } else {
          time_to_maturity <- input$time_to_maturity_1
        }

        if(input$x_axis == "volatility") {
          volatility <- seq(
            input$volatility_2[1],
            input$volatility_2[2],
            length.out = 100)
        } else {
          volatility <- input$volatility_1
        }

        if(input$x_axis == "dividend_yield") {
          dividend_yield <- seq(
            input$dividend_yield_2[1],
            input$dividend_yield_2[2],
            length.out = 100)
        } else {
          dividend_yield <- input$dividend_yield_1
        }

        FUN = function(x) {
          assign(input$x_axis, x)

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

        x <- get(input$x_axis)

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
