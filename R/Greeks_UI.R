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

  greeks_list <-
    list(
      "Fair Value" = "fair_value",
      "Delta" = "delta",
      "Vega" = "vega",
      "Theta" = "theta",
      "Rho" = "rho",
      "Epsilon" = "epsilon",
      "Lambda" = "lambda",
      "Gamma" = "gamma",
      "Vanna" = "vanna",
      "Charm" = "charm",
      "Vomma" = "vomma",
      "Veta" = "veta",
      "Speed" = "speed")

  params_list <-
    list(
      "Initial Price" = "initial_price",
      "Exercise Price" = "exercise_price",
      "Riskless Interest Rate" = "r",
      "Time to Maturity" = "time_to_maturity",
      "Volatility" = "volatility",
      "Dividend Yield" = "dividend_yield")

  ui <- fluidPage(
    fluidRow(
      # x-axis
      column(
        width = 6,
        selectInput(
          inputId = "x_axis",
          label = "X-axis",
          choices = names(params_list),
          selected = "initial_price",
          multiple = FALSE)
      ),
      # option type
      column(
        width = 6,
        selectInput(
          inputId = "option_type",
          label = "Option Type",
          choices = list("European", "American"),
          selected = "European",
          multiple = FALSE)
      )
      ), # fluidRow
    fluidRow(
      # greek
      column(
        width = 6,
        selectInput(
          inputId = "greek",
          label = "Greek",
          choices = names(greeks_list),
          selected = c("Fair Value", "Delta"),
          multiple = TRUE)
      ),
      # payoff
      conditionalPanel(
        condition = ("input.option_type == 'European'"),
        column(
          width = 6,
          selectInput(
            inputId = "payoff_european",
            label = "Payoff",
            choices = list(
              "Call" = "call",
              "Put" = "put",
              "Cash or nothing Call" = "cash_or_nothing_call",
              "Cash or nothing Put" = "cash_or_nothing_put",
              "Asset or nothing Call" = "asset_or_nothing_call",
              "Asset or nothing Put" =  "asset_or_nothing_put"),
            selected = "call",
            multiple = FALSE)
        )
      ), # contionalPanel
      # payoff
      conditionalPanel(
        condition = ("input.option_type != 'European'"),
        column(
          width = 6,
          selectInput(
            inputId = "payoff",
            label = "Payoff",
            choices = list(
              "Call" = "call",
              "Put" = "put"),
            selected = "call",
            multiple = FALSE)
        )
      ) # conditionalPanel
    ), # fluidRow

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
            inputId = "initial_price",
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
            inputId = "exercise_price",
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
            inputId = "r",
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
        condition = ("input.x_axis != 'Time to Maturity'"),
        column(
          width = 6,
          sliderInput(
            inputId = "time_to_maturity",
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
    ##### third row of slider panels with 'Volatility' and 'Dividend Yield #####
    ############################################################################

    fluidRow(

      # Volatility
      conditionalPanel(
        condition = ("input.x_axis != 'Volatility'"),
        column(
          width = 6,
          sliderInput(
            inputId = "volatility",
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
            inputId = "dividend_yield",
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
  )

  server <- function(input, output) {

    output$plot <- renderPlotly(
      {

        ## The parameters for the computation of the Greeks are extracted from
        ## input

        initial_price <- input$initial_price
        exercise_price <- input$exercise_price
        r <- input$r
        time_to_maturity <- input$time_to_maturity
        volatility <- input$volatility
        dividend_yield <- input$dividend_yield

        payoff <-
          ifelse(input$option_type == "European",
                 payoff <- input$payoff_european,
                 payoff <- input$payoff)

        x_bounds <- input[[eval(paste(params_list[[input$x_axis]], "_2", sep = ""))]]

        x_from <- x_bounds[1]
        x_to <- x_bounds[2]

        step_size <- (x_to - x_from)/200

        precision <- max(ceiling(-log(step_size, base = 10)), 4)

        step_size <- round(step_size, precision)

        x <- seq(x_from, x_to, by = step_size)

        ## FUN is the function applied to sapply that compute the Greeks

        FUN <- function(x) {
          assign(params_list[[input$x_axis]], x)

          Greeks(
            initial_price = initial_price,
            exercise_price = exercise_price,
            r = r,
            time_to_maturity = time_to_maturity,
            volatility = volatility,
            dividend_yield = dividend_yield,
            option_type = input$option_type,
            payoff = payoff,
            greek = greeks_list[input$greek],
            steps = 100) %>%
            round(4)
        }

        if (length(input$greek) == 1) {
          Option_price <-
            tibble(
              x,
              Value = sapply(
                X = x,
                FUN = FUN),
              Greek = input$greek)

          colnames(Option_price)[1] <- input$x_axis

        } else {

          Option_price <-
            sapply(
              X = matrix(x),
              FUN = FUN
            ) %>%
            t() %>%
            as_tibble()

          colnames(Option_price) <- input$greek

          Option_price <-
            Option_price %>%
            add_column(x, .before = 1) %>%
            pivot_longer(cols = -x,
                         names_to = "Greek",
                         values_to = "Value")

          colnames(Option_price)[1] <- input$x_axis

        }

        plot <-
          Option_price %>%
          ggplot() +
          geom_line(mapping = aes(x = .data[[input$x_axis]],
                                  y = .data$Value,
                                  color = .data$Greek)) +
          theme_minimal() +
          xlab(input$x_axis) +
          ggtitle(paste("Prices and Sensitivites of", input$option_type, "Options"))

        ggplotly(plot)

      }
    )
  }

  shinyApp(ui = ui, server = server, options = list(height = 1000))

}
