#' @title
#' Opens a shiny app to interactively visualize option prices and Greeks.
#'
#' @description
#' Opens a shiny app to interactively visualize option prices and Greeks.
#' This works for European Options (see [BS_European_Greeks]), American
#' Options (see [Binomial_American_Greeks]), Geometric Asian Options (see
#' [BS_Geometric_Asian_Greeks]), as well as Asian options (see
#' [BS_Malliavin_Asian_Greeks]).
#' For performance reasons, just the Black-Scholes model is possible, and for
#' some cases, the set of Greeks is limited.
#' On the y-Axis, the option value resp. the value of the greeks are displayed,
#' for the x-axis, several parameters like `initial_price` or `time_to_maturity`
#' are possible.
#'
#' \if{latex}{
#' \figure{GreeksUI.png}{The interface of the functions}
#' }
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

  ############################################################################
  ####### first row of selection panels with 'X-Axis' and 'Option Type' ######
  ############################################################################

  ui <- fluidPage(
    fluidRow(
      # x-axis
      conditionalPanel(
        condition = ("input.option_type == 'Asian'"),
        column(
          width = 6,
          selectInput(
            inputId = "x_axis_asian",
            label = "X-axis",
            choices = c("Initial Price", "Exercise Price"),
            selected = "initial_price",
            multiple = FALSE)
        )
      ), # conditionalPanel
      conditionalPanel(
        condition = ("input.option_type != 'Asian'"),
        column(
          width = 6,
          selectInput(
            inputId = "x_axis",
            label = "X-axis",
            choices = names(params_list),
            selected = "initial_price",
            multiple = FALSE)
        )
      ), # conditionalPanel
      # option type
      column(
        width = 6,
        selectInput(
          inputId = "option_type",
          label = "Option Type",
          choices = list("European", "American", "Geometric Asian", "Asian"),
          selected = "European",
          multiple = FALSE)
      )
    ), # fluidRow

    ############################################################################
    ######### second row of selection panels with 'Greek' and 'Payoff' #########
    ############################################################################

    fluidRow(
      # greek for European Options
      conditionalPanel(
        condition = ("input.option_type == 'European'"),
        column(
          width = 6,
          selectInput(
            inputId = "greek_european",
            label = "Greek",
            choices = names(greeks_list),
            selected = c("Fair Value", "Delta"),
            multiple = TRUE)
        )
      ), # conditionalPanel
      # greek for American Options
      conditionalPanel(
        condition = ("input.option_type == 'American'"),
        column(
          width = 6,
          selectInput(
            inputId = "greek_american",
            label = "Greek",
            choices = c("Fair Value", "Delta", "Vega", "Theta", "Rho",
                        "Epsilon", "Gamma"),
            selected = c("Fair Value", "Delta"),
            multiple = TRUE)
        )
      ), # conditionalPanel
      # greek for Geometric Asian Options
      conditionalPanel(
        condition = ("input.option_type == 'Geometric Asian'"),
        column(
          width = 6,
          selectInput(
            inputId = "greek_geometric_asian",
            label = "Greek",
            choices = c("Fair Value", "Delta", "Vega", "Theta", "Rho",
                        "Epsilon", "Gamma"),
            selected = c("Fair Value", "Delta"),
            multiple = TRUE)
        )
      ), # conditionalPanel
      # greek for Geometric Asian Options
      conditionalPanel(
        condition = ("input.option_type == 'Asian'"),
        column(
          width = 6,
          selectInput(
            inputId = "greek_asian",
            label = "Greek",
            choices = c("Fair Value", "Delta", "Rho", "Vega"),
            selected = c("Fair Value", "Delta"),
            multiple = TRUE)
        )
      ), # conditionalPanel
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
      ), # conditionalPanel
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
        condition = ("((input.option_type != 'Asian') && (input.x_axis != 'Initial Price'))"),
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
      ), # conditionalPanel
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
            value = c(1, 200)
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
      ), # conditionalPanel
      # Exercise Price
      conditionalPanel(
        condition = ("input.x_axis == 'Exercise Price'"),
        column(
          width = 6,
          sliderInput(
            inputId = "exercise_price_2",
            label = "Exercise Price",
            min = 1,
            max = 200,
            value = c(1, 200)
          )
        )
      ), # conditionalPanel
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
      ), # conditionalPanel

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
      ), # conditionalPanel
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
      ), # conditionalPanel
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
      ), # conditionalPanel
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

        if (input$option_type == "European") {
          greek <- input$greek_european
        } else if (input$option_type == "American") {
          greek <- input$greek_american
        } else if (input$option_type == "Geometric Asian") {
          greek <- input$greek_geometric_asian
        } else if (input$option_type == "Asian") {
          greek <- input$greek_asian
        }

        greek <- greeks_list[greek] %>%
          unlist() %>%
          unname()

        if (input$option_type == "Asian") {
          x_bounds <-
            input[[eval(paste(params_list[[input$x_axis_asian]], "_2", sep = ""))]]
        } else {
          x_bounds <-
            input[[eval(paste(params_list[[input$x_axis]], "_2", sep = ""))]]
        }

        x_from <- x_bounds[1]
        x_to <- x_bounds[2]

        step_size <- (x_to - x_from)/200

        precision <- max(ceiling(-log(step_size, base = 10)), 4)

        step_size <- round(step_size, precision)

        x <- seq(x_from, x_to, by = step_size)

        if (input$option_type == "Asian") {
          assign(params_list[[input$x_axis_asian]], x)
        } else {
          assign(params_list[[input$x_axis]], x)
        }

        if(input$option_type == "Asian") {

          Option_price <-
            BS_Malliavin_Asian_Greeks(
              initial_price = initial_price,
              exercise_price = exercise_price,
              r = r,
              time_to_maturity = time_to_maturity,
              volatility = volatility,
              dividend_yield = dividend_yield,
              payoff = payoff,
              greek = greek
            ) |> as_tibble() |>
            add_column(x = x) |>
            pivot_longer(cols = -x,
                         names_to = "Greek",
                         values_to = "Value")
        } else {

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
              greek = greek,
              steps = 100) %>%
              round(4)
          }

          Option_price_list <-
            lapply(
              X = matrix(x),
              FUN = FUN
            )

          Option_price <-
            matrix(nrow = length(x), ncol = length(greek))

          for(row in 1:length(x)) {
            Option_price[row, ] <- round(Option_price_list[[row]], 4)
          }

          colnames(Option_price) <- greek

          Option_price <-
            Option_price %>%
            as_tibble() %>%
            add_column(x) %>%
            pivot_longer(cols = -x,
                         names_to = "Greek",
                         values_to = "Value")

        }

        colnames(Option_price) <-
          replace(colnames(Option_price), colnames(Option_price) == "x", input$x_axis)

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
