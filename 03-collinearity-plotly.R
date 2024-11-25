library(shiny)
library(shinyFeedback)
library(plotly)
library(MASS)  # for mvrnorm

ui <- fluidPage(
  useShinyFeedback(), # enable `feedbackWarning()` in server
  titlePanel("Multicollinearity Simulation"),

  sidebarLayout(
    sidebarPanel(
      numericInput("correlation",
                   "Correlation between X1 and X2:",
                   min = 0, max = 0.99,
                   value = 0.9,
                   step = 0.1),
      numericInput("n_sims",
                   "Number of simulations:",
                   min = 100, max = 1000,
                   value = 200),
      numericInput("sample_size",
                   "Sample size for each simulation:",
                   min = 20, max = 500,
                   value = 50),
      numericInput("true_beta1",
                   "True β1:",
                   value = 2),
      numericInput("true_beta2",
                   "True β2:",
                   value = 3),
      numericInput("noise_sd",
                   "Error term SD:",
                   min = 0.1, max = 5,
                   value = 1),
      p("This app demonstrates how multicollinearity affects coefficient estimates.
        As the correlation between predictors increases, the estimates become more unstable
        and negatively correlated with each other, even though the true relationship
        between them is zero.")
    ),

    mainPanel(
      plotlyOutput("coef_plot"),
    )
  )
)

server <- function(input, output) {

  sim_data <- reactive({

    invalidCorr <- abs(input$correlation) >= 1
    feedbackWarning("correlation", invalidCorr,
                    "Correlation must be inside (-1, 1)")
    req(!invalidCorr)

    invalidNsim <- input$n_sims < 1
    feedbackWarning("n_sims", invalidNsim,
                    "Number of simulations must be positive")
    req(!invalidNsim)

    # Correlation matrix
    sigma <- matrix(c(1, input$correlation,
                      input$correlation, 1),
                    nrow = 2)

    # Storage for results
    betas <- matrix(NA, nrow = input$n_sims, ncol = 2)

    # Run simulations
    for (i in 1:input$n_sims) {
      # Generate correlated predictors
      X <- mvrnorm(input$sample_size,
                   mu = c(0, 0),
                   Sigma = sigma)

      # Generate response
      y <- input$true_beta1 * X[,1] +
        input$true_beta2 * X[,2] +
        rnorm(input$sample_size, 0, input$noise_sd)

      # Fit model and store coefficients
      fit <- lm(y ~ X)
      betas[i,] <- coef(fit)[-1]  # exclude intercept
    }

    # Convert to data frame
    data.frame(
      beta1 = betas[,1],
      beta2 = betas[,2]
    )
  })

  output$coef_plot <- renderPlotly({
    df <- sim_data()

    vline <- function(x = 0) {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = "red")
      )
    }

    hline <- function(y = 0) {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = "red")
      )
    }

    plot_ly(df, x = ~ beta1, y = ~ beta2, type = "scatter", mode = "markers") |>
      layout(
        shapes = list(vline(input$true_beta1), hline(input$true_beta2)),
        xaxis = list(title = "β̂₁"),
        yaxis = list(title = "β̂₂"),
        title = list(
          text = paste0(
            "Estimated Coefficients Across Simulations<br>",
            "<sup>Correlation between X₁ and X₂ = ", input$correlation, "</sup>"
          )
        )
      )
  })
}

shinyApp(ui, server)