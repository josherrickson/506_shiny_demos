library(shiny)
library(shinyFeedback)
library(ggplot2)
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
      plotOutput("coef_plot"),
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

  output$coef_plot <- renderPlot({
    df <- sim_data()

    ggplot(df, aes(x = beta1, y = beta2)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = input$true_beta2, linetype = "dashed", color = "red") +
      geom_vline(xintercept = input$true_beta1, linetype = "dashed", color = "red") +
      labs(x = expression(hat(beta)[1]),
           y = expression(hat(beta)[2]),
           title = "Estimated Coefficients Across Simulations",
           subtitle = paste("Correlation between X1 and X2 =", input$correlation)) +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
}

shinyApp(ui, server)