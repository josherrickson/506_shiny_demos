library(shiny)
library(nycflights13)
library(forestplot)

# Import and clean data
nnmaps <- read.csv("chicago-nmmaps.csv")
nnmaps$date <- as.Date(nnmaps$date)
nnmaps$month_year <- (nnmaps$year - 1997) * 12 + nnmaps$month_numeric

ui <- fluidPage(
  titlePanel("NNMaps Chicago Analysis"),

  sidebarLayout(
    sidebarPanel(
      selectInput("outcome",
                  "Choose Outcome Variable:",
                  choices = c(
                    "Oxygen" = "o3",
                    "Dewpoint" = "dewpoint",
                    "Particulates" = "pm10"
                  ),
                  selected = "o3"),

      radioButtons("time_measure",
                   "Measure of time:",
                   choices = c("Year" = "year",
                               "Month & Year" = "month_year",
                               "Season" = "season"),
                   selected = "year"),

      conditionalPanel(
        condition = "input.time_measure == 'year'",
        radioButtons("year_type",
                     "Year representation:",
                     choices = c("Categorical" = "categorical",
                                 "Continuous" = "continuous"),
                     selected = "categorical")
      )

    ),

    mainPanel(
      h4("Model Summary"),
      tableOutput("model_stats"),
      h4("Coefficient Estimates"),
      tableOutput("coef_table"),
      h4("Coefficient Plot"),
      plotOutput("coef_plot", height = "400px")
    )
  )
)

server <- function(input, output) {

  model_fit <- reactive({
    form <- as.formula({
      if (input$time_measure == "year") {
        if (input$year_type == "categorical") {
          paste(input$outcome, "~ as.factor(year)")
        } else {
          paste(input$outcome, "~ year")
        }
      } else {
        paste(input$outcome, "~", input$time_measure)
      }
    })
    form <- update(form, . ~ . + temp)

    lm(form, data = nnmaps)
  })

  output$model_stats <- renderTable({
    fit <- summary(model_fit())
    data.frame(
      Statistic = c("R-squared", "Adjusted R-squared", "Number of Observations"),
      Value = c(
        format(fit$r.squared, digits = 4),
        format(fit$adj.r.squared, digits = 4),
        format(length(residuals(model_fit())))
      )
    )
  }, rownames = FALSE, align = 'lr', width = "100%")

  output$coef_table <- renderTable({
    coef_summary <- round(summary(model_fit())$coefficients, 4)
    coef_summary[, 4] <- as.character(coef_summary[, 4])
    coef_summary[coef_summary[, 4] == "0", 4] <- "< .001"
    data.frame(
      Variable = rownames(coef_summary),
      Estimate = format(coef_summary[, "Estimate"], digits = 4),
      "Std._Error" = format(coef_summary[, "Std. Error"], digits = 4),
      "t_value" = format(coef_summary[, "t value"], digits = 4),
      "p_value" = format(coef_summary[, "Pr(>|t|)"], digits = 4)
    )
  }, rownames = FALSE, align = 'lrrrr', width = "100%", )

  output$coef_plot <- renderPlot({
    coef_summary <- summary(model_fit())$coefficients

    # Remove intercept for plot
    coef_summary <- coef_summary[-1, , drop = FALSE]

    coef_names <- rownames(coef_summary)
    estimates <- coef_summary[, "Estimate"]
    ci_lower <- estimates - 2 * coef_summary[, "Std. Error"]
    ci_upper <- estimates + 2 * coef_summary[, "Std. Error"]

    forestplot(
      labeltext = coef_names,
      mean = estimates,
      lower = ci_lower,
      upper = ci_upper,
      title = "Coefficient Estimates with 95% Confidence Intervals",
      xlab = "Coefficient Estimate",
      txt_gp = fpTxtGp(label = gpar(cex = 1.2)),
      col = fpColors(box = "royalblue",
                     lines = "darkblue",
                     zero = "gray50"),
      zero = 0,
      boxsize = 0.2,
      ci.vertices = TRUE,
      xticks = c(floor(min(ci_lower)), 0, ceiling(max(ci_upper)))
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)