library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("False Discovery Rate procedures"),

  sidebarLayout(
    sidebarPanel(
      p("To control the False Discover Rate across a series of tests,
        enter here a list of p-values. Additionally, you may choose your
        alpha parameter, which is the proportion of false positives
        among all tests performed."),
      textInput(inputId = "pvalues",
                label = "p-values:",
                value = ".01, .05, .03, .2, .001"),
      p("(Non-numeric inputs and p-values outside of (0,1] are ignored.)"),
      numericInput("alpha",
                   label = "False Discovery Rate (alpha):",
                   min = 0, max = 1,
                   value = ".05",
                   step = .05),
      p("Which procedure should be run? Benjamini-Yekutieli is more
        conservative, but is appropriate if the tests generating the p-values
        are not independent."),
      radioButtons(inputId = "procedure",
                   label = "",
                   choices = list("Benjamini-Hochberg" = "hoch",
                                  "Benjamini-Yekutieli" = "yeku"),
                   inline = TRUE),
      br(),
      br(),
      p("By checking the box below, the output will be sorted by
        p-value. This may make it more clear what the B-H procedure
        is doing (especially in the plot). This is for display purposes
        only and does not affect the calculations."),
      checkboxInput(inputId = "sorted",
                    label = "Sort p-values?",
                    value = FALSE),
      br(),
      p("The Benjamini-Hochberg procedure works by first sorting
        all p-values in order from smallest to largest and giving
        them ranks (e.g. the first has rank 1, the second rank 2, etc).
        For each p-value, a critical value is calculated as
        alpha*rank/(# tests). The largest p-value for which this
        critical value is greater than the p-value is identified,
        and all p-values from the smallest through the identified
        p-value are determined to be statistically significant."),
      br(),
      p("The Benjamini-Yekutieli procedure works in a similar fashion, but
      the critical value is scaled down by an additional factor which is a
      function of the number of p-values calculated. The Benjamini-Hochberg
        assumes that the tests are independent of each other; whereas
        Benjamini-Yekutieli is appropriate if that assumption is not valid.")

    ),

    mainPanel(
      tableOutput("result"),
      p("In this plot, the p-values are represented by the points (values on
        the x-axis correspond to Row No. in the above table). The line
        represents the Benjamini-Hochberg critical values. The largest
        p-value at or below the line and all p-values smaller than that
        one are deemed to be significant. (Much more visually obvious if
        the \"Sort p-values\" option is checked.)"),
      plotOutput("plot"),
      p("It can occur that a p-value smaller than the largest p-value
        below the critical value is itself larger than its own critical
        value. This does not represent the problem, nor make the p-value
        fail to reject.")
    )
  )
)

server <- function(input, output) {

  process_pvalues <- reactive({
    pv <- input$pvalues
    if (pv == "") {
      return(data.frame(pv = numeric(),
                        row = numeric(),
                        rank = numeric(),
                        critvals = numeric(),
                        signif = logical()))
    }

    pv <- as.numeric(strsplit(pv, split = ",[:blank:]*")[[1]])
    if (any(!is.na(pv) & pv <= 0) | any(!is.na(pv) & pv > 1)) {
      pv[(pv >  0) & (pv <= 1)] <- NA
    }
    pv <- pv[!is.na(pv)]

    m <- length(pv)

    outtab <- data.frame(pv = pv)
    outtab$row <- seq_len(m)
    outtab <- outtab[order(outtab$pv), ]
    outtab$rank <- seq_len(m)
    outtab$critvals <- outtab$rank*input$alpha/m

    if (input$procedure == "yeku") {
      outtab$critvals <- outtab$critvals/(log(m) + -digamma(1) + 1/(2*m))
    }

    thresh <- max(which(outtab$pv < outtab$critvals))
    outtab$signif <- seq_len(nrow(outtab)) <= thresh

    if (!input$sorted) {
      outtab <- outtab[order(outtab$row), ]
    }

    outtab[, names(outtab) != "row"]
  })

  output$result <- renderTable({
    outtab <- process_pvalues()

    outtab <- cbind(seq_len(nrow(outtab)), outtab)
    outtab$signif <- ifelse(outtab$signif, "Yes", "No")
    names(outtab) <- c("Number", "p-value", "Rank",
                       "BH Critical Value", "Significant?")
    print(outtab)
  },
  digits = 3,
  hover =  TRUE,
  align = "rcrcl")

  output$plot <- renderPlot({
    outtab <- process_pvalues()

    outtab$rows <- seq_len(nrow(outtab))
    ggplot(outtab, aes(x = rows)) +
      geom_point(aes(y = pv, color = signif), size = 4) +
      geom_line(aes(y = critvals)) +
      theme(text = element_text(size = 20)) +
      xlab("Number") + ylab("") +
      guides(color = guide_legend("Sigificant?"))
  })
}

shinyApp(ui, server)
