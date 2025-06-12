# app.R

library(shiny)
library(shinythemes)
source("option_pricing.R")
source("option_greeks.R")
source("utils.R")
source("plots_2d.R")
source("plots_3d.R")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Option Greeks Visualizer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Option Type", choices = c("Call" = 0, "Put" = 1)),
      selectInput("greek", "Greek", choices = c("delta", "gamma", "vega", "theta", "rho")),
      sliderInput("S", "Spot Price Range", min = 50, max = 150, value = c(75, 125)),
      numericInput("X", "Strike Price", value = 100),
      sliderInput("t", "Time to Maturity (Years)", min = 0.01, max = 1, value = 0.5, step = 0.01),
      sliderInput("r", "Risk-free Rate", min = 0, max = 0.2, value = 0.05),
      sliderInput("v", "Volatility", min = 0.01, max = 1, value = 0.2)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("2D Greeks", plotOutput("greek2d", hover = hoverOpts("hover_info")), verbatimTextOutput("hover_details")),
        tabPanel("3D Greeks", plotOutput("greek3d"))
      )
    )
  )
)

server <- function(input, output) {

  output$greek2d <- renderPlot({
    S_range <- seq(input$S[1], input$S[2], length.out = 100)
    plot2DGreeks(as.numeric(input$type), input$greek, S_range, input$X, input$t, input$r, input$v)
  })

  output$hover_details <- renderPrint({
    hover <- input$hover_info
    if (!is.null(hover)) {
      cat(paste0("Spot Price: ", round(hover$x, 2), "\n", input$greek, ": ", round(hover$y, 4)))
    }
  })

  output$greek3d <- renderPlot({
    S_vals <- seq(input$S[1], input$S[2], length.out = 25)
    t_vals <- seq(0.01, 1, length.out = 25)
    plot3DGreeks(as.numeric(input$type), input$greek, S_vals, input$X, t_vals, input$r, input$v)
  })
}

shinyApp(ui, server)
