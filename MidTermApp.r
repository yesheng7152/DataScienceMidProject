library(shiny)
library(plotly)
# See above for the definitions of ui and server
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Select layout with input and output definitions ----
  sidebarLayout(
    
    # Select panel for inputs ----
    sidebarPanel(
      # Input: Select for the variable to see the relationship with Happiness Score ----
      selectInput("var",
                  label = "Choose a second varible to see its relationship with Happiness Score",
                  choices = list("Freedom",
                                 "Trust Government and Corruption",
                                 "Average life Expectancy",
                                 "Research and Development",
                                 "Homicides",
                                 "GDP",
                                 "Education",
                                 "CO2"),
                  selected = "Freedom"),
      # Input: Select the type of linear fit ----
      radioButtons("type",
                   label = "Choose the type of smoothe fit line, lm represents linear fit and loess 
                   represent the loess smooth fit",
                   choices = list("lm",
                                  "loess"),
                   selected="lm")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  output$distPlot<-renderPlotly({
    
    plot_ly(CountryData %>% filter(!is.na(var)), x = ~var, color = I("blue")) %>%
      add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
      add_lines(y = ~fitted(lm(Happiness.Score ~ var)),
                line = list(color = '#07A4B5'),
                name = "Lm Smoother", showlegend = TRUE) %>%
      layout(xaxis = list(title = 'var'),
             yaxis = list(title = 'Happiness Score'),
             legend = list(x = 10, y = 1))
  })
  
}

shinyApp(ui = ui, server = server)