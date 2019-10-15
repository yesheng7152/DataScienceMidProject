library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
#install.packages("leaflet")
load(file = "CountryData.RData")
# See above for the definitions of ui and server
ui <- navbarPage("Title",
  tabPanel("Map",
           sidebarPanel(
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
                       selected = "Freedom")),
           mainPanel(
           leafletOutput(outputId = "distPlot")
           )
  ),
    
    tabPanel("Relation",
             sidebarPanel(
             selectInput("var2",
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
             radioButtons("type",
                          label = "Choose the type of smoothe fit line, lm represents linear fit and loess 
                   represent the loess smooth fit",
                          choices = list("lm",
                                         "loess"),
                          selected="lm")),
             mainPanel(
             plotlyOutput(outputId = "relation"))
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

  output$distPlot<-renderLeaflet({
    data <- switch(input$var, 
                   "Freedom" = CountryData$Freedom,
                   "Trust Government and Corruption" = CountryData$Trust..Government.Corruption.,
                   "Average life Expectancy"= CountryData$average,
                   "Research and Development"= CountryData$RD,
                   "Homicides" = CountryData$Homicides,
                   "GDP" = CountryData$GDP,
                   "Education" = CountryData$Education,
                   "CO2" = CountryData$CO2)
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = CountryData$Happiness.Score)
    pal2 <- colorNumeric(
      palette = "Reds",
      domain = data)
    
    myLabels <- paste("<strong>", CountryData$Country, "</strong>", "<br/>", 
                      "Happiness Rank:", CountryData$Happiness.Rank)
    myPopups <- paste(input$var, data)
    leaflet(WorldCountry) %>% addTiles() %>% 
      addPolygons(
        fillColor = pal(CountryData$Happiness.Score),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.8,
        highlight = highlightOptions(weight = 3,
                                     color = "grey",
                                     fillOpacity = 0.8,
                                     bringToFront = TRUE),
        label = lapply(myLabels, HTML),
        popup = myPopups) %>%
      addPolygons(
        fillColor = pal2(data),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.2,
        highlight = highlightOptions(weight = 3,
                                     color = "grey",
                                     fillOpacity = 0.2,
                                     bringToFront = TRUE),
        label = lapply(myLabels, HTML),
        popup = myPopups)})
  
  output$relation<-renderPlotly({

    plot_ly(CountryData %>% filter(!is.na(get(input$var2))), x = ~get(input$var2), color = I("blue")) %>%
      add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
      add_lines(y = ~fitted(get(input$type)(Happiness.Score ~ get(input$var2))),
                line = list(color = '#07A4B5'),
                name = "Lm Smoother", showlegend = TRUE) %>%
      layout(xaxis = list(title = 'input$var2'),
             yaxis = list(title = 'Happiness Score'),
             legend = list(x = 10, y = 1))
  })
  
}

shinyApp(ui = ui, server = server)