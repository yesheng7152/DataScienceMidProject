library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(leafem)
library(DT)

CountryData<-read.csv("CountryData.csv")
# See above for the definitions of ui and server
ui <- navbarPage("Happiness In Real Perspective",
  tabPanel("Map",
           sidebarPanel(
           selectInput("var",
                       label = "Choose a second varible to see its relationship with Happiness Score",
                       choices = list("Freedom",
                                      "Trust in Government",
                                      "Average life Expectancy",
                                      "Research and development expenditure (% of GDP)",
                                      "Homicides (per 100,000 people)",
                                      "Log of GDP in Billions",
                                      "Government expenditure on education, total (% of GDP)",
                                      "CO2 emissions (metric tons per capita)"),
                       selected = "Freedom"),
           img(src="https://github.com/yesheng7152/DataScienceMidProject/blob/master/legend.png?raw=true",
               height='200px',
               width='200px')
           ),
           mainPanel(
           leafletOutput(outputId = "distPlot")
           )
  ),
    
    tabPanel("Relationship",
             sidebarPanel(
             selectInput("var2",
                         label = "Choose a second varible to see its relationship with Happiness Score",
                         choices = list("Freedom",
                                        "Trust in Government",
                                        "Average life Expectancy",
                                        "Research and development expenditure (% of GDP)",
                                        "Homicides (per 100,000 people)",
                                        "Log of GDP in Billions",
                                        "Government expenditure on education, total (% of GDP)",
                                        "CO2 emissions (metric tons per capita)"),
                         selected = "Freedom"),
             radioButtons("type",
                          label = "Choose the type of smoothe fit line, lm represents linear fit and loess 
                   represent the loess smooth fit",
                          choices = list("lm",
                                         "loess"),
                          selected="lm")),
             mainPanel(
             plotlyOutput(outputId = "relation"))
             ),
  tabPanel("TableSet",
           h2("Happiness verses other variables"),
           hr(),
           DT::dataTableOutput('CountryData'))
)

   


      




server <- function(input, output) {
  
  # 
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$distPlot<-renderLeaflet({
    data <- switch(input$var, 
                   "Freedom" = CountryData$Freedom,
                   "Trust in Government" = CountryData$Trust.in.Government,
                   "Average life Expectancy"= CountryData$Average.Life.Expectency,
                   "Research and development expenditure (% of GDP)"= CountryData$Research.and.Development,
                   "Homicides (per 100,000 people)" = CountryData$Homicides,
                   "Log of GDP in Billions" = CountryData$GDP,
                   "Government expenditure on education, total (% of GDP)" = CountryData$Education,
                   "CO2 emissions (metric tons per capita)" = CountryData$CO2)

    
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
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 3,
                                     color = "grey",
                                     fillOpacity = 0.7,
                                     bringToFront = TRUE),
        label = lapply(myLabels, HTML),
        popup = myPopups) %>%
      addPolygons(
        fillColor = pal2(data),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.3,
        highlight = highlightOptions(weight = 3,
                                     color = "grey",
                                     fillOpacity = 0.3,
                                     bringToFront = FALSE),
        label = lapply(myLabels, HTML),
        popup = myPopups)
  })
  
  output$relation<-renderPlotly({
    data2 <- switch(input$var2, 
                   "Freedom" = "Freedom",
                   "Trust in Government" = "Trust.in.Government",
                   "Average life Expectancy"= "Average.Life.Expectency",
                   "Research and development expenditure (% of GDP)"= "Research.and.Development",
                   "Homicides (per 100,000 people)" = "Homicides",
                   "Log of GDP in Billions" = "GDP",
                   "Government expenditure on education, total (% of GDP)" = "Education",
                   "CO2 emissions (metric tons per capita)" = "CO2")
    plot_ly(CountryData %>% filter(!is.na(get(data2))), x = ~get(data2), color = I("blue")) %>%
      add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
      add_lines(y = ~fitted(get(input$type)(Happiness.Score ~ get(data2))),
                line = list(color = '#07A4B5'),
                name = "Lm Smoother", showlegend = TRUE) %>%
      layout(xaxis = list(title = input$var2),
             yaxis = list(title = 'Happiness Score'),
             legend = list(x = 10, y = 1))
  })
  
  output$CountryData <-DT::renderDataTable({
    copyCD<-CountryData
    copyCD<-filter(copyCD,Id != "-99" & Country!="NA")
    rownames(copyCD)=copyCD[,1]
    copyCD[1]<-NULL
    DT::datatable(copyCD,filter="top",rownames=FALSE)
  })
  
}

shinyApp(ui = ui, server = server)