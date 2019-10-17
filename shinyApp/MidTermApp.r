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
                                      "Trust in Government",
                                      "Average life Expectancy",
                                      "Research and Development",
                                      "Homicides",
                                      "Log of GDP in Billions",
                                      "Education",
                                      "CO2"),
                       selected = "Freedom")),
           mainPanel(
           leafletOutput(outputId = "distPlot"),
           plotOutput(outputId = "legend")
           )
  ),
    
    tabPanel("Relationship",
             sidebarPanel(
             selectInput("var2",
                         label = "Choose a second varible to see its relationship with Happiness Score",
                         choices = c("Freedom",
                                        "Trust in Government",
                                        "Average Life Expectency",
                                        "Research and Development",
                                        "Homicides",
                                        "log of GDP in Billion "="GDP",
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
output$legend<-renderPlot({
  bivariate_color_scale<-tibble(
    "3-3"="#3F2949",
    "2-3"="#435786",
    "1-3"="#4885C1",
    "3-2"="#77324C",
    "2-2"="#806A8A",
    "1-2"="#89A1C8",
    "3-1"="#AE3A4E",
    "2-1"="#BC7C8F",
    "1-1"="#CABED0"
  )
  bivariate_color_scale%>%
    gather("group","fill")%>%
    separate(group, into = c("x","y"),sep="-")%>%
    mutate(x= as.integer(x),
           y=as.integer(y))%>%
    ggplot(aes(x,y))+
    geom_tile(aes(fill=fill))+
    scale_fill_identity()+
    labs(x="Freedom",
         y="Happiness Score")
})
  output$distPlot<-renderLeaflet({
    data <- switch(input$var, 
                   "Freedom" = CountryData$Freedom,
                   "Trust in Government" = CountryData$`Trust in Government`,
                   "Average life Expectancy"= CountryData$`Average Life Expectency`,
                   "Research and Development"= CountryData$`Research and Development`,
                   "Homicides" = CountryData$Homicides,
                   "Log of GDP in Billions" = CountryData$GDP,
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
        popup = myPopups)})
  
  output$relation<-renderPlotly({

    plot_ly(CountryData %>% filter(!is.na(get(input$var2))), x = ~get(input$var2), color = I("blue")) %>%
      add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
      add_lines(y = ~fitted(get(input$type)(Happiness.Score ~ get(input$var2))),
                line = list(color = '#07A4B5'),
                name = "Lm Smoother", showlegend = TRUE) %>%
      layout(xaxis = list(title = input$var2),
             yaxis = list(title = 'Happiness Score'),
             legend = list(x = 10, y = 1))
  })
  
}

shinyApp(ui = ui, server = server)