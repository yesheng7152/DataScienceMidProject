#Load all needed packages
library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(leafem)
library(DT)

#Read the cleaned dataset
CountryData <- read.csv("CountryData.csv")

#UI for the app that contains a map, graph, and tableset
ui <- navbarPage("Happiness In Real Perspective",
                 #Tab panel that contains the map ----
                 tabPanel("Map",
                          
                          #Sidebar Panel for inputs ----
                          sidebarPanel(
                              
                              # Inputs: Selection for the various factors that might associate with Happiness---
                              selectInput("var",
                                          label = "Choose a second varible to see its relationship with Happiness Score",
                                          choices = list(
                                              "Freedom",
                                              "Trust in Government",
                                              "Average life Expectancy",
                                              "Research and development expenditure (% of GDP)",
                                              "Homicides (per 100,000 people)",
                                              "Log of GDP in Billions",
                                              "Government expenditure on education, total (% of GDP)",
                                              "CO2 emissions (metric tons per capita)"
                                          ),
                                          selected = "Freedom"
                              ),
                              
                              #Attaching premade legend to the sidebarPanel ----
                              img(
                                  src = "https://github.com/yesheng7152/DataScienceMidProject/blob/master/legend.png?raw=true",
                                  height = '200px',
                                  width = '200px'
                              )
                          ),
                          
                          #Main panel for displaying outpus ----
                          mainPanel(
                              # Output: Map ----
                              leafletOutput(outputId = "distPlot")
                          )
                 ),
                 
                 #Tab Panel for Relationsip Graph
                 tabPanel("Relationship",
                          
                          # Sidebar Panel for inputs ---- 
                          sidebarPanel(
                              
                              # Inputs: Selection for the various factors that might associate with Happiness ----
                              selectInput("var2",
                                          label = "Choose a second varible to see its relationship with Happiness Score",
                                          choices = list(
                                              "Freedom",
                                              "Trust in Government",
                                              "Average life Expectancy",
                                              "Research and development expenditure (% of GDP)",
                                              "Homicides (per 100,000 people)",
                                              "Log of GDP in Billions",
                                              "Government expenditure on education, total (% of GDP)",
                                              "CO2 emissions (metric tons per capita)"
                                          ),
                                          selected = "Freedom"
                              ),
                              
                              # Inputs: Radio Button for the different types of line fit ----
                              radioButtons("type",
                                           label = "Choose the type of smoothe fit line, lm represents linear fit and loess
                   represent the loess smooth fit",
                                           choices = list("lm",
                                                          "loess"),
                                           selected = "lm"
                              )
                          ),
                          
                          # Main panel for displaying outpus ----
                          mainPanel(
                              
                              #Output: Scatter plot with fitted line 
                              plotlyOutput(outputId = "relation"))
                 ),
                 
                 #Tab Panel that contains Table Set
                 tabPanel("TableSet",
                          h2("Happiness verses other variables"),
                          hr(),
                          
                          # Display the data 
                          DT::dataTableOutput('CountryData')
                 )
)


# Define server logic required to produce a desired Map, Plot, and DataTable
server <- function(input, output) {
    # All expressions that generates either Map, Plotly or Data Table
    # are wraped in a call to renderLeaflet, renderPlotly, or renderDataTable
    # to indicate that:
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output types are Map, Plotly, and Data Table
    
    #Generating the output for the Map tab panel 
    output$distPlot <- renderLeaflet({
        
        #To reassigned the appropriate values to data based on the input variable
        data <- switch(input$var,
                       "Freedom" = CountryData$Freedom,
                       "Trust in Government" = CountryData$Trust.in.Government,
                       "Average life Expectancy" = CountryData$Average.Life.Expectency,
                       "Research and development expenditure (% of GDP)" = CountryData$Research.and.Development,
                       "Homicides (per 100,000 people)" = CountryData$Homicides,
                       "Log of GDP in Billions" = CountryData$GDP,
                       "Government expenditure on education, total (% of GDP)" = CountryData$Education,
                       "CO2 emissions (metric tons per capita)" = CountryData$CO2)
        
        # Generating color spectrums for Happiness and varible that user choose
        pal <- colorNumeric(palette = "Blues", domain = CountryData$Happiness.Score)
        pal2 <- colorNumeric(palette = "Reds", domain = data)
        
        # Set up the labels
        myLabels <-
            paste("<strong>", CountryData$Country,"</strong>","<br/>",
                  "Happiness Rank:",CountryData$Happiness.Rank)
        # Set up the popup based on the varible choosed
        myPopups <- paste(input$var, data)
        
        #Creating the Map
        leaflet(WorldCountry) %>% addTiles() %>%
            #Adding the first layer of color (blue) based on the happiness score
            addPolygons(fillColor = pal(CountryData$Happiness.Score),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 3,
                            color = "grey",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = lapply(myLabels, HTML),
                        popup = myPopups) %>%
            #Adding the second layer of color(Red) based on the input variable
            addPolygons(fillColor = pal2(data),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        fillOpacity = 0.3,
                        highlight = highlightOptions(
                            weight = 3,
                            color = "grey",
                            fillOpacity = 0.3,
                            bringToFront = FALSE ),
                        label = lapply(myLabels, HTML),
                        popup = myPopups)
    })
    
    #Generating the output for the Relationship tab panel 
    output$relation <- renderPlotly({
        
        #To reassigned the appropriate values to data based on the input variable
        data2 <- switch(input$var2,
                        "Freedom" = "Freedom",
                        "Trust in Government" = "Trust.in.Government",
                        "Average life Expectancy" = "Average.Life.Expectency",
                        "Research and development expenditure (% of GDP)" = "Research.and.Development",
                        "Homicides (per 100,000 people)" = "Homicides",
                        "Log of GDP in Billions" = "GDP",
                        "Government expenditure on education, total (% of GDP)" = "Education",
                        "CO2 emissions (metric tons per capita)" = "CO2"
        )
        
        #Generate the scatter plot with fitted line 
        #Filter out all the empty data points and set the scatter points
        # to be blue 
        plot_ly(CountryData %>% filter(!is.na(get(data2))),
                x = ~ get(data2),
                color = I("blue")) %>%
            add_markers(
                y = ~ Happiness.Score,
                text = ~ Country,
                showlegend = FALSE
            ) %>%
            #Add the fitted line for the scatter plot
            add_lines(
                y = ~ fitted(get(input$type)(Happiness.Score ~ get(data2))),
                line = list(color = '#07A4B5'),
                name = input$type,
                showlegend = TRUE
            ) %>%
            # Add names for the x and y axis
            layout(
                xaxis = list(title = input$var2),
                yaxis = list(title = 'Happiness Score'),
                legend = list(x = 10, y = 1)
            )
    })
    
    #Generating the output for the TableSet tab panel 
    output$CountryData <- DT::renderDataTable({
        
        #Make a copy of the DataSet
        copyCD <- CountryData
        #Filter out any invalid data 
        copyCD <- filter(copyCD, Id != "-99" & Country != "NA")
        #Rename the rows with more meanful information 
        rownames(copyCD) = copyCD[, 1]
        copyCD[1] <- NULL
        #Create the Data Table
        DT::datatable(copyCD, filter = "top", rownames = FALSE)
    })
    
}

#Run the Application
shinyApp(ui = ui, server = server)
