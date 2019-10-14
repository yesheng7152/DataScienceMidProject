library(leaflet)    # The map-making package
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(dplyr)      # Used for data manipulation and merging
library(htmltools)
library(tidyr)     
library(ggplot2)   
library(readr)
library(plotly)   # for interactive visuals
library(stringr)  # to process character strings
library(forcats)

#Read all the data
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")
co2<-read.csv("CO2.csv")
happiness<-read.csv("2015.csv")
education<-read.csv("educationExpen.csv")
gdp<-read.csv("GDP.csv")
homicides<-read.csv("homicides.csv")
lifeFemale<-read.csv("LifeExpenFemale.csv")
lifeMale<-read.csv("LifeExpectMale.csv")
head(lifeMale)
rd<-read.csv("R&D.csv")
#Clean education data
education[,3:59]<-list(NULL)
education[,4:6]<-list(NULL)
colnames(education)[3]<-parse_number(colnames(education)[3])
colnames(education)[3]<-"Education"
#Clean co2 data
co2[,3:58]<-list(NULL)
co2[,4:7]<-list(NULL)
colnames(co2)[3]<-parse_number(colnames(co2)[3])
colnames(co2)[3]<-"CO2"
#Clean gdp data
gdp[,3:59]<-list(NULL)
gdp[,4:6]<-list(NULL)
colnames(gdp)[3]<-parse_number(colnames(gdp)[3])
colnames(gdp)[3]<-"GDP"
#Clean homicides data
homicides[,3:59]<-list(NULL)
homicides[,4:6]<-list(NULL)
colnames(homicides)[3]<-parse_number(colnames(homicides)[3])
colnames(homicides)[3]<-"Homicides"
#Clean lifeFemale data
lifeFemale[,3:59]<-list(NULL)
lifeFemale[,4:6]<-list(NULL)
colnames(lifeFemale)[3]<-parse_number(colnames(lifeFemale)[3])
#Clean lifeMale data
lifeMale[,3:59]<-list(NULL)
lifeMale[,4:6]<-list(NULL)
colnames(lifeMale)[3]<-parse_number(colnames(lifeMale)[3])
#Clean r&d data
rd[,3:59]<-list(NULL)
rd[,4:6]<-list(NULL)
colnames(rd)[3]<-parse_number(colnames(rd)[3])
colnames(rd)[3]<-"RD"
#combin data for female and male life expectance 
life<-full_join(x = lifeFemale, y = lifeMale, by = "Country.Code")
life[,1]<-list(NULL)
colnames(life)[2]<-"Female"
colnames(life)[3]<-"Country"
colnames(life)[4]<-"Male"
life<-filter(life, Female != "NA" | Male != "NA")
life$average = (life$Female+life$Male)/2
#Combine the rest of the data frame with the happiness data frame
overall<- full_join(x=life, y=rd, by="Country.Code") 
colnames(overall)[3]="Country"
overall[,6]<-list(NULL)
overall<- full_join(x=overall, y=homicides, by="Country.Code") 
colnames(overall)[3]="Country"
overall[,7]<-list(NULL)
overall<-full_join(x=overall, y=gdp, by="Country.Code")
overall[,8]<-list(NULL)
overall<-full_join(x=overall, y=education, by="Country.Code")
overall[,9]<-list(NULL)
overall<-full_join(x=overall, y=co2, by="Country.Code")
overall[,10]<-list(NULL)
colnames(overall)[3]<-"Country"
overall<-left_join(x = happiness, y = overall, by = "Country")
overall[,5:8]<-list(NULL)
overall[,7:8]<-list(NULL)



CountryData <- left_join(x=data.frame(Id = WorldCountry$id), y=overall, by = c("Id" ="Country.Code"))
head(CountryData)
#CountryData<-filter(CountryData, Happiness.Rank != "NA")
head(CountryData)
##### Graphs 
#Relationship between happyiness score and Freedom
p <- plot_ly(CountryData %>% filter(!is.na(Freedom)), x = ~Freedom, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ Freedom)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Freedom'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and Trust Government and Corruption
p1<- plot_ly(CountryData %>% filter(!is.na(Trust..Government.Corruption.)), x = ~Trust..Government.Corruption., color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ Trust..Government.Corruption.)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Trust..Government.Corruption.'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and Average Life Expectancy
p3 <- plot_ly(CountryData %>% filter(!is.na(average)), x = ~average, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ average)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Average Life Expectancy'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and Research and Development
p4 <- plot_ly(CountryData %>% filter(!is.na(RD)), x = ~RD, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ RD)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Research and Development'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and Homicides
p5 <- plot_ly(CountryData %>% filter(!is.na(Homicides)), x = ~Homicides, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ Homicides)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Homicides'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and GDP
p6 <- plot_ly(CountryData %>% filter(!is.na(GDP)), x = ~GDP, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ GDP)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'GDP'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and Education
p7 <- plot_ly(CountryData %>% filter(!is.na(Education)), x = ~Education, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ Education)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'Education'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))

#Relationship between happyiness score and CO2
p8 <- plot_ly(CountryData %>% filter(!is.na(CO2)), x = ~CO2, color = I("blue")) %>%
  add_markers(y = ~Happiness.Score, text = ~Country, showlegend = FALSE) %>%
  add_lines(y = ~fitted(lm(Happiness.Score ~ CO2)),
            line = list(color = '#07A4B5'),
            name = "Lm Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = 'CO2'),
         yaxis = list(title = 'Happiness Score'),
         legend = list(x = 10, y = 1))


#####Maps(1):
pal <- colorNumeric(
  palette = "Blues",
  domain = CountryData$Happiness.Score)
pal2 <- colorNumeric(
  palette = "Reds",
  domain = CountryData$Freedom)

myLabels <- paste("<strong>", CountryData$Country, "</strong>", "<br/>", 
                  "Happiness Rank:", CountryData$Happiness.Rank)
myPopups <- paste("Freedom", CountryData$Freedom)

Map <- leaflet(WorldCountry) %>% addTiles() %>% 
  addPolygons(
    fillColor = pal(CountryData$Happiness.Score),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.8,
    highlight = highlightOptions(weight = 3,
                                 color = "grey",
                                 fillOpacity = 0.5,
                                 bringToFront = TRUE),
    label = lapply(myLabels, HTML),
    popup = myPopups) %>%
  addPolygons(
    fillColor = pal2(CountryData$Freedom),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.2,
    highlight = highlightOptions(weight = 3,
                                 color = "grey",
                                 fillOpacity = 0.5,
                                 bringToFront = TRUE),
    label = lapply(myLabels, HTML),
    popup = myPopups)%>%
  #addLegend(pal = pal, values = CountryData$Happiness.Score,
   #         title = "Life Expectancy", position = "bottomright")%>%
  addLegend(pal = pal2, values = CountryData$Freedom,
            title = "Life Expectancy", position = "bottomright")




#####Maps(2):
pal <- colorNumeric(
  palette = "Blues",
  domain = CountryData$Happiness.Score)
myLabels <- paste("<strong>", CountryData$Country, "</strong>", "<br/>", 
                  "Happiness Rank:", CountryData$Happiness.Rank)
myPopups <- paste("Freedom", CountryData$Freedom)

Map <- leaflet(WorldCountry) %>% addTiles() %>% 
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
    fillColor = pal2(CountryData$Freedom),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.3,
    highlight = highlightOptions(weight = 3,
                                 color = "grey",
                                 fillOpacity = 0.3,
                                 bringToFront = TRUE),
    label = lapply(myLabels, HTML),
    popup = myPopups)%>%
  addLegend(pal = pal, values = CountryData$Happiness.Score,
           title = "Life Expectancy", position = "bottomleft")%>%
  addLegend(pal = pal2, values = CountryData$Freedom,
            title = "Life Expectancy", position = "bottomright")


