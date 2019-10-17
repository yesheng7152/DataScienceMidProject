#importing all the packages
library(geojsonio) 
library(dplyr)      
library(htmltools)
library(tidyr)     
library(ggplot2)   
library(readr)
library(plotly)  


#Reading the raw data
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
gdp[3]=log(gdp[3]/1000000000, 2)

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
colnames(rd)[3]<-"Research and Development"

#Combin data for female and male for the average life expectancy
life<-full_join(x = lifeFemale, y = lifeMale, by = "Country.Code")
life[,1]<-list(NULL)
colnames(life)[2]<-"Female"
colnames(life)[3]<-"Country"
colnames(life)[4]<-"Male"
life<-filter(life, Female != "NA" | Male != "NA")
life$`Average Life Expectency` = (life$Female+life$Male)/2

#Combine the rest of the dataset with the happiness dataset
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
colnames(overall)[6]<-"Trust in Government"
head(overall)
#Join the complete data set with WorldCountry, preparing for creating map using leaflet
CountryData <- left_join(x=data.frame(Id = WorldCountry$id), y=overall, by = c("Id" ="Country.Code"))

#Save the clean dataset as a csv file"
write.csv(CountryData,"CountryData.csv")

#Creating the bivariate legend for the map 
#Setting the colors
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
#Creating the lengend using ggplot
legend<-bivariate_color_scale%>%
  gather("group","fill")%>%
  separate(group, into = c("x","y"),sep="-")%>%
  mutate(x= as.integer(x),
         y=as.integer(y))%>%
  ggplot(aes(x,y))+
  geom_tile(aes(fill=fill))+
  scale_fill_identity()+
  labs(x="Varying Factors",
       y="Happiness Score")
#Save the ggplot as a png file, this image will be uploaded into github, so it can be referenced later in the 
#app
ggsave("legend.png",plot=legend,
       width = 2, height = 2, bg="transparent")


