
mydata <-read.csv("E:/AI project/map2.csv",stringsAsFactors=FALSE,header=TRUE)


install.packages("ggmap")
library(ggplot2)
library(ggmap)
register_google(key = "AIzaSyBphHG_WZeb-oUABg6OMU_92rbTb6ahtOo")
library(jsonlite)
devtools::install_github("dkahle/ggmap")

#Visualize the map
#basic map chart2
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(tidyverse)
library(geofacet)
library(stringr)
library(tidycensus)
library(jsonlite)

data <- data.frame(Rate_of_Fatalities= mydata$RateofFatalities2012,
                   state = tolower(mydata$State))
map <- map_data("state")
low_color='#ccdbe5' 
high_color="#114365"
legend_title = 'Rate.of.Fatalities.'
ggplot(data, aes(map_id = tolower(mydata$State))) +
  geom_map(aes(fill =Rate_of_Fatalities),color="#ffffff",size=.15, map = map) + 
  expand_limits(x = map$long, y = map$lat) +
  coord_map() +
  labs(x = "", y = "") +
  scale_fill_continuous(low = low_color, high= high_color, guide = guide_colorbar(title = legend_title))+
  ggtitle('The Distribution of Fatalities rate in US for 2012', 
          subtitle = 'North Dakota has highest fatalities within US for 2012')+ theme(legend.position = "bottom", 
            panel.background = element_blank())
  

# would run successfully
library(leafletCN)
mydata <-read.csv("E:/AI project/map2.csv",stringsAsFactors=FALSE,header=TRUE)
mydata$long<-mydata$Lon
# Show first 20 rows from the `quakes` dataset
leaflet(data = mydata) %>% addTiles() %>%
  addMarkers(~long,~Lat, label = ~as.character(mydata$StateRankFatalities2012)) %>%
addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorBin("YlOrRd", POP2005)(POP2005) )

# would run successfully
library(plotly)
library(ggplot2)
library(crosstalk)
#test on mydata 
scatterplot <- plot_ly(mydata, x = ~State, y =~RateofFatalities2012) %>%  add_markers(color = I("navy"))
scatterplot
#################################################################################################
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(plotly))
usa <- albersusa::usa_sf("laea")
usd <- crosstalk::SharedData$new(usa)
p <- ggplot(usd) + geom_sf(aes(fill = pop_2010))

ggplotly(p) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  ) %>%
  widgetframe::frameWidget()






