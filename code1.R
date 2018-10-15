#workplace<-read.csv("L:/Pictures/5374-Sagar/Projects/Workplace Safety and Health in the US.csv",stringsAsFactors=FALSE,header=TRUE)
m<-workplace$Number.of.Injuries.Illnesses.2012[which(workplace$State.or.Federal.Program=="Federal"& 
                                                       workplace$State.or.Federal.Program!="NA")]
m
workplace$Number.of.Injuries.Illnesses.2012[workplace$Number.of.Injuries.Illnesses.2012==""]<-"NA"
mean(as.numeric(m))
workplace$Number.of.Injuries.Illnesses.2012<-as.numeric(workplace$Number.of.Injuries.Illnesses.2012)

#clean data insert condational mean value into the missing position
#by summary the data we know all the missing value belong to Federal program. we pick all the injury and iliness 
# data out, give the missing position the mean value of Federal group
mydata <-read.csv("E:/AI project/map2.csv",stringsAsFactors=FALSE,header=TRUE)
mydata$Injuries.Illnesses2012Rate[which(is.na(mydata$Injuries.Illnesses2012Rate))]<-"NA"
mm<-mydata$Injuries.Illnesses2012Rate[which(mydata$StateorFederalProgram=="Federal"&
                                        mydata$Injuries.Illnesses2012Rate!="NA")]
mm
mean(as.numeric(mm))
mydata$Injuries.Illnesses2012Rate[which(is.na(mydata$Injuries.Illnesses2012Rate))]<-3.480952

is.na(mydata$Injuries.Illnesses2012Rate)
max(mydata$Injuries.Illnesses2012Rate)
which(mydata$Injuries.Illnesses2012Rate==5.6)
mydata$State[31]
# input data
mydata <-read.csv("D:/Pictures/5374-Sagar/usa_map.csv",stringsAsFactors=FALSE,header=TRUE)
#################################################################################################
#call ggplot2
# basic bar chart1 for fatailities in US
library(ggplot2)
newdata_dec<- mydata[order(mydata$NumberofFatalities2012, decreasing = F), ]

newdata_dec <- within(newdata_dec, {
  bar_color = ifelse(newdata_dec$State == "Massachusetts","lightcoral","gray53")
  })
knitr::kable(newdata_dec)

barchart <- ggplot() +geom_bar(data = newdata_dec,
                               position = "identity",
                               width = 0.8,
                               stat = "identity",
                               fill = newdata_dec$bar_color,
                               aes(x =newdata_dec$State , y =newdata_dec$NumberofFatalities2012 ))+
  scale_x_discrete(limits = newdata_dec$State) +
  coord_flip()+theme_minimal()
barchart1<-barchart+annotate("text",label = newdata_dec$NumberofFatalities2012,
                             x = newdata_dec$State, 
                             y = newdata_dec$NumberofFatalities2012- 1 ,
                             color = "white",
                             family = "sans",
                             fontface = "bold",size =2, hjust = 1)+
  labs(y="Number of Fatalities", x="State", title="The Distribution of Fatalities in US for 2012",font=3)
print(barchart1)

# basic injury and illness bar chart2
mydata$NumberofInjuries.Illnesses2012[is.na(mydata$NumberofInjuries.Illnesses2012)]<-48400
mydata$Injuries.Illnesses2012Rate[which(is.na(mydata$Injuries.Illnesses2012Rate))]<-3.480952

library(ggplot2)
injurt_dec<- mydata[order(mydata$NumberofInjuries.Illnesses2012 , decreasing = F), ]

injurt_dec <- within(injurt_dec, {
  bar_color = ifelse(injurt_dec$State == "Massachusetts","lightcoral","gray53")
})
knitr::kable(injurt_dec)

barchart0 <- ggplot() +geom_bar(data = injurt_dec,
                               position = "identity",
                               width = 0.85,
                               stat = "identity",
                               fill = injurt_dec$bar_color,
                                 aes(x =injurt_dec$State , y =injurt_dec$NumberofInjuries.Illnesses2012 ))+
  scale_x_discrete(limits = injurt_dec$State) +
  coord_flip()+theme_minimal()
injurt_dec$percentage <- paste(round(as.numeric(injurt_dec$NumberofInjuries.Illnesses2012))/1000,
                               "K", sep = "")
barchart00<-barchart0+annotate("text",label = injurt_dec$percentage,
                             x = injurt_dec$State, 
                             y = injurt_dec$NumberofInjuries.Illnesses2012+10,
                             color = "white",
                             family = "sans",
                             fontface = "bold",size =3, hjust = 1)+
  labs(y="Number of Injuries and Illnesses", x="State", title="The Distribution of Injuries and Illnesses in US for 2012")
print(barchart00)

# we could see from the ordered horizional bar chart we could know the highest number of fatalities is taxes, 
#MA have lower number of fatalities only 44 people, but for the injuries and illness MA located about top 10 among 
# 50 states, California ranked the 1st, which had the highest number of injuries and illiness in 2012

##################################################################################################################################################################################################
#library(ggmap)
#register_google(key = "AIzaSyBphHG_WZeb-oUABg6OMU_92rbTb6ahtOo")
#library(jsonlite)
#devtools::install_github("dkahle/ggmap")

#Visualize the map
#basic map chart2 the fata rate in US


mydata <-read.csv("D:/Pictures/5374-Sagar/usa_map.csv",stringsAsFactors=FALSE,header=TRUE)
library(ggplot2)
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(tidyverse)
library(geofacet)
library(stringr)
library(tidycensus)

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

##################################################################################################################################################################################################
# map for injury and illness rate
data <- data.frame(Rate_of_inj= mydata$Injuries.Illnesses2012Rate,
                   state = tolower(mydata$State))
map <- map_data("state")
low_color='#ccdbe5' 
high_color="#114365"
legend_title = 'Rate.of.Injury.'
ggplot(data, aes(map_id = tolower(mydata$State))) +
  geom_map(aes(fill =Rate_of_inj),color="#ffffff",size=.15, map = map) + 
  expand_limits(x = map$long, y = map$lat) +
  coord_map() +
  labs(x = "", y = "") +
  scale_fill_continuous(low = low_color, high= high_color, guide = guide_colorbar(title = legend_title))+
  ggtitle('The Distribution ofinjury rate in US for 2012', 
          subtitle = 'Mainehas highest injury and illness rate within US for 2012')+ 
  theme(legend.position = "bottom", panel.background = element_blank())




