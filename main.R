#install.packages("maptools")
install.packages("latticeExtra")##第一次使用前先安裝
library(httr)#取得網頁原始碼的工具
library(xml2) 
library(jsonlite)#取得json格式文件的套件
library(maptools) #HTML原始碼解析套件
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plyr)
library(ggmap)
library(mapdata)
library(maps)
library(leaflet)
library(devtools)
library(RColorBrewer)
search()

Sys.setlocale("LC_CTYPE", "UTF-8")
climate <- read.csv("/**location**/GlobalLandTemperaturesByCountry.csv")
climate <- climate[,-3]
climate$year <-year(climate$dt)
climate$dt 
climate <- filter(climate, year > 1984)
View(climate)

#install.packages("mice")
library(mice)
md.pattern(climate)
sum(is.na(climate))
climate<-na.omit(climate)
View(climate)

climate$AverageTemperature <- as.numeric(climate$AverageTemperature)
climate <- climate[,-1]

write.csv(climate,file = "/**location**/climate.csv")

climate <- read.table("/**location**/climate.csv",sep = ",",header = TRUE)
View(climate)

climate <- climate[,-1]
climate_GP <- group_by(climate,Country,year)
climate_year <- summarise(climate_GP, mean_climate = mean(AverageTemperature))
View(climate_year)

cs_age <- read.csv("/**location**/data_age_merge.csv")
cs <- read.csv("/**location**/data_merge.csv")
male <- read.csv("/**location**/male_age.csv")
female <- read.csv("/**location**/female_age.csv")
View(cs)
cs <- cs[,-1]
cs_age <- cs_age[,-1]
male <- male[,-1]
female <- female[,-1]

ggplot(cs,aes(x=mean_climate,y=rate))+
  geom_point(col = "steelblue")+
  geom_smooth(col="red")+
  theme(panel.grid.major.x = element_blank()#刪除直網格線
        ,panel.grid.minor.x =element_blank()#刪除直網格線
        ,panel.grid.major.y =element_blank()
        ,panel.grid.minor.y =element_blank())+
  xlab("average temperature")+
  ylab("suicide rate")+
  ggtitle("WORLD")+
  theme(plot.title = element_text(size = rel(1.5),lineheight =0.9,family="Times",face="bold",hjust = 0.5),
        plot.background = element_rect(fill = 'grey'))

ggplot(cs,aes(x=sex,y=rate))+
  geom_boxplot(col = "steelblue")+
  theme_gray()+
  coord_flip()+
  xlab("sex")+
  ylab("suicide rate")+
  ggtitle("male&female")+
  theme(plot.title = element_text(size = rel(1.5),lineheight =0.9,hjust = 0.5),
        plot.background = element_rect(fill = 'grey'),
        panel.grid.major.x = element_blank()#刪除直網格線
        ,panel.grid.minor.x =element_blank()#刪除直網格線
        ,panel.grid.major.y =element_blank())

meanc <- read.csv("/**location**/mean_c.csv")
meanc <- meanc[,-1]
meanc$COUNTRY <- meanc$Country

wmap <- read.csv("/**location**/map-location.csv")

world_c <- merge(wmap,meanc)

#No Icon
map<-leaflet(wmap) %>%
  addTiles() %>%
  clearBounds %>%
  addCircleMarkers(lng = wmap$long, lat = wmap$lat,
                 popup = ~paste(wmap$COUNTRY,"Avg temperature：",round(meanc$mean_climate,2)))
map

#Icon
cityIcon<- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/maps-and-locations-vol-2/24/_building-512.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 30)

worldmap<-leaflet(wmap)  %>% addTiles() %>%
  setView(lng=10, lat=45, zoom=4) %>%
  addMarkers(lng = wmap$long, lat = wmap$lat, 
             popup = ~paste(wmap$COUNTRY,"Avg temperature：",round(meanc$mean_climate,2)),
             icon = cityIcon)
worldmap
