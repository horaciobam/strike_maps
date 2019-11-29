source('country_geocoding.R')

library(ggmap)
data <- read.csv("Responses1.csv", sep = ",", header=T)
country<- data$País.donde.reside.actualmente

# From Google maps
register_google(key = "AIzaSyArEZpq9pQRmm03tuPJQ7kpHBY5R0b_CQI")

library("ggmap")
library(maptools)
library(maps)

visited <- as.character(country)[2:5]

ll.visited <- country_geocoding(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

library(plyr)
counts <- ddply(ll.visited, ~ country, nrow)
uni <- unique(ll.visited)
total <- left_join(counts, uni, by="country")

#First try of a map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
#Now Layer the cities on top
mp <- mp+ geom_point(data = total, aes(x=lon, y=lat, size=V1), color="blue", alpha=.5) 
mp
