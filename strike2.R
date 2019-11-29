source('country_geocoding.R')

library(ggmap)
data <- read.csv("pais.csv", sep = "|", header=T)

# From Google maps
register_google(key = "AIzaSyBHXXWu667ViYsh-ZNqGcFMANacLO69ZFA")

library("ggmap")
library(maptools)
library(maps)

visited <- as.character(data$pais)
#TODO: make the next line to return a data frame
ll.visited <- country_geocoding(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

#First try of a map
#map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
#points(visit.x,visit.y, col="red", pch=16)
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp

# Second try of a map (not working)
map <- get_map(location = 'world')
mapPoints <- ggmap(map) + 
          geom_point(aes(x = lon, y = lat, size = table(ll.visited$address)), data = ll.visited, alpha = .5)
mapPoints
