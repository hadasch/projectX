rm(list=ls())
library(data.table)
# list.data <- readRDS("list_data.rds")
# 
# Locs <- list()
#   for (sel.dat in names(list.data)) {
#   L <- as.data.table(list.data[[sel.dat]]$`plot data info`["L","freq_list"])
#   names(L)[1] <- "Loc"
#   Locs[[sel.dat]] <- L
#   }
# Locations <- data.table(plyr::ldply(Locs, data.frame, .id="Data"))
# Locations$Loc <- as.character(Locations$Loc)
# fwrite(Locations, file="Locations.txt", sep="\t")

####
library(maps)
library(mapdata)
library(ggmap)

Latlon <- fread("Locations latlon.txt")
Latlon <- Latlon[Data=="boro"]
names(Latlon)[4:5] <- c("lat", "lon")
Latlon[, Data := c("aman & boro")]
Latlon[Loc=="Habiganj", Data := c("boro")]

map <- get_map(c(median(Latlon$lon), median(Latlon$lat)), 
               zoom = 6, source = "google", maptype = "terrain", language = "en-EN")

ggmap(map) +
  geom_point(data = Latlon, aes(x = lon, y = lat, color=Data), alpha = 0.7, size=7) +
  labs(x = NULL, y = NULL, size  = 'Data points', color = "Dataset") +
  scale_x_continuous(limits = c(88, 92.6)) +
  scale_y_continuous(limits = c(21, 26.5))
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_rect(fill = "white"))


