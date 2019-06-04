#Clean workspace and load libraries
rm(list=ls())
library("httr")
library("jsonlite")
library("tidyverse")
library("leaflet")
library("RColorBrewer")
library("htmlwidgets")
library("leaflet.extras")

library("webmap")


#create the map
asse_data<-read.csv("./dependencies/dataset_webmap.csv")
asse_data$deviceid<-runif(nrow(asse_data),1,7) %>% round
write.csv(asse_data,"../som_assessment_data_w_gps.csv")
som_basemap() %>% som_map_add_points(asse_data) %>% print



browseURL("myMap.html")

