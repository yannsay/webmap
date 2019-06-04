#Clean workspace and load libraries
rm(list=ls())
library("httr")
library("jsonlite")
library("tidyverse")
library("leaflet")
library("RColorBrewer")
library("htmlwidgets")
library("leaflet.extras")

user <- "cccmcluster_somalia"
# pw <- "give your password"

# Creates base map
sampling_frame <- read.csv("./dependencies/samplingframe.csv", stringsAsFactors = F)
sampling_frame <- sampling_frame %>% 
  select(district, Site, grep("geo", names(sampling_frame), value = T)) 

getColor <- function(df) {
  df$color <- NA
  df[df[, "district"] == "mogadishu_daynile", "color"] <- "yellow"
  df[df[, "district"] == "mogadishu_kahda", "color"] <- "blue"
  df[df[, "district"] == "bossaso", "color"] <- "red"
  df[df[, "district"] == "hargeysa", "color"] <- "orange"
  return(df$color)
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(sampling_frame)
)

base_map <- leaflet() %>%
  addTiles(group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
  addAwesomeMarkers(data = sampling_frame, lng = ~X_geopoint_1_longitude, lat = ~X_geopoint_1_latitude, label = ~as.character(Site),
                    icon = icons, group = "samplingframe") %>%
    addLayersControl(baseGroups = c("OSM","Esri"),
                     position = "topleft") %>%
    addScaleBar()

class(base_map) <- c("leaflet", "suppress_viewer", "htmlwidget")

pal <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Dark2"))(20),
                   levels = 1:20)

#get the data from koboserver
get_from_kobo <- function(id = NULL) {
  call <- paste0("https://kobocat.unhcr.org/api/v1/data/", id, ".csv") 
  get_all <- GET(call, authenticate(user,pw))
  df <- content(get_all, type = "text/csv")
  names(df) <- sub(".*?/", "", names(df))
  names(df) <- gsub("/", ".", names(df))
  return(df)
}

df_csa <- get_from_kobo("13968") 
head(df_csa)
df_csa_short <- df_csa[, c("deviceid", "assessment_district", "assessment_settlement", grep("gps", names(df_csa), value = T))]

for (i in 1:length(unique(df_csa_short$assessment_district))) {
  ass_district_i <- unique(df_csa_short$assessment_district)[i]
  unique_device_id <- unique(df_csa_short[df_csa_short[, "assessment_district"] == ass_district_i, "deviceid"]) %>% unlist()
  for (j in 1:length(unique_device_id)) {
    df_csa_short[df_csa_short[, "assessment_district"] == ass_district_i & df_csa_short[,"deviceid"] == unique_device_id[j], "enum_color"] <- j
  }
}

#create the map
base_map %>%
  addCircleMarkers(data = df_csa_short, lng = ~`_gps_longitude`, lat = ~`_gps_latitude`,
                   radius = 4, color = ~pal(enum_color), label = ~as.character(assessment_settlement)) %>%
  saveWidget(file="myMap.html")
browseURL("myMap.html")

base_map %>%
  addCircleMarkers(data = df_csa_short, lng = ~`_gps_longitude`, lat = ~`_gps_latitude`,
                   radius = 6, color = ~pal(enum_color), label = ~as.character(assessment_settlement)) %>%
  print()



