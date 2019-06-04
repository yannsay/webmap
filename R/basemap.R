#' create basemap with settlement points
#' @return a leaflet object with points created from the somalia sampling frame
#' @export
som_basemap<-function(){
  lng.column.name = "X_geopoint_1_longitude"
  lat.column.name = "X_geopoint_1_latitude"
  long <- as.formula(paste0("~",lng.column.name))
  lat <-  as.formula(paste0("~",lat.column.name))
  getColor<-get_somalia_district_colors
  base_map <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
    addAwesomeMarkers(data = sampling_frame, lng = long,
                      lat = lat, label = ~as.character(Site),
                      icon = webmap_icons(), group = "samplingframe") %>%
    addLayersControl(baseGroups = c("OSM","Esri"),
                     position = "topleft") %>%
    addScaleBar()

  class(base_map) <- c("leaflet", "suppress_viewer", "htmlwidget")
  base_map
}
