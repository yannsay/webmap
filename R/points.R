
assign_colors <- function(data,color_by = "deviceid", color_group = "assessment_district"){

  for (i in 1:length(unique(data[[color_group]]))) {
    ass_district_i <- unique(data[[color_group]])[i]
    unique_device_id <- unique(data[data[, color_group] == ass_district_i, color_by]) %>% unlist()
    for (j in 1:length(unique_device_id)) {
      data[data[, "assessment_district"] == ass_district_i & data[,"deviceid"] == unique_device_id[j], "enum_color"] <- j
    }
  }
  data



}

#' add points of an assesment to the web map
#' @param .base_map a leaflet basemap, i.e. made with base_map()
#' @param assessment_data your assessment data as a data.frame (can use read.csv() to make this)
#' @param gps.lng.column name of the latitude column in quotes (optional)
#' @param gps.lon.column name of the latitude column in quotes (optional)
#' @param color.by.column name of the column to color by (max 8 unique values)
#' @param color.group.column name of the column of groups between which colours don't need to be unique
#' @export
som_map_add_points<-function(.base_map,
                             assessment_data,
                             gps.lng.column = "X_gps_longitude",
                             gps.lat.column = "X_gps_latitude",
                             color.by.column = "deviceid",
                             color.group.column = "district_idp"){


  if(!(gps.lng.column %in% names(assessment_data))){stop(paste(gps.lng.column," not in data headers"))}
  if(!(gps.lat.column %in% names(assessment_data))){stop(paste(gps.lat.column,"gps.lat.column not in data headers"))}
  if(!(color.by.column %in% names(assessment_data))){stop(paste(color.by.column," not in data headers"))}
  if(!(gps.lng.column %in% names(assessment_data))){stop(paste(gps.lng.column," not in data headers"))}
  lng.formula <- as.formula(paste0("~",gps.lng.column))
  lat.formula <- as.formula(paste0("~",gps.lat.column))

  assessment_data <- assign_colors(assessment_data)

  pal <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Dark2"))(20),
                     levels = 1:20)

  .base_map %>%
    addCircleMarkers(data = assessment_data, lng = lng.formula, lat = lat.formula,
                     radius = 6, color = ~pal(enum_color), label = ~as.character(assessment_settlement))

}
