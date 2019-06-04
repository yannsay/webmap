get_somalia_district_colors <- function(df) {
  df$color <- NA
  df[df[, "district"] == "mogadishu_daynile", "color"] <- "yellow"
  df[df[, "district"] == "mogadishu_kahda", "color"] <- "blue"
  df[df[, "district"] == "bossaso", "color"] <- "red"
  df[df[, "district"] == "hargeysa", "color"] <- "orange"
  return(df$color)
}

webmap_icons<-function(){
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = get_somalia_district_colors(sampling_frame)
  )
}
