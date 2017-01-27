
stations_start <- dplyr::select(data, starts_with('start.station')) %>% unique()
stations_end <- dplyr::select(data, starts_with('end.station')) %>% unique()
colnames(stations_start) <- colnames(stations_end) <- c('STATION_ID',
                                                        'STATION_NAME',
                                                        'STATION_LAT',
                                                        'STATION_LON')
stations <- rbind(stations_start, stations_end) %>% unique()
rownames(stations) <- stations$STATION_NAME

# reorder columns as stations_name should be first to create igraph
stations <- dplyr::select(stations, STATION_NAME, STATION_ID,
                          STATION_LAT, STATION_LON)

# q <- qmplot(x=STATION_LON, y=STATION_LAT,
#             data = stations,
#             mapcolor = 'bw',
#             maptype = 'toner-lite',
#             extent = 'device',
#             zoom = 14,
#             color=I('red'), alpha = I(.7))

map_stations_loc <- function(df,
                             plot_lat_bt=38.68034, plot_lat_up=42.77152,
                             plot_lon_lft=-76.01713, plot_lon_rit=-71.95005){
  ## show stations on maps
  # x is data frame for stations info
  q <- qmplot(x=STATION_LON, y=STATION_LAT,
              data = df,
              maptype = 'toner-lite',
              extent = 'device',
              zoom = 14,
              color=I('red'), alpha = I(.7))
  return(q)
}

p_all_stations <- map_stations_loc(stations)
ggsave(file.path(FIGDIR, 'all_stations_locations.pdf'), p_all_stations,
       width=10, height = 10)
get_toner_basic <- function(df){
  start_lat_min <- min(df$STATION_LAT)
  start_lat_max <- max(df$STATION_LAT)
  start_lon_min <- min(df$STATION_LON)
  start_lon_max <- max(df$STATION_LON)
  plot_lat_bt  <- start_lat_min - 1
  plot_lat_up  <- start_lat_max + 1
  plot_lon_lft <- start_lon_min - 1
  plot_lon_rit <- start_lon_max + 1
  map_corners <- expand.grid(lon=c(plot_lon_lft, plot_lon_rit),
                             lat=c(plot_lat_bt, plot_lat_up))
  print(map_corners)
  q <- qmplot(x=lon, y=lat,
              data=map_corners,
              maptype = 'toner-lite',
              extent='device',
              zoom=10,
              color=I('black'), alpha=I(0.05))
  return(q)
}

# map_stations_loc(stations)
# start_lat_min <- min(data$start.station.latitude)
# start_lat_max <- max(data$start.station.latitude)
# start_lon_min <- min(data$start.station.longitude)
# start_lon_max <- max(data$start.station.longitude)
# plot_lat_bt  <- start_lat_min - 2
# plot_lat_up  <- start_lat_max + 2
# plot_lon_lft <- start_lon_min - 2
# plot_lon_rit <- start_lon_max + 2
# map_corners <- expand.grid(lon=c(plot_lon_lft, plot_lon_rit),
#                            lat=c(plot_lat_bt, plot_lat_up))


