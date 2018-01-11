
# Map the trips. Visualize the trips on a map. 

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('osmplotr') # extracting spatial data 
library('ggplot2') # plots
library('osrm') # get shortest path
library('sf') # convert to sf object
library('arules') # discretization
library("magick") # to create animated plots


# Import data -------------------------------------------------------------

# import and prepare data for visualization: creation of one table with number
# of trips by time between two stations. Extract the path between the stations
# to map the trips. 

# import trips data
trips <- readRDS('data/processed/trips.rds')

# aggregate trips by hour of the day and by start and end station. The aggregate
# is done hourly as an example. 
trips <- trips[, .(count = uniqueN(trip_id)), 
               by = list(hour_of_day, start_station_id, end_station_id)]

# average number of trips by hour (used to display an histogram on the map)
hourly_trips <- trips[!is.na(hour_of_day), .(count = sum(count)), by = hour_of_day]

# import stations coordinates
station <- readRDS('data/processed/stations.rds')


# Austin map --------------------------------------------------------------

# plot a basic Austin map with the streets and the bike stations

# austin bbox dereived from min and max of station longitude and latitude
austin_bbox <- get_bbox(latlon = c(min(station$longitude) - 0.02, 
                                   min(station$latitude) - 0.015, 
                                   max(station$longitude) + 0.02, 
                                   max(station$latitude) + 0.02))

# get the streets geometry
austin_streets <- extract_osm_objects(key = 'highway', 
                                      bbox = austin_bbox, 
                                      return_type = 'line', 
                                      sf = TRUE, 
                                      geom_only = TRUE)

# base map with streets and stations
austin_map <- osm_basemap(bbox = austin_bbox, bg = 'gray20')
austin_map <- add_osm_objects(map = austin_map, 
                              obj = austin_streets, 
                              col = 'gray40')

# clean session
rm(austin_bbox, austin_streets)


# Shortest path between bike stations -------------------------------------

# the osm api is used to have the shortest pathh between all stations. 

# sortest paths between all pairs of stations.
# nrow_station <- nrow(station)
# for (i in 1:(nrow_station - 1)) {
#   for (j in (i+1):nrow_station) {
#     route <- osrmRoute(src = as.numeric(station[i, .(station_id, longitude, latitude)]),
#                        dst = as.numeric(station[j, .(station_id, longitude, latitude)]),
#                        sp = TRUE)
#     route <- st_as_sf(route)
#     if (exists("shortest_path")) {
#       shortest_path <- rbind(shortest_path, route)
#     } else {
#       shortest_path <- route
#     }
#     cat(i, '-', j, ' ', sep = '')
#   }
# }
# 
# # save results and clean session
# saveRDS(shortest_path, 'data/shortest_path.rds')
# rm(i, j, route, nrow_station)
shortest_path <- readRDS('data/shortest_path.rds')
names(shortest_path) <- c('start_station_id', 'end_station_id', 'duration', 
                          'distance', 'geometry')


# Merge trips and paths  --------------------------------------------------

# average number of trips between all pairs of stations by hour

# Create id in both tables to be able to merge.
shortest_path$id <- paste(shortest_path$start_station_id, 
                          shortest_path$end_station_id, 
                          sep = '-')

trips$id <- paste(trips$start_station_id, 
                  trips$end_station_id, 
                  sep = '-')

# add geometry to trips
setDT(shortest_path)
trips <- merge(x = trips, 
               y = shortest_path[, .(id, geometry)],
               by = 'id',
               all = FALSE)

# creation of a size vector to pass to add_osm_object. 

#' 
#' This function changes the range of values of a numeric vector.
#' 
changeRange <- function(x, min, max){
  result <- (((x - min(x)) * (max - min)) / (max(x) - min(x))) + min
  return(result)
}

trips[, size := changeRange(count, min = 0.5, max = 4)]

# clena trips table and convert to sf object
trips <- trips[, .(start_station_id, end_station_id, hour_of_day, size, count, geometry)]
trips_sf <- st_as_sf(trips)

# clean session
rm(changeRange, shortest_path)


# Evolving map ------------------------------------------------------------

# create one frame by hour. Add paths by color because ad_osm_objects accepts
# only one value as color argument
img <- image_graph(width = 1200, height = 680, res = 96)
out <- lapply(X = sort(unique(trips_sf$hour_of_day)), 
              FUN = function(hour){
                cat(hour)
                # add path to the map by level of color
                data <- trips_sf[trips_sf$hour_of_day == hour, ]
                data <- data[!is.na(data$start_station_id), ]
                map <- austin_map
                for (size in unique(data$size)) {
                  data_size <- data[data$size == size,]
                  data_size <- data_size[!is.na(data_size$start_station_id), ]
                  map <- add_osm_objects(map = map,
                                         obj = data_size,
                                         col = 'dodgerblue3',
                                         size = size)
                }
                # add stations and hour to the plot
                map <- map +
                  geom_point(data = station, mapping = aes(x = longitude, y = latitude), 
                             colour = 'darkgoldenrod1') +
                  geom_text(data = data.frame(x = min(station$longitude) - 0.01,
                                              y = min(station$latitude) - 0.01,
                                              label = hour), 
                            mapping = aes(x = x, 
                                          y = y, 
                                          label = paste0(label, ':00')), 
                            colour = 'white',
                            size = 12)
                # add histogram of average number of trips by hour
                hist_data <- hourly_trips[, .(hour_of_day, count, color = factor(ifelse(hour_of_day == hour, 1, 0)))]
                hist_grob <- ggplotGrob(ggplot(data = hist_data, 
                                               mapping = aes(x = hour_of_day, y = count, fill = color)) + 
                                          geom_bar(stat = 'identity') + 
                                          labs(x = 'Hour of the day', 
                                               y = 'Number of trips') + 
                                          scale_fill_manual(breaks = c(0, 1), values = c('dodgerblue3', 'coral3')) + 
                                          theme(legend.position = 'none',
                                                plot.background = element_rect(fill = 'transparent', color = NA),
                                                panel.background = element_rect(fill = 'transparent', color = NA),
                                                axis.title = element_blank(),
                                                axis.text = element_blank(),
                                                axis.ticks = element_blank(),
                                                panel.grid = element_blank()))
                map <- map + 
                  labs(title = 'Bike trips by hour in Austin, Texas, US') + 
                  theme(plot.title = element_text(size = 20)) +
                  annotation_custom(grob = hist_grob, 
                                    xmin = min(station$longitude) + 0.04, 
                                    xmax = max(station$longitude) + 0.02, 
                                    ymin = max(station$latitude) + 0.005, 
                                    ymax = max(station$latitude) + 0.02)
                print(map)
              })
dev.off()
animation <- image_animate(img, fps = 2)
print(animation)

# save result as gif and clean session
image_write(animation, "output/hourly_trips.gif")
rm(out, img, animation)


