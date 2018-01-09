
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

# import stations coordinates
station <- readRDS('data/processed/stations.rds')


# Austin map --------------------------------------------------------------

# plot a basic Austin map with the streets and the bike stations

# austin bbox dereived from min and max of station longitude and latitude
austin_bbox <- get_bbox(latlon = c(min(station$longitude) - 0.02, 
                                   min(station$latitude) - 0.02, 
                                   max(station$longitude) + 0.02, 
                                   max(station$latitude) + 0.02))

# get the streets geometry
austin_streets <- extract_osm_objects(key = 'highway', 
                                      bbox = austin_bbox, 
                                      return_type = 'line', 
                                      sf = TRUE, 
                                      geom_only = TRUE)

# base map
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

# creation of a color vector to pass to add_osm_object. First count is
# discretized and then a color palette is used to as levels.
createPalette <- colorRampPalette(c("dodgerblue", "white"))
trips$color <- discretize(x = trips$count, 
                          method = 'interval', 
                          categories = 10, 
                          labels = createPalette(10))

# clena trips table and convert to sf object
trips <- trips[, .(start_station_id, end_station_id, hour_of_day, color, geometry)]
trips <- st_as_sf(trips)

# clean session
rm(createPalette, shortest_path)


# Evolving map ------------------------------------------------------------

# create one frame by hour. Add paths by color because ad_osm_objects accepts
# only one value as color argument
img <- image_graph(600, 340, res = 96)
lapply(X = unique(trips$hour_of_day), 
       FUN = function(hour){
         data <- trips[trips$hour_of_day == hour, ]
         map <- austin_map
         for (color in unique(data$color)) {
           object <- data[data$color == color,]
           map <- add_osm_objects(map = map,
                                  obj = object,
                                  col = color)
         }
         print(map)
       })
dev.off()
animation <- image_animate(img, fps = 2)
print(animation)

hour <- 1
data <- trips[trips$hour_of_day == hour, ]
# map <- austin_map
for (color in unique(data$color)) {
  austin_map <- add_osm_objects(map = austin_map,
                                obj = data[data$color == color,],
                                col = color)
}
print(austin_map)

# some color are not present for some hour so a condition should be added before
# add_osm_objects

# add paths by color because ad_osm_objects accepts only one value as color
# argument
for (color in unique(trips$color)) {
  austin_map <- add_osm_objects(map = austin_map,
                                obj = trips[trips$color == color,],
                                col = color)
}

# plot the map
print(austin_map)
