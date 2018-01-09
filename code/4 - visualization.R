
# Map the trips. Visualize the trips on a map. 

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('osmplotr') # extracting spatial data 
library('ggplot2') # plots
library('osrm') # shortest path 


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

# get the Austin map, bbox from http://boundingbox.klokantech.com/
austin_bbox <- get_bbox(latlon = c(-97.938468, 30.098609, -97.561402, 30.516919))

# get the streets geometry
austin_streets <- extract_osm_objects(key = 'highway', bbox = austin_bbox, return_type = 'line', sf = TRUE, geom_only = TRUE)

# base map
austin_map <- osm_basemap(bbox = austin_bbox, bg = 'gray20')
austin_map <- add_osm_objects(map = austin_map, obj = austin_streets, col = 'gray40')
print(austin_map)


# Shortest path between bike stations -------------------------------------

# the osm api is used to have the shortest pathh between all stations. 

# sortest paths between all pairs of stations.
# nrow_station <- nrow(station)
# for (i in 1:(nrow_station - 1)) {
#   for (j in (i+1):nrow_station) {
#     route <- osrmRoute(src = as.numeric(station[i, .(station_id, longitude, latitude)]),
#                        dst = as.numeric(station[j, .(station_id, longitude, latitude)]),
#                        sp = TRUE)
#     route <- sf::st_as_sf(route)
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

# add paths to the map
austin_map <- add_osm_objects(map = austin_map, obj = shortest_path, col = 'dodgerblue')

# plot the map
print(austin_map)


shortest_path <- osrmRoute(src = as.numeric(station[1, .(station_id, longitude, latitude)]),
                           dst = as.numeric(station[2, .(station_id, longitude, latitude)]),
                           sp = TRUE)




