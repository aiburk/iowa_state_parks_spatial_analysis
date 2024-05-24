# Description: Code for pulling/analyzing state parks spatial data from
#              Iowa DNR's ArcGIS REST directory
# Author: Aidan Burk
# Date Created: 1/20/2024
# Date Edited: 5/22/2024
# Data: 'https://programs.iowadnr.gov/geospatial/rest/services'
# Notes:

# Packages

library(tidyverse)
library(sf)
library(arcpullr)
library(mapview)
library(units)
library(measurements)
library(grid)
library(gridExtra)
library(leafem)
library(leaflet)
library(tidycensus)
library(htmltools)
library(leaflegend)
library(nngeo)


# Load Data ---------------------------------------------------------------

## Data with IA state parks boundaries
ia_state_parks_trails_areas <- arcpullr::get_spatial_layer(
  "https://programs.iowadnr.gov/geospatial/rest/services/Recreation/State_Parks_Trails/MapServer/2")

## Data with IA state parks points & amenities features
ia_state_parks_trails_points <- arcpullr::get_spatial_layer(
  "https://programs.iowadnr.gov/geospatial/rest/services/Recreation/State_Parks_Trails/MapServer/3")


# Data Cleaning -----------------------------------------------------------

# Subset ia_state_parks_trails_area to keep only rows with TYPE
# 'State Forest', 'State Park', 'State Preserve', or 'State Recreation Area'
ia_state_parks_trails_areas <- ia_state_parks_trails_areas |>
  dplyr::filter(TYPE == 'State Forest' | TYPE == 'State Park' |
                  TYPE == 'State Preserve' | TYPE == 'State Recreation Area')

# Make invalid geometry valid
ia_state_parks_trails_areas$geoms <- sf::st_make_valid(
  ia_state_parks_trails_areas$geoms)
# Change all instances of 'Lake MacBride State Park' to 
# 'Lake Macbride State Park'
ia_state_parks_trails_areas <- ia_state_parks_trails_areas |>
  mutate(
    NAME = ifelse(str_detect(NAME, 'Lake MacBride State Park') == TRUE,
                  yes = 'Lake Macbride State Park',
                  no = NAME)
  )

# Load Tigris Iowa county boundaries
iowa_counties <- tigris::counties(state = 'IA')
# Match state park polygons to county boundaries
ia_state_parks_trails_areas <- sf::st_transform(
  ia_state_parks_trails_areas, crs = sf::st_crs(iowa_counties))
# Get the intersections of parks & counties
parks_counties_intersections <- sf::st_intersection(
  ia_state_parks_trails_areas, iowa_counties)


# Change intersections geometry type
parks_counties_intersections$geoms <- parks_counties_intersections$geoms |>
  sf::st_cast("MULTIPOLYGON")
# Rename county column
parks_counties_intersections <- parks_counties_intersections |>
  dplyr::rename(county = 'NAME.1')
# Filter out unneeded columns
parks_counties_intersections <-
  parks_counties_intersections[-c(8:34,36:41,43:54)]

# Get area values for parks
parks_counties_intersections$area <- sf::st_area(
  parks_counties_intersections$geoms)
# Get area sums for parks with multiple rows
parks_counties_int_sums <- parks_counties_intersections |>
  sf::st_drop_geometry() |>
  dplyr::group_by(NAME) |>
  dplyr::summarise(total_area = sum(area))
# Join these total area values to parks_counties_intersections
parks_counties_intersections <- full_join(
  parks_counties_intersections, parks_counties_int_sums, by = 'NAME')
# Resetting row indices
row.names(parks_counties_intersections) <- NULL
# Arranging by name
parks_counties_intersections <- parks_counties_intersections |>
  dplyr::arrange(NAME)

# Points NAME col has units (ex. 'Stephens State Forest - Unionville Unit')
# Reduce NAME col values to just the names excluding units
ia_state_parks_trails_points <- ia_state_parks_trails_points |>
  dplyr::mutate(
    NAME = ifelse(stringr::str_detect(NAME, ' - ') == TRUE,
                  yes = stringr::str_extract(NAME, '.+?(?= - )'),
                  no = NAME))



####    Combine Features    ####

# Get More_Info column grouped by NAME from points dataset
parks_links <- ia_state_parks_trails_points |> 
  st_drop_geometry() |>
  group_by(NAME) |>
  summarize(More_Info = paste(unique(More_Info), collapse= " ")) |>
  mutate(
    More_Info = str_squish(More_Info)
  )
# Create grouped df of parks_counties_intersections (1 row for each park),
# getting combined geometry
parks_combined_geoms <- parks_counties_intersections |>
  group_by(NAME) |>
  summarize(geometry = st_union(geoms)) |>
  ungroup()

# Create grouped df of parks_counties_intersections (1 row for each park),
# getting column summaries excluding geometry
parks_combined_features <- parks_counties_intersections |> 
  st_drop_geometry() |>
  # Make county name NA if a park's area is less than 1% of total area
  # (Assumed to be an intersection mistake & realistically belonging to the
  #  nearest other county)
  mutate(county =
           ifelse((set_units(area, NULL) > (.01*set_units(total_area, NULL))) 
                  == TRUE,
                  yes = str_replace_all(county, ' ', '-'),
                  no = NA)) |>
  group_by(NAME) |>
  # Get summed area & unique values for county, owner, manager, & type
  summarize(area = sum(area),
            county    = paste(unique(county), collapse=" "),
            OWNER     = paste(unique(OWNER), collapse=", "),
            MANAGER   = paste(unique(MANAGER), collapse=" "),
            TYPE      = paste(unique(TYPE), collapse=", ")) |>
  # Format county values
  mutate(county = 
           str_remove_all(county, 'NA') |> 
           str_squish() |>
           str_replace_all(" ", ", ") |>
           str_replace_all('-', ' '),
         TYPE = 
           ifelse(str_detect(TYPE, ',') == TRUE,
                  yes = str_extract(TYPE, '^([^,])+'),
                  no = TYPE)) |>
  ungroup()
# Join with More_Info & geometry columns
ia_state_parks_combined <- parks_combined_features |> 
  left_join(parks_links) |>
  full_join(parks_combined_geoms) |>
  st_as_sf() |> # Convert to sf
  st_transform(crs = 4326)


# Recuding ia_state_parks_trails points to relevant columns
ia_state_parks_trails_points <-
  ia_state_parks_trails_points[c(40,5:27,29:35)]

# This dataset's features of interests are its amenities
# columns rather than geometry
# Creating new df from ia_state_parks_trails_points with geometry dropped
parks_amenities <- ia_state_parks_trails_points |>
  sf::st_drop_geometry()
# Changing amenities columns' type from character to integer
parks_amenities[c(3:31)] <- parks_amenities[c(3:31)] |>
  lapply(as.integer)
# Changing NA amenity col factors to 0
parks_amenities[c(3:31)][is.na(parks_amenities[c(3:31)])] <- 0
# Getting sums of amenity cols by NAME, then mutating to change
# values to 1 if greater than 0
parks_amenities <- parks_amenities |>
  group_by(NAME) |>
  summarise(across(pavedBikeT:paddleBoat, sum)) |>
  dplyr::mutate(across(pavedBikeT:paddleBoat, ~ifelse(. > 0, yes = 1, no = 0)))



# Joining amenities features to the combined geometry dataframe
ia_state_parks_df <- dplyr::left_join(
  ia_state_parks_combined, parks_amenities, by = 'NAME')
# Reshaping data to create amenities column with the names of cols where
# row values are 1 (TRUE)
ia_state_parks_df <- ia_state_parks_df |> sf::st_drop_geometry() |>
  pivot_longer(cols = c(9:36)) |>
  group_by(NAME) |>
  summarise(amenities = toString(name[as.logical(value)])) |>
  right_join(ia_state_parks_df) |>
  select(names(ia_state_parks_df), everything())
# Mutating to adjust the spelling/wording for each amenity, then removing
# instances of NA in amenities
ia_state_parks_df <- ia_state_parks_df |>
  dplyr::mutate(
    amenities = stringr::str_replace_all(amenities,
    c(
      'pavedBikeT' = 'Paved Bike Trail',
      'fishing'    = 'Fishing',
      'hiking'     = 'Hiking',
      'running'    = 'Running',
      'paddling'   = 'Paddling',
      'sailing'    = 'Sailing',
      'waterski'   = 'Waterski',
      'discgolf'   = 'Disc Golf',
      'horseshoe'  = 'Horseshoe',
      'wildlifeVi' = 'Wildlife Viewing',
      'playground' = 'Playground',
      'rockClimbi' = 'Rock Climbing',
      'swimming'   = 'Swimming',
      'horsebackR' = 'Horseback Riding',
      'mountainBi' = 'Mountain Biking',
      'shootingSp' = 'Shooting Sports',
      'golf'       = 'Golf',
      'camping'    = 'Camping',
      'walking'    = 'Walking',
      'geocaching' = 'Geocaching',
      'picnic'     = 'Picnic',
      'windsurfin' = 'Windsurfing',
      'iceFishing' = 'Ice Fishing',
      'sledding'   = 'Sledding',
      'hunting'    = 'Hunting',
      'iceSkating' = 'Ice Skating',
      'trapping'   = 'Trapping',
      'xcSkiing'   = 'Cross-Country Skiing',
      'paddleBoat' = 'Paddle Boat'
    ))) |>
  dplyr::mutate(
    amenities = ifelse(stringr::str_detect(amenities, 'NA,') == TRUE,
                       yes = '', no = amenities))
# Converting ia_state_parks_df to sf object
ia_state_parks_df <- ia_state_parks_df |>
  sf::st_as_sf() |>
  st_transform(crs = 4326)







# Data Visualization ------------------------------------------------------

#### Map of parks_counties_intersections geometry ####

parks_counties_int_map <-
  parks_counties_intersections[c(2,4,7,9,10)] |>
  mapview::mapview(zcol = 'TYPE',
                   layer.name = 'parks_counties_intersections - TYPE')
parks_counties_int_map



####     Map of ia_state_parks_combined geometry      ####

state_parks_combine_map <-
  ia_state_parks_combined[c(2,5:7)] |>
  mapview::mapview(zcol = 'TYPE',
                   layer.name = 'ia_state_parks_combined - TYPE')
state_parks_combine_map



####        State Park area by county plot        ####

# Summing area by county
parks_county_area_sums <- parks_counties_intersections |>
  sf::st_drop_geometry() |>
  group_by(county) |>
  summarise(total_area = sum(area))
# Dropping units in area column
parks_county_area_sums$total_area <-
  units::drop_units(parks_county_area_sums$total_area)
# Converting area from square meters to square miles
parks_county_area_sums$total_area <-
  parks_county_area_sums$total_area |>
  measurements::conv_unit('m2', 'mi2')
# Plotting park area by county (total_area > 2)
subset(parks_county_area_sums, total_area > 4) |>
  ggplot(aes(reorder(county, total_area),total_area)) +
  geom_bar(stat = 'identity', fill = 'lightblue', color = 'lightblue4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("county") +
  ylab("total state parks area (square miles)") +
  ggtitle("Total area of state parks by county (Counties with area > 4 mi^2)") +
  geom_text(aes(label = round(total_area,1)), nudge_y = 1.3) +
  ylim(0,20) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#### Grid of 4 Area by State Parks plots, 1 for each TYPE factor ####

# Reformat area column
ia_state_parks_combined$area <-
  units::drop_units(ia_state_parks_combined$area) |>
  # Convert from square meters to square miles
  measurements::conv_unit('m2', 'mi2')
# Plotting area by State Forest
forest_plot <-
  subset(ia_state_parks_combined, TYPE == 'State Forest') |>
  ggplot(aes(reorder(NAME, area), area)) +
  geom_bar(stat = 'identity', fill = 'mediumpurple', color = 'mediumpurple4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("state forest") +
  ylab("area (square miles)") +
  ggtitle("State Forests by Area (mi^2)") +
  geom_text(aes(label = round(area,1)), nudge_y = 2) +
  ylim(0,30) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Plotting area by State Park
park_plot <-
  subset(ia_state_parks_combined, TYPE == 'State Park' & area > 2) |>
  ggplot(aes(reorder(NAME, area), area)) +
  geom_bar(stat = 'identity', fill = 'cadetblue3', color = 'cadetblue4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("state park") +
  ylab("area (square miles)") +
  ggtitle("State Parks by Area (area > 2 mi^2)") +
  geom_text(aes(label = round(area,1)), nudge_y = .3) +
  ylim(0,5) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Plotting area by State Preserve
preserve_plot <-
subset(ia_state_parks_combined, TYPE == 'State Preserve' & area > .2) |>
  ggplot(aes(reorder(NAME, area), area)) +
  geom_bar(stat = 'identity', fill = 'palegreen3', color = '#6C8400') +
  theme(axis.text = element_text(size = 10)) +
  xlab("state preserve") +
  ylab("area (square miles)") +
  ggtitle("State Preserves by Area (area > 0.2 mi^2)") +
  geom_text(aes(label = round(area,1)), nudge_y = .1) +
  ylim(0,1.5) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Plotting area by State Recreation Area
rec_plot <-
  subset(ia_state_parks_combined, TYPE == 'State Recreation Area' & area > .2) |>
  ggplot(aes(reorder(NAME, area), area)) +
  geom_bar(stat = 'identity', fill = 'yellow', color = 'lightgoldenrod4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("state recreation area") +
  ylab("area (square miles)") +
  ggtitle("State Recreation Areas by Area (area > 0.2 mi^2") +
  geom_text(aes(label = round(area,1)), nudge_y = 1) +
  ylim(0,12) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Arranging the 4 plots on a grid
gridExtra::grid.arrange(forest_plot,
                        park_plot,
                        preserve_plot,
                        rec_plot,
                        ncol =2)

# Area by State Parks plot including all TYPE factors
subset(ia_state_parks_combined, area > 3) |>
  ggplot(aes(reorder(NAME, area), area)) +
  geom_bar(stat = 'identity', fill = 'lightblue', color = 'lightblue4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("state park") +
  ylab("area (square miles)") +
  ggtitle("State Parks (All types) by Area (area > 3 mi^2)") +
  geom_text(aes(label = round(area,1)), nudge_y = 2) +
  ylim(0,30) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#### Choropleth map of counties by total state park area ####

# Making df with all Iowa counties & their geometry, joining with
# total state park areas, & changing NA county areas to 0
counties_area_geom <- iowa_counties[c(5)] |>
  rename('county' = 'NAME') |>
  left_join(parks_county_area_sums) |>
  mutate(total_area = ifelse(is.na(total_area) == TRUE,
                             yes = 0,
                             no  = total_area))
# Create html labels for total area choropleth map
labels_county_park_area <- counties_area_geom$county |>
  lapply(htmltools::HTML)
# Create total area palette for total area choropleth map
pal_county_park_area <- colorNumeric(
  palette = 'Blues',
  domain = counties_area_geom$total_area,
)

# Create choropleth map with state park polygons overlay
leaflet(options = leafletOptions(zoomControl = TRUE,
                                 zoomSnap = 0.25,
                                 zoomDelta = 1)) |>
  leaflet::addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = counties_area_geom, 
    fillColor = ~pal_county_park_area(total_area), 
    fillOpacity = 0.6,
    label = labels_county_park_area,
    labelOptions = labelOptions(noHide = TRUE, 
                                textOnly = TRUE, 
                                direction = 'center',
                                style = list("font-size" = "10px")),
    color = "black", weight = 1
  ) |>
  addLegend(
    pal = pal_county_park_area, values = counties_area_geom$total_area, 
    opacity = 0.95,
    title = 'Total area of state parks<br>(square miles)'
  ) |>
  addPolygons(
    data = ia_state_parks_combined,
    color = 'black', weight = 1,
    fillColor = 'gainsboro', fillOpacity = 1
  )


#### Map of state parks with one layer for each amenity ####

# List of amenity cols in ia_state_parks_df that have more than .15 meean
amenity_col_list <- ia_state_parks_df[9:37] |> sf::st_drop_geometry() |>
  lapply(mean, na.rm = TRUE)
amenity_col_list <- amenity_col_list[amenity_col_list > .15] |> names()
# List with amenities from amenity_col_list as keys & colors as values
colors_key <- setNames(c('aquamarine',
                         'skyblue',
                         'thistle',
                         'plum',
                         'palegreen',
                         'goldenrod',
                         'darkseagreen',
                         'moccasin',
                         'cadetblue',
                         'greenyellow',
                         'lightcyan',
                         'lightpink',
                         'lightsalmon',
                         'mediumpurple',
                         'rosybrown',
                         'mistyrose',
                         'mediumaquamarine',
                         'palevioletred',
                         'yellow',
                         'red'),
                       amenity_col_list)
# Creating base map
amenity_layer_map <- leaflet() |>
  leaflet::addProviderTiles(providers$CartoDB.Positron)
# Looping through amenity_col_list & ia_state_parks_df to add a grouped
# polygon layer for each amenity to the map
for (i in 1:length(amenity_col_list)) {
  for (col in 1:ncol(ia_state_parks_df)) {
    if (identical(colnames(ia_state_parks_df[,col] |>
                            st_drop_geometry()),
                 amenity_col_list[i]) == TRUE) {
  amenity_layer_map <- amenity_layer_map |>
    addPolygons(data = subset(ia_state_parks_df,
                              ia_state_parks_df[col] |>
                                st_drop_geometry() == 1),
                group = amenity_col_list[i],
                fillColor = colors_key[amenity_col_list[i]] |> unname(),
                weight = 1,
                color = 'black',
                fillOpacity = .8,
                highlightOptions = highlightOptions(weight = 2,
                                                    color = 'blue',
                                                    fillOpacity = .8,
                                                    bringToFront = TRUE),
                label = ia_state_parks_df$NAME |>
                  subset(ia_state_parks_df[col] |>
                           st_drop_geometry() == 1))
    }}}
# Adding overlay layers control to map
amenity_layer_map_check <- amenity_layer_map |>
  addLayersControl(overlayGroups = c(amenity_col_list))
# Adding base layers control to map
amenity_layer_map_base <- amenity_layer_map |>
  addLayersControl(baseGroups = c(amenity_col_list))

amenity_layer_map_check |> hideGroup(amenity_col_list)
amenity_layer_map_base



####       State parks within place buffers map       ####

# Get place sf with population
places <- tidycensus::get_acs(geography = "place",
                              variables = "B01001_001",
                              state = "IA",
                              geometry = TRUE) |>
  st_transform(crs = 3417) # transform to get foot unit

# Extract cities from NAME without extraneous parts
places <- places |>
  mutate(
    NAME =
      ifelse(str_detect(NAME, 'city') == TRUE,
             yes = str_extract(NAME, '.+?(?= city)'),
             no = str_extract(NAME, '.+?(?= CDP)'))
  )

# Buffer places using st_buffer - 30 miles
places_buffer <- places |>
  st_buffer(dist = 158400) |> # 30 mi = 158400 ft
  st_transform(crs = 4326) # Convert back to 4326
# Transform ia_state_parks_trails_areas to 4326
ia_state_parks_trails_areas <- ia_state_parks_trails_areas |>
  st_transform(crs = 4326)
# Spatial join of places and state parks
places_parks_join <- st_join(places_buffer, ia_state_parks_trails_areas)
# Summarize count of number of parks in each place buffer & get centroids
places_parks_count <- places_parks_join |>
  group_by(NAME.x) |>
  summarize(num_parks = length(unique(str_to_lower(na.omit(NAME.y)))),
            pop = mean(estimate), # Including population
            GEOID = unique(GEOID)) |> #Including GEOID
  filter(pop > 50) |> # Filter out places with population < 50
  mutate(centroid = st_centroid(geometry)) |>
  st_set_geometry('centroid') # Set sf geometry to centroid points
# Create html labels for places with num_parks leaflet map
labels_numparks <- paste(
  "<strong>", places_parks_count$NAME.x,
  "</strong><br>Population:", places_parks_count$pop,
  "<br># of state parks within 30 miles:", places_parks_count$num_parks) |>
  lapply(htmltools::HTML)

# Create num_parks palette for places with num_parks leaflet map
pal_numparks <- colorNumeric(
  palette = 'Greens',
  domain = places_parks_count$num_parks
)
# Create leaflet map of places by pop & num_parks
leaflet(places_parks_count) |>
  leaflet::addProviderTiles(providers$CartoDB.Positron) |>
  leaflegend::addSymbolsSize(values = ~sqrt(pop)*45,
                             shape = 'circle',
                             color = 'black',
                             fillColor = ~pal_numparks(num_parks),
                             fillOpacity = .9,
                             label = labels_numparks,
                             baseSize = 5) |>
  addLegend(pal = pal_numparks, values = ~num_parks,
            opacity = .9,
            position = 'bottomright',
            title = 'Number<br>of Parks') |>
  leaflegend::addLegendSize(values = ~sqrt(pop)*45,
                            title = 'Population',
                            shape = 'circle',
                            color = 'black',
                            fillColor = 'white',
                            opacity = .9,
                            baseSize = 5,
                            position = 'bottomleft',
                            stacked = TRUE,
                            # Format population back to 
                            # unscaled values in legend
                            numberFormat = function(x) {
                              prettyNum((x/45)^(2),
                                        big.mark = ',',
                                        scientific = FALSE,
                                        digits = 1)},
                            breaks = 5) 



####      Average distance to nearest 5 parks map       ####

# Get distance of closest 5 state parks for each place (list)
place_parks_dist <- nngeo::st_nn(st_transform(places, crs = 4326),
                                 ia_state_parks_combined,
                                 k = 5, returnDist = TRUE)
# Get mean distance for each place
places <- places |>
  mutate(
    avg_dist = place_parks_dist$dist |>
      map(mean) |>
      as.numeric() |>
      measurements::conv_unit('m', 'mi') # Convert from meters to miles
  )

# Join mean distances to the data with centroids
places_parks_count <- places_parks_count |>
  left_join(places[c(2,7)] |> 
              st_drop_geometry() |>
              rename('NAME.x' = 'NAME'))

# Create html labels for places with avg_dist leaflet map
labels_avgdist <- paste(
  "<strong>", places_parks_count$NAME.x,
  "</strong><br>Population:", places_parks_count$pop,
  "<br>Average distance (mi) to nearest 5 parks:", 
  places_parks_count$avg_dist |>
                                              round(2)) |>
  lapply(htmltools::HTML)

# Create avg_dist palette for places with avg_dist leaflet map
pal_avgdist <- colorNumeric(
  palette = 'PuBu',
  domain = places_parks_count$avg_dist,
  reverse = TRUE
)

# Create leaflet map of places by pop & avg_dist
leaflet(places_parks_count) |>
  leaflet::addProviderTiles(providers$CartoDB.Positron) |>
  leaflegend::addSymbolsSize(values = ~sqrt(pop)*45,
                             shape = 'circle',
                             color = 'black',
                             fillColor = ~pal_avgdist(avg_dist),
                             fillOpacity = .9,
                             label = labels_avgdist,
                             baseSize = 5) |>
  addLegend(pal = pal_avgdist, values = ~avg_dist,
            opacity = .9,
            position = 'bottomright',
            title = 'Average distance<br>(in miles) to<br>nearest 5 parks') |>
  leaflegend::addLegendSize(values = ~sqrt(pop)*45,
                            title = 'Population',
                            shape = 'circle',
                            color = 'black',
                            fillColor = 'white',
                            opacity = .9,
                            baseSize = 5,
                            position = 'bottomleft',
                            stacked = TRUE,
                            # Format population back to 
                            # unscaled values in legend
                            numberFormat = function(x) {
                              prettyNum((x/45)^(2),
                                        big.mark = ',',
                                        scientific = FALSE,
                                        digits = 1)},
                            breaks = 5) 


####     Layered map that combines both previous maps:         ####
###     1st layer for number of parks within 30 miles,          ###
###     2nd layer for average distance to nearest 5 parks       ###

leaflet(places_parks_count) |>
  leaflet::addProviderTiles(providers$CartoDB.Positron) |>
  leaflegend::addSymbolsSize(values = ~sqrt(pop)*45,
                             shape = 'circle',
                             color = 'black',
                             fillColor = ~pal_numparks(num_parks),
                             fillOpacity = .9,
                             label = labels_numparks,
                             baseSize = 5,
                             group = 'Number of Parks') |>
  leaflegend::addSymbolsSize(values = ~sqrt(pop)*45,
                             shape = 'circle',
                             color = 'black',
                             fillColor = ~pal_avgdist(avg_dist),
                             fillOpacity = .9,
                             label = labels_avgdist,
                             baseSize = 5,
                             group = 'Average Distance') |>
  addLegend(pal = pal_numparks, values = ~num_parks,
            opacity = .9,
            position = 'bottomright',
            title = 'Number<br>of Parks',
            group = 'Number of Parks') |>
  addLegend(pal = pal_avgdist, values = ~avg_dist,
            opacity = .9,
            position = 'bottomright',
            title = 'Average distance<br>(in miles) to<br>nearest 5 parks',
            group = 'Average Distance') |>
  leaflegend::addLegendSize(values = ~sqrt(pop)*45,
                            title = 'Population',
                            shape = 'circle',
                            color = 'black',
                            fillColor = 'white',
                            opacity = .9,
                            baseSize = 5,
                            position = 'bottomleft',
                            stacked = TRUE,
                            # Format population back to 
                            # unscaled values in legend
                            numberFormat = function(x) {
                              prettyNum((x/45)^(2),
                                        big.mark = ',',
                                        scientific = FALSE,
                                        digits = 1)},
                            breaks = 5) |>
  addLayersControl(overlayGroups = c('Number of Parks', 'Average Distance'),
                   options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup('Average Distance')



####     Top/Bottom Average distance bar plot grids      ####

# Top plot for population < 2.5k
dist_plot_2.5_high <- places_parks_count |>
  subset(pop < 2500) |>
  top_n(10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'lightgoldenrod2', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: < 2,500',
       subtitle = 'Highest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Top plot for population 2.5k - 10k
dist_plot_2.5_to_10_high <- places_parks_count |>
  subset(pop >= 2500 & pop < 10000) |>
  top_n(10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'seagreen3', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: 2,500 - 10,000',
       subtitle = 'Highest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Top plot for population 10k-50k
dist_plot_10_to_50_high <- places_parks_count |>
  subset(pop >= 10000 & pop < 50000) |>
  top_n(10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: 10,000 - 50,000',
       subtitle = 'Highest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Arrange top plots on a grid
dist_grid_highest <- gridExtra::grid.arrange(
  dist_plot_2.5_high,
  dist_plot_2.5_to_10_high,
  dist_plot_10_to_50_high,
  ncol = 3,
  top = grid::textGrob(
    'Highest of Average distance (mi) to nearest 5 Iowa state parks by City')
  )

# Bottom plot for population < 2.5k
dist_plot_2.5_low <- places_parks_count |>
  subset(pop < 2500) |>
  top_n(-10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'lightgoldenrod2', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: < 2,500',
       subtitle = 'Lowest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Bottom plot for population 2.5k - 10k
dist_plot_2.5_to_10_low <- places_parks_count |>
  subset(pop >= 2500 & pop < 10000) |>
  top_n(-10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'seagreen3', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: 2,500 - 10,000',
       subtitle = 'Lowest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Bottom plot for population 10k-50k
dist_plot_10_to_50_low <- places_parks_count |>
  subset(pop >= 10000 & pop < 50000) |>
  top_n(-10, avg_dist) |> 
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) +
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Population: 10,000 - 50,000',
       subtitle = 'Lowest 10') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Arrange bottoms plots on a grid
dist_grid_lowest <- gridExtra::grid.arrange(
  dist_plot_2.5_low,
  dist_plot_2.5_to_10_low,
  dist_plot_10_to_50_low,
  ncol =3,
  top = grid::textGrob(
    'Lowest of Average distance (mi) to nearest 5 Iowa state parks by City'))

# Plot for Population >50k (All cities included, top to bottom)
dist_plot_50 <- places_parks_count |>
  subset(pop >= 50000) |>
  ggplot(aes(reorder(NAME.x, avg_dist), avg_dist)) +
  geom_bar(stat = 'identity', fill = 'plum', color = 'snow1') +
  theme(axis.text = element_text(size = 10)) + 
  labs(x = 'city',
       y = 'average distance (mi)',
       title = 'Average distance to nearest 5 Iowa state parks by City',
       subtitle = 'Population: > 50,000 - highest to lowest') +
  geom_text(aes(label = round(avg_dist,1)), nudge_y = 4) +
  ylim(0,60) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Arrange the high & low grids on a grid
grid.arrange(dist_grid_highest, 
             dist_grid_lowest, 
             ncol = 1)
dist_plot_50



####   Average distance histogram with normal curve & vlines   ####

places_parks_count |>
  ggplot(aes(x = avg_dist)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 2,
                 fill = 'paleturquoise3', color ='#e9ecef') +
  # Plot density curve
  stat_function(fun = dnorm, 
                args = list(mean = mean(places_parks_count$avg_dist), 
                            sd = sd(places_parks_count$avg_dist))) +
  # Rescale y axis
  scale_y_continuous('count', 
                     breaks = round(seq(0,90,30) / (2 * 948),3), 
                     labels = seq(0,90,30)) +
  ggtitle('Histogram - Average distance (mi) to nearest 5 Iowa state parks') +
  xlab('Average distance (mi)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add a vertical line for each city with pop >= 50,000
  geom_vline(data = places_parks_count |>
               subset(pop >= 50000),
             mapping = aes(xintercept = avg_dist),
             linetype = 'dashed', color = 'grey50') +
  # Add a label for each vertical line
  geom_text(data = places_parks_count |>
              subset(pop >= 50000 & NAME.x != 'Council Bluffs'),
            mapping = aes(x = avg_dist, label = NAME.x, y = Inf),
            angle = 90, vjust = -.6, hjust = 1.2, size = 8/.pt) +
  # Council Bluffs needs to be adjusted differently due to overlap
  geom_text(data = places_parks_count |>
              subset(NAME.x == 'Council Bluffs'),
            mapping = aes(x = avg_dist, label = NAME.x, y = Inf),
            angle = 90, vjust = 1.2, hjust = 1.2, size = 8/.pt)

####   Number of Parks histogram with normal curve & vlines   ####

# num_parks is non-continuous with some cities sharing the same value, 
# so lines/labels will be grouped
# Create summarized table of top cities pasted together grouped by num_parks
num_parks_top_cities_labels <- places_parks_count |>
  st_drop_geometry() |>
  subset(pop >= 50000) |>
  group_by(num_parks) |>
  # Make West Des Moines multiline to save space
  mutate(NAME.x = ifelse(str_detect(NAME.x, 'West Des Moines') == TRUE,
                         yes = 'West\nDes Moines',
                         no = NAME.x)) |>
  summarise(Names = paste(unique(NAME.x), collapse= ",\n"))

# Plot histogram
places_parks_count |>
  ggplot(aes(x = num_parks)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 1,
                 fill = 'paleturquoise3', color ='#e9ecef') +
  # Plot density curve
  stat_function(fun = dnorm, 
                args = list(mean = mean(places_parks_count$num_parks), 
                            sd = sd(places_parks_count$num_parks))) +
  # Rescale y axis
  scale_y_continuous('count', 
                     breaks = round(seq(0,150,50) / (1 * 948),3), 
                     labels = seq(0,150,50)) +
  ggtitle('Histogram - Number of Iowa state parks within 30 miles') +
  xlab('Number of parks') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add vertical lines for cities with pop >= 50,000
  geom_vline(data = num_parks_top_cities_labels,
             mapping = aes(xintercept = num_parks + .1),
             linetype = 'dashed', color = 'grey50') +
  # Add a label for each vertical line
  geom_text(data = num_parks_top_cities_labels,
            mapping = aes(x = num_parks, label = Names, y = Inf, vjust = 1),
            hjust = 1, size = 8/.pt)


# Model Analysis: State Parks & Population Variables -----------------------

####    Get places_parks_count variables for all    #### 
####          places rather than pop > 50           ####

# Summarize count of number of parks in each place buffer & get centroids
places_parks_count_all <- places_parks_join |>
  group_by(NAME.x) |>
  summarize(num_parks = length(unique(str_to_lower(NAME.y))),
            pop = mean(estimate), # Including population
            GEOID = unique(GEOID)) |> #Including GEOID
  mutate(centroid = st_centroid(geometry)) |>
  st_set_geometry('centroid') # Set sf geometry to centroid points
# Join mean distances to the data with centroids
places_parks_count_all <- places_parks_count_all |>
  left_join(places[c(2,7)] |> 
              st_drop_geometry() |>
              rename('NAME.x' = 'NAME'))


####    Get Population Variables    ####

## Race

# Vector of variables
raceth <- c(
  "B03002_001", # Total Population
  "B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009", "B03002_012" # Race & Ethnicity Categories
)
# Pull wide data from Tidycensus
race_data <- get_acs(geography = "place",
                     state = "IA",
                     variables = raceth,
                     output = "wide",
                     year = 2022)
# Get racial group population percentages
race_data <- race_data |>
  mutate(race_white_pct = B03002_003E / B03002_001E,
         race_nonwhite_pct = 1 - race_white_pct,
         race_hisp_pct = B03002_012E / B03002_001E,
         race_black_pct = B03002_004E / B03002_001E,
         race_nat_pct = B03002_005E / B03002_001E,
         race_asian_pct = B03002_006E / B03002_001E,
         race_pacisle_pct = B03002_007E / B03002_001E,
         race_other_pct = B03002_008E / B03002_001E,
         race_multiple_pct = B03002_009E / B03002_001E)
# Filter out unwanted vars
race_data <- race_data |>
  select(GEOID, NAME, race_white_pct, race_nonwhite_pct, race_hisp_pct,
         race_black_pct, race_nat_pct, race_asian_pct, race_pacisle_pct,
         race_other_pct, race_multiple_pct)

# Join
iowa_places_vars <- left_join(places_parks_count_all |> st_drop_geometry(),
                              race_data, by = join_by(GEOID))

## Median Household Income

# Pull data from Tidycensus
median_household <- get_acs(geography = "place",
                            state = "IA",
                            year = 2022,
                            variables = "B19013_001") |>
  # Rename variables
  rename(median_income_estimate = estimate,
         median_income_moe = moe)

# Join
iowa_places_vars <- left_join(iowa_places_vars, 
                              median_household, by = join_by(GEOID))

## Education Status

# Pull wide education data 
education_data <- get_acs(geography = "place",
                          state = "IA",
                          year = 2022,
                          table = "B15003",
                          output = "wide") |>
# Calculate percent of high school and college grads
  mutate(
    ed_hs_grad_pct = (B15003_017E + B15003_018E + B15003_019E + B15003_020E + 
                        B15003_021E + B15003_022E + B15003_023E + 
                        B15003_024E + B15003_025E) / B15003_001E,
    ed_bach_grad_pct = (B15003_022E + B15003_023E + 
                          B15003_024E + B15003_025E) / B15003_001E
    ) |>
# Filter out unwanted vars
  select(GEOID, NAME, ed_hs_grad_pct, ed_bach_grad_pct)

# Join
iowa_places_vars <- left_join(iowa_places_vars, education_data, 
                              by = join_by(GEOID))

## Population under the age of 18 data

# Pull population under 18 data
under_18_data <- get_acs(geography = "place",
                         state = "IA",
                         year = 2022,
                         table = "B09001",
                         output = "wide") |>
  select(GEOID, NAME, B09001_001E) |>
  rename('under_18' = 'B09001_001E')

# Join
iowa_places_vars <- left_join(iowa_places_vars, under_18_data, 
                              by = join_by(GEOID)) |>
  mutate(under_18_pct = under_18 / pop) # Calculate percent under 18

## Percent of Households with income under $25,000

# Vector of variables
household_income_brackets <- c("B19001_001", #total
                               "B19001_002", "B19001_003", "B19001_004",
                               "B19001_005") # 24,999
# Pull wide data
poverty_data <- get_acs(geography = "place",
                        state = "IA",
                        year = 2022,
                        variables = household_income_brackets,
                        output = "wide") |>
  # Calculate percentage
  mutate(income_percent_below_25k = (B19001_002E + B19001_003E +
                                       B19001_003E + B19001_004E +
                                       B19001_005E) / B19001_001E) |>
  select(GEOID, NAME, income_percent_below_25k) # Filter out unwanted vars

# Join
iowa_places_vars <- left_join(iowa_places_vars, poverty_data,
                              by = join_by(GEOID))

# Filter out unwanted vars in iowa_places_vars
iowa_places_vars <- iowa_places_vars |>
  select(GEOID, NAME.x, num_parks, avg_dist, pop, 
         race_white_pct, race_nonwhite_pct, race_hisp_pct, race_black_pct,
         race_nat_pct, race_asian_pct, race_pacisle_pct, race_other_pct,
         race_multiple_pct, 
         median_income_estimate, 
         ed_hs_grad_pct, ed_bach_grad_pct,
         under_18_pct, 
         income_percent_below_25k) |>
  rename(NAME = 'NAME.x')


####    Run Models    ####

## Compile list of bivariate lms

# Name variables

explanatory_vars <- c("race_white_pct", "race_nonwhite_pct", "race_hisp_pct",
                      "race_black_pct", "race_nat_pct", "race_asian_pct", 
                      "race_pacisle_pct", "race_other_pct", 
                      "race_multiple_pct", # race
                      "median_income_estimate", # median household income
                      "ed_hs_grad_pct", "ed_bach_grad_pct", #education
                      "under_18_pct", # under 18
                      "income_percent_below_25k") # low income

response_var <- "avg_dist"

# Map models to list

models <- map(explanatory_vars, ~ lm(reformulate(.x, response = response_var), 
                                     data = iowa_places_vars |> na.omit()))

names(models) <- explanatory_vars


multi_model <- lm(avg_dist ~ ed_bach_grad_pct + pop
                  + income_percent_below_25k + race_black_pct +
                    race_hisp_pct + median_income_estimate + under_18_pct, 
                  data = iowa_places_vars |> na.omit())
summary(multi_model)

map(models, summary)



# Write Data --------------------------------------------------------------

write_rds(ia_state_parks_df, "iowa_state_parks_df.rds")
