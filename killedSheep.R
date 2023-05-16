rovbasedata <- readr::read_csv("data/rawData/rovbaseSample.csv")
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>% 
  select(NAME_2,geometry)

# select attack id, date and coordinates (check if coordinate system is the same as in scandinavia)
killedsheepclean <- rovbasedata %>% select("HändelseID", "Skada på", "Fynddatum", "Kommune",
                                          "Nord (RT90)", "Öst (RT90)") %>%
  na.omit() %>%
  filter(`Skada på` %in% c("Får")) %>%
  separate(Fynddatum, c("Year")) %>%
  filter(Year>1998)

# read as spatial points and change coordinate to RT90
killedsheepSpatial <- st_as_sf(x = killedsheepclean,
                           coords = c("Öst (RT90)", "Nord (RT90)"),
                           crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>% 
  select(HändelseID, geometry)

# intersect with scandinavia kommunes (count number of dog data per kommune)
scandinavia$killedsheep <- lengths(st_intersects(scandinavia, killedsheepSpatial))

# join variable to clean data
#clean data does not have clean kommune names also seems to have NA data

cleandata <- cleandata %>% drop_na("Y_coordinate_fatefile","X_coordinate_fatefile")
subsetlocationsSpatial <- st_as_sf(x = cleandata,
                                   coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                                   crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

subsetKilledsheep <- st_intersection(subsetlocationsSpatial, scandinavia)

# add dogattack column to explanatoryvars in readData.R

# plot data
p1 <- ggplot() +
  geom_sf(data = scandinavia) +
  #geom_sf(data = dogdataSpatial,
  #        aes(fill="Fynddatum", alpha = 0.05)) +
  #geom_sf(data = dogdataSpatial, aes(color = Fynddatum),size = 0.7) +
  geom_sf(data = killedsheepSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Sheep attacks by wolves (1998-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p1


#confirm if the rovbasesample data is complete
# i have only taken into account the neumber of events not how
# many sheep are killed in each event - discuss with Camilla.
# to do it by year I first need to separate the datsets by year, then run spatail intersection by year,
# and then join the data from each year or otherwise fixed the kommune name in the original file
# and do it in the by dataframe manipulation  
