# read data
KilledWolvesData <- readr::read_csv("data/rawData/Rovbase230311_CW_killed wolves_final.csv")
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>% 
  select(NAME_2,geometry)

#select columns of interest
killedWolvesClean <- KilledWolvesData %>% select("RovbaseID", "Dødsdato",
                                                 "Nord (RT90)", "Øst (RT90)")
  
#remove first two row data from 1997 and 1993 i think (double-check)
killedWolvesClean %>%
  filter(!(RovbaseID %in% c("M495546","M495563")))

# convert to spatial data
killedWolvesSpatial <- st_as_sf(x = killedWolvesClean,
                           coords = c("Øst (RT90)", "Nord (RT90)"),
                           crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>% 
  select(RovbaseID,geometry)

# intersect dog attacks with scandinavia kommunes (count number of dog data per kommune)
scandinavia$killedWolves <- lengths(st_intersects(scandinavia, killedWolvesSpatial))

#clean data does not have clean kommune names also seems to have NA data
subsetlocationsSpatial <- st_as_sf(x = cleandata,
                                   coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                                   crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

# (DOUBLE_CHECK THIS METHOD LATER!) matches mod_id and number of killed wolves by geometry
subsetKilledWolves <- st_intersection(subsetlocationsSpatial, scandinavia)
# DOUBLE CHECK IF IT SHOULD BE LEFT JOIN HERE
#cleandata2 <- right_join(cleandata, subsetKilledWolves, by="mod_id")


# plot data
p1 <- ggplot() +
  geom_sf(data = scandinavia) +
  #geom_sf(data = dogdataSpatial,
  #        aes(fill="Fynddatum", alpha = 0.05)) +
  #geom_sf(data = dogdataSpatial, aes(color = Fynddatum),size = 0.7) +
  geom_sf(data = killedWolvesSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Legally killed wolves (2000-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p1


i need to run the dog attack first to have all in the same file.
thne calculate correlations
scale
run the models
prepare meeting

modeldata <- cleandata %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU-ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county", "number_neighbour_terr", "dogattack", "killedWolves") %>%
  rename(fate = Fate,
         id = `SLU-ID`,
         Spring = Spring,
         terrRug = ruggedness_mean,
         humPop = pop_mean,
         fi = Fi,
         mooseHunt = hunt_county,
         wolfNeighbour =  number_neighbour_terr) %>%
  mutate(fate = as.factor(recode(fate, 'illegal'='1', 'legal'='0')))
head(modeldata)

# calculate correlations with remaining variables
# run histogram p6 and correlation with other variables (descriptive stats.R)
cordata <- cleandata2 %>% select(all_of(explanatoryvars))

cordatalong <- cordata %>% 
  pivot_longer(
    cols = !territory,
    names_to = "variable", 
    values_to = "value"
  )

p6 <- ggplot(data = cordatalong,
             aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  ylab("Count") +
  ggtitle(label = "Territory features") + 
  theme_minimal() +
  theme(text = element_text(size = 20))
p6




#calculate the total number of attacks for now
