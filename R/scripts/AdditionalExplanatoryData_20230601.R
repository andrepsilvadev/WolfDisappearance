## Additional explanatory data ##
## Andre P. Silva ##

# notes:
# this script was used for the analyses presented at the wolf across boarders 
# conference
# clean data does not have clean kommune names also seems to have NA data

# bear density
 # am I using the corrected bear denity by cecilia? shall I include bear density due to
 # the correlation with human density

# average income per municipality 
 # prevasive number of NAs
# so far it seems I was using the individual income availble for Sweden but missing for nowray 
# for norway and sweden household income seem to be available but only since 2011
# give it another ago

# attacks on sheep
  # confirm if the rovbasesample data is complete
  # i have only taken into account the number of events not how
  # many sheep are killed in each event - discuss with Camilla.
  # to do it by year I first need to separate the datasets by year, then run spatail intersection by year,
  # and then join the data from each year or otherwise fixed the kommune name in the original file
  # and do it in the by dataframe manipulation
  # a lof of NAs
  # warning in killedsheepclean

# attacks on domestic dogs
 # double check the intersection method  

# spatial data -----------------------------------------------------------------
norway <- sf::st_read("data/spatialData/gadm36_NOR_shp/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_shp/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>% 
  select(NAME_2,geometry)

# income data per municipality -------------------------------------------------
incomeData <- readr::read_csv("data/rawData/avg_income_municipalities_1991_2021.csv", locale = locale(encoding = "ISO-8859-1")) %>%
  rowwise() %>%
  mutate(IncAvg1998_2021 = mean(c_across(c('year_1998': 'year_2021')), na.rm=TRUE)) %>%
  mutate(Kommun = tolower(Kommun)) %>% #to minimize mismatches due to case sensitivity
  select("Kommun","IncAvg1998_2021") %>%
  rename(KommunerNamn = Kommun)

# number of legally killed wolves ----------------------------------------------
KilledWolvesData <- readr::read_csv("data/rawData/Rovbase230311_CW_killed wolves_final.csv")

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
subsetlocationsSpatial <- st_as_sf(x = dataToModel,
                                   coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                                   crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

# (DOUBLE_CHECK THIS METHOD LATER!) matches mod_id and number of killed wolves by geometry
subsetKilledWolves <- st_intersection(subsetlocationsSpatial, scandinavia)

# wolf attacks on sheep --------------------------------------------------------
rovbasedata <- readr::read_csv("data/rawData/rovbaseSample.csv")
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

dataToModel <- dataToModel %>% drop_na("Y_coordinate_fatefile","X_coordinate_fatefile")
subsetlocationsSpatial <- st_as_sf(x = dataToModel,
                                   coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                                   crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

subsetKilledsheep <- st_intersection(subsetlocationsSpatial, scandinavia)

# wolf attacks on domestic dogs ------------------------------------------------
dogdata <- readr::read_csv("data/rawData/Wolf attacks on dogs Sweden and Norway_Rovbase 230131_230210_CW.csv")

# select attack id, date and coordinates (check if coordinate system is the same as in scandinavia)
dogdataclean <- dogdata %>% select("HändelseID", "Country", "Fynddatum", "Kommune",
                                   "Nord (RT90)", "Öst (RT90)") %>%
  na.omit()

# double check temporal span of the records
as.vector(dogdataclean %>% select("Fynddatum")) #1997-2022

# read as spatial points and change coordinate to RT90
dogdataSpatial <- st_as_sf(x = dogdataclean,
                           coords = c("Öst (RT90)", "Nord (RT90)"),
                           crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>% 
  select(HändelseID,geometry)

# intersect with scandinavia kommunes (count number of dog data per kommune)
scandinavia$dogattack <- lengths(st_intersects(scandinavia, dogdataSpatial))

# join variable to clean data
#clean data does not have clean kommune names also seems to have NA data

dataToModel <- dataToModel %>% drop_na("Y_coordinate_fatefile","X_coordinate_fatefile")
subsetlocationsSpatial <- st_as_sf(x = dataToModel,
                                   coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                                   crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

subsetDogAttacks <- st_intersection(subsetlocationsSpatial, scandinavia)

# join explanatory data to main matrix -----------------------------------------
# data on remaining variables calculated above is transported in the scandinavia object above
dataToModel.join <- right_join(dataToModel, subsetDogAttacks, by="mod_id") 
dataToModel.join1 <- left_join(dataToModel.join, incomeData, by="KommunerNamn") # alot of NAS because matching is done by kommunernamn and not spatial 
colSums(is.na(dataToModel.join1))

# run histogram p6 and correlation with other variables (descriptive stats.R)
explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county2",
                     "bear_density_BZtiff", "artificial_area_total",
                     "Fi", "number_neighbour_terr","dogattack", "killedWolves",
                     "IncAvg1998_2021", "killedsheep")
cordata <- dataToModel.join1 %>% select(all_of(explanatoryvars))

cordatalong <- cordata %>% 
  pivot_longer(
    cols = !territory,
    names_to = "variable", 
    values_to = "value"
  )

# variable distribution
p1 <- ggplot(data = cordatalong,
             aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  ylab("Count") +
  ggtitle(label = "Territory features") + 
  theme_minimal() +
  theme(text = element_text(size = 20))
p1

# variable correlation
cormat <- round(cor(cordata[2:16], use = "complete.obs", method = "spearman"),2)
cormat

# Get upper triangle of the correlation matrix
# from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p2 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)
p2  






# plot explanatory data

# number of legally killed wolves ----------------------------------------------
p3 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = killedWolvesSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Legally killed wolves (2000-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p3

# number of wolf attacks on sheep ----------------------------------------------
p4 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = killedsheepSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Sheep attacks by wolves (1998-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p4

# number of wolf attacks on domestic dogs --------------------------------------
p5 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = dogdataSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Wolf attacks on domestic dogs (1997-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p5



