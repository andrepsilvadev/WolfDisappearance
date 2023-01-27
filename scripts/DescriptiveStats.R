# Descriptive stats ##
## Andre P. Silva ##

source("scripts/libraries.R")
source("scripts/readData.R")

p <- ggplot(data = dataClean,
            aes(x = Fate, fill = sex)) + 
  #viridis::scale_fill_viridis(discrete=TRUE) +
  geom_histogram(stat = "count") +
  geom_text(stat= "count",
            aes(label=..count..),
            #check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            vjust=0,
            size=3) +
  ylab("Total Number of Individuals") +
  ggtitle(label = "Wolf fate (1998-2020)") + 
  theme_minimal()
p


p1 <- ggplot(data = subset,
             aes(x = Country_fatefile)) + 
  #viridis::scale_fill_viridis(discrete=TRUE) +
  geom_histogram(stat = "count") +
  ylab("Total number of records") +
  ggtitle(label = "Unknown fate (1998-2020)") + 
  theme_minimal()
p1

p2 <- ggplot(data = subset,
             aes(x = KommunerNamn)) + 
  #viridis::scale_fill_viridis(discrete=TRUE) +
  geom_histogram(stat = "count") +
  ylab("Total number of records") +
  ggtitle(label = "Unknown fate (1998-2020)") + 
  theme_minimal()
p2


next
repeated id - wolves recaptured? - used only fate from the last year captured
mod_id, SLU_id
ids that have two numbers
673 individuals!?
  cannot be total number of individuals ? is it?
  location of each class in space
what does fate = "other" mean? (other old age)
confirmed illegal = 1 ?
  what is coordinate system?
  rewrite column names

prepare files to discuss tmrw








# I used the upload button to add files in local machine
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>%
  select(NAME_2,geometry)

subsetSpatial <- st_as_sf(x = dataClean,
                          coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                          crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%
  filter(Fate %in% c("illegal","legal")) # having problems plotting all categories R cloud

ggplot() +
  geom_sf(data = scandinavia, aes(fill = NULL)) +
  geom_sf(data = subsetSpatial, aes(color = Fate),size = 0.8) +
  #facet_wrap(~Fate) +
  #viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Wolf fate (1998-2020)") + 
  theme_minimal()


st_intersection(scandinavia, subsetSpatial)  