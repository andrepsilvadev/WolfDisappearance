libraries("ggplot2","tidyverse","sf")

data <- readr::read_csv("data/rawData/rawData.csv")
head(data)

dataClean <- dplyr::select(data, 1:13, 23) %>%
  mutate(mod_id = 1:nrow(data)) %>% #
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  # keep only the latest record for each individual
  group_by(id) %>%
  arrange(desc(Spring)) %>% # distinct keeps the first row
  distinct(id, .keep_all=TRUE)

# check only one row per individual
unique(dataClean$id)
# check missing values
sapply(dataClean, function(y) sum(length(which(is.na(y)))))

subset <- dataClean %>% dplyr::filter(Fate %in% NA)

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

library(credentials)
credentials::set_github_pat("TOKEN")




  

# I used the upload button to add files in local machine
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>%
  select(NAME_2,geometry)
plot(scandinavia)

head(subset)

subsetSpatial <- st_as_sf(x = subset,
                          coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                          crs = "EPSG:32632") %>% 
  st_transform(crs = st_crs(scandinavia)) %>%
  select(mod_id)

plot(scandinavia, col=NA, axes=TRUE)
plot(subsetSpatial, add = TRUE)
plot(subsetSpatial, pch = 20, cex = 2, col="red", add = TRUE)
         
st_intersection(scandinavia, subsetSpatial)     
