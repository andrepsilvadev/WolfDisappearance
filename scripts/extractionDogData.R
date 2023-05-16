# read data
dogdata <- readr::read_csv("data/rawData/Wolf attacks on dogs Sweden and Norway_Rovbase 230131_230210_CW.csv")
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>% 
  select(NAME_2,geometry)

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

cleandata <- cleandata %>% drop_na("Y_coordinate_fatefile","X_coordinate_fatefile")
subsetlocationsSpatial <- st_as_sf(x = cleandata,
                          coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                          crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%  # having problems plotting all categories R cloud
  select(mod_id,geometry) 

subsetDogAttacks <- st_intersection(subsetlocationsSpatial, scandinavia)
#cleandata2 <- right_join(cleandata, subsetDogAttacks, by="mod_id") # see joinDataDiFFERENTSOURCES.R

# add dogattack column to explanatoryvars in readData.R

# plot data
p1 <- ggplot() +
  geom_sf(data = scandinavia) +
  #geom_sf(data = dogdataSpatial,
  #        aes(fill="Fynddatum", alpha = 0.05)) +
  #geom_sf(data = dogdataSpatial, aes(color = Fynddatum),size = 0.7) +
  geom_sf(data = dogdataSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Wolf attacks on domestic dogs (1997-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p1


# scaled data  
modeldatascaled <- modeldata %>% 
  mutate_at(c("terrRug", "humPop", "fi", "mooseHunt", "wolfNeighbour", "dogattack"),
            ~(scale(.) %>% as.vector)) %>%
  mutate(humPop2 = humPop^2)
head(modeldatascaled)

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


# add dog attack to model data (selection variables in model.R)
modeldata <- cleandata3 %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU-ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county", "number_neighbour_terr", "dogattack", "RHA") %>%
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


#correlation with other variables 
#only for continuous variables? what about categoricals? 
cormat <- round(cor(cordata[2:11], use = "complete.obs", method = "spearman"),2)
cormat

# Get upper triangle of the correlation matrix
# from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p7 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)
p7  


# run candidate models
# I have done it in the model script



# test with reindeer husbandry area - aborted variable would have an
# excess of zeros as few individuals or killed within the RHA

# i have added the reindeer husbandry area on top of the dog attack
reindeerhusbandry <- sf::st_read("data/spatialData/TamreinNoSv.shp") %>%
  st_transform(crs = st_crs(scandinavia))

subset2 <- st_intersection(subsetlocationsSpatial, reindeerhusbandry) %>%
  mutate(RHA = 1) %>%
  select("mod_id","RHA")


#%>%   select("NAME_2","dogattack")

#cleandata3 <- right_join(cleandata2, subset2, by="mod_id")
cleandata3 <- right_join(subset2,cleandata2, by="mod_id") %>%
  mutate(RHA = ifelse(is.na(RHA), 0, RHA))
