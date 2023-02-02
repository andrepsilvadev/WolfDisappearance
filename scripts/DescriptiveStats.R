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


p3 <- ggplot(data = dataClean,
            aes(x = KommunerNamn, fill = Fate)) + 
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
p3

subset <- dataClean %>%
  filter(Fate %in% c("illegal","legal"))

p3 <- ggplot(data = subset,
             aes(x = forcats::fct_infreq(KommunerNamn), fill = Fate)) + 
  geom_bar(stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  geom_text(stat= "count",
            aes(label=..count..),
            #check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            vjust=0,
            size=3) +
  ylab("Total Number of Individuals") +
  ggtitle(label = "Wolf fate (1998-2020)") + 
  theme_minimal()
p3


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

#####
## Spatial distribution of disappearance data
#####

# I used the upload button to add files in local machine
norway <- sf::st_read("data/spatialData/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>%
  select(NAME_2,geometry)
reindeerhusbandry <- sf::st_read("data/spatialData/TamreinNoSv.shp") #%>% #UTM33
  #st_transform(crs = st_crs(scandinavia))

subsetSpatial <- st_as_sf(x = dataClean,
                          coords = c("X_coordinate_fatefile","Y_coordinate_fatefile"),
                          crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%
  filter(Fate %in% c("illegal","legal")) # having problems plotting all categories R cloud

p4 <- ggplot() +
      geom_sf(data = scandinavia) +
      geom_sf(data = reindeerhusbandry,
          aes(fill="Reindeer husbandry area",alpha = 0.05)) +
      geom_sf(data = subsetSpatial, aes(color = Fate),size = 0.7) +
      coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
      #facet_wrap(~Fate) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      ggtitle(label = "Wolf fate (1998-2020)") + 
      geom_text(size = 30) +
      theme_minimal()
p4
ggsave("output/WolfFateSpatialDistribution.png",
       p4, width = 14, height = 10, dpi = 600, bg = "white")


#####
## Proportion of disappearance data per county
#####

points <- subsetSpatial %>%
  select("Fate","geometry") %>%
  slice_sample(10)

intersection <- st_intersection(x = scandinavia, y = points)

yellowlegal <- subsetSpatial %>%
  filter(Fate %in% "legal")
illegal <- subsetSpatial %>%
  filter(Fate %in% "illegal")

st_intersection(scandinavia, legal) 

scandinavia$PropIllegal <- subsetSpatial$illegal/(subsetSpatial$illegal + subsetSpatial$legal)

#####
## Explanatory variables - distribution
#####

cordata <- cleandata %>% select(all_of(explanatoryVars))

cordatalong <- cordata %>% 
  pivot_longer(
    cols = !territory,
    names_to = "variable", 
    values_to = "value"
  )

p4 <- ggplot(data = cordatalong,
            aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  ylab("Count") +
  ggtitle(label = "Territory features") + 
  theme_minimal() +
  theme(text = element_text(size = 20))
p4

ggsave("output/explanatoryVariableDistribution.png",
       p4, width = 12, height = 10, dpi = 600, bg = "white")

#####
## Explanatory variables - correlation
#####
# only for continuous variables? what about categoricals? 
cormat <- round(cor(cordata[2:9], use = "complete.obs", method = "spearman"),2)
cormat

# Get upper triangle of the correlation matrix
# from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p5 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)
p5  

ggsave("output/explanatoryVariableCorrelation.png",
       p5, width = 12, height = 5, dpi = 600, bg = "white")

#####
## Descriptive stats - comparison explanatory variables between
## groups
#####

boxplotdata <- cleandata %>%
  filter(Fate %in% c("legal", "illegal")) %>%
  select(c("territory",all_of(explanatoryvars),"Fate")) %>%
  pivot_longer(
    cols = explanatoryvars,
    names_to = "variable", 
    values_to = "value"
  )

bp <- ggplot(boxplotdata, aes(x=Fate, y=value, group=Fate)) + 
  geom_boxplot(aes(fill=Fate)) +
  # trying to hide outlier values
  #geom_boxplot(aes(fill=Fate), outlier.shape = NA) +
  #coord_cartesian(ylim = quantile(boxplotdata$value, c(0.1, 0.9), na.rm = TRUE)) +
  #scale_fill_viridis_d() +
  facet_wrap(~variable, scales = "free_y") +
  ggtitle(label = "Explanatory variables in illegal vs legal fate") + 
  theme_minimal()
bp

number of neigbour territories is categorical so boxplot cannot be used



# ggsave("fig/yearly_sex_counts.png", my_plot, width = 15, height = 10)
# # This also works for grid.arrange() plots
# combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

# 

### need to exclude correlated variables here 
## moose data seems a lot of zeros - informative?
## low heterogeneity in road variables


# if not used delete
# cormatlong <- cormat %>% 
#   pivot_longer(
#     cols = !territory,
#     names_to = "variable", 
#     values_to = "value"
#   )

