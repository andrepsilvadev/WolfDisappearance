# join external from differnet sources (dog attacks, killed wolves, incomeData)

cleandata2 <- left_join(cleandata, subsetKilledWolves, by="mod_id")
cleandata3 <- right_join(cleandata2, subsetDogAttacks, by="mod_id") 
cleandata4 <- left_join(cleandata3, incomeData, by="KommunerNamn") # alot of NAS because matching is done by kommunernamn and not spatial 
cleandata5 <- left_join(cleandata4, subsetKilledsheep, by="mod_id") # alot of NAS because matching is done by kommunernamn and not spatial 

colnames(cleandata5)
colSums(is.na(cleandata5))


# run histogram p6 and correlation with other variables (descriptive stats.R)
explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county2",
                     "bear_density_BZtiff", "artificial_area_total",
                     "Fi", "number_neighbour_terr","dogattack", "killedWolves",
                     "IncAvg1998_2021", "killedsheep")
cordata <- cleandata5 %>% select(all_of(explanatoryvars))

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

#correlation with other variables 
#only for continuous variables? what about categoricals? 
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


# add dog attack to model data (selection variables in model.R)
modeldata <- cleandata5 %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU.ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county2", "number_neighbour_terr", "dogattack", "artificial_area_total",
         "killedWolves", "killedsheep","IncAvg1998_2021","bear_density_BZtiff") %>%
  rename(fate = Fate,
         id = `SLU.ID`,
         Spring = Spring,
         terrRug = ruggedness_mean,
         humPop = pop_mean,
         fi = Fi,
         mooseHunt = hunt_county2,
         wolfNeighbour =  number_neighbour_terr,
         sheepAttacks = killedsheep,
         bearDensity = bear_density_BZtiff) %>%
  mutate(fate = as.factor(recode(fate, 'illegal'='1', 'legal'='0')))
head(modeldata)

# scaled data  
modeldatascaled <- modeldata %>% 
  mutate_at(c("terrRug", "humPop", "fi", "mooseHunt", "wolfNeighbour",
              "artificial_area_total", "dogattack", "killedWolves",
              "IncAvg1998_2021", "sheepAttacks", "bearDensity"),
            ~(scale(.) %>% as.vector)) %>%
  mutate(humPop2 = humPop^2)
head(modeldatascaled)

# run candidate models
# I have done it in the model script

just to visualize

explanatoryvars1 <- c("dogattack","killedWolves")
  
boxplotdata <- cleandata4 %>%
  filter(Fate %in% c("legal", "illegal")) %>%
  select(c("territory",all_of(explanatoryvars1),"Fate")) %>%
  pivot_longer(
    cols = explanatoryvars1,
    names_to = "variable", 
    values_to = "value"
  )

p8 <- ggplot(boxplotdata, aes(x=Fate, y=value, group=Fate)) + 
  geom_boxplot(aes(fill=Fate)) +
  # trying to hide outlier values
  #geom_boxplot(aes(fill=Fate), outlier.shape = NA) +
  #coord_cartesian(ylim = quantile(boxplotdata$value, c(0.1, 0.9), na.rm = TRUE)) +
  #scale_fill_viridis_d() +
  facet_wrap(~variable, scales = "free_y") +
  ggtitle(label = "Explanatory variables in illegal vs legal fate") + 
  theme_minimal() +
  theme(text = element_text(size = 20))
p8

