## Model input data ##
## Andre P. Silva ##

# Note: Göra 3 is not yet corrected in these files

# raw data ---------------------------------------------------------------------
data <- readr::read_csv2("C:\\Users\\Cecilia\\Downloads\\FateWolvesTerritory1998_2020_Scandinavia_Final_CDB.csv",
                         locale = locale(encoding = "ISO-8859-1"))
dim(data) # 1916   77

# data clean -------------------------------------------------------------------
idvars <- c(colnames(data)[1:13], "KommunerNamn", "SLU.ID")
explanatoryvars <- c("ruggedness_mean",
                     "average_gravel_km",
                     "average_paved_km",
                     "artificial_area_total",
                     "pop_mean",
                     "mean_snow",
                     "hunt_afo",
                     "hunt_county2",
                     "Fi",
                     "number_neighbour_terr",
                     "bear_density_BZtiff")

varstoselect <- c(idvars, explanatoryvars)

cleandata <- data %>%
  dplyr::select(all_of(varstoselect)) %>%
  mutate(mod_id = 1:nrow(data)) %>% 
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Legal", "legal")) %>%
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  mutate(Fate = str_replace(Fate, "censor", "censored")) %>%
  mutate(Fate = str_replace(Fate, "cernsored", "censored")) %>%
  mutate(Fate = str_replace(Fate, "censoreded", "censored")) %>%
  mutate(KommunerNamn = tolower(KommunerNamn)) %>%
  ungroup()

dim(cleandata) # 1916   27

# add new explanatory variables ------------------------------------------------
# Note: in the following script I have added additional explanatory variables 
# such as income. You will not be able to run it withour original variable raw data
# so I send the ouput of this section in case you want to compare with the previous
# step
#source("scripts/AdditionalExplanatoryData.R")

dataToModel7 <- read.csv(
  file = 'C:\\Users\\Cecilia\\Downloads\\dataToModel7.csv') # 1920 48 ###double-check why 1920****

dim(dataToModel7) # 1920   48
length(unique(dataToModel7$territory)) # 435 all territories  ####double-check Göra 3****other few errors (Municipalities) corrected Andre not in original dataset **Cecilia double chekc in original dataset
nrow(dataToModel7) # 1920 all observations
unique(dataToModel7$Fate) # NA, "illegal", "censored", "other", "traffic", "legal", "natural" 

# territory selection ----------------------------------------------------------
territoriesToModel <- dataToModel7 %>%
  group_by(territory) %>%
  # only keep territories that have "NA","illegal" or,"censored" in their fate
  # by excluding other fates
  filter(!any(Fate %in% c("other","legal","natural","traffic"))) %>% # i get 291
  # keep the row with the maximum inbreeding, if tied keep the first row
  group_by(territory, Autumn) %>%
  slice_max(mean_fi, with_ties=FALSE) %>% 
  # add column to double-check potential duplicate rows within territories 
  #mutate(Autumn_dup = duplicated(Autumn)) %>%
  ungroup() %>%
  mutate(Fate_binary = ifelse(is.na(Fate), 0,1),.after=Fate) %>%
  arrange(territory, Autumn)

#20230706 double-checking
test_sub <- dataToModel7 %>%
  dplyr::group_by(territory) %>%
  filter(all(c("illegal", "other") %in% Fate)) #6 territories

unique(test_sub$territory)
#"Tennådalen"  "Leksand 2"   "Glaskogen 4" "Vismen 1"    "Hedbyn 4"    "Kärrgången" 

test_sub1 <- dataToModel7 %>%
  dplyr::group_by(territory) %>%
  filter(all(c("illegal", "legal") %in% Fate)) #12 territories

unique(test_sub1$territory)
# [1] "Björnås 1"     "Tansen 1"      "Furudal"       "Krokvattnet"   "Tenskog 2"     "Sjösveden  4"  "Överhogdal"    "Sjunda 1"      "Liksjön"       "Skultuna"     
# [11] "Färna 3"       "Kroppefjäll 6"

test_sub2 <- dataToModel7 %>%
  dplyr::group_by(territory) %>%
  filter(all(c("illegal", "natural") %in% Fate)) #zero

test_sub3 <- dataToModel7 %>%
  dplyr::group_by(territory) %>%
  filter(all(c("illegal", "traffic") %in% Fate)) #zero




  # # only keep territories that have "NA","illegal" or,"censored" in their fate
  # # by excluding other fates
  # filter(!any(Fate %in% c("other","legal","natural","traffic"))) %>% # i get 291
  # # keep the row with the maximum inbreeding, if tied keep the first row
  # group_by(territory, Autumn) %>%
  # slice_max(mean_fi, with_ties=FALSE) %>% 
  # # add column to double-check potential duplicate rows within territories 
  # #mutate(Autumn_dup = duplicated(Autumn)) %>%
  # ungroup() %>%
  # mutate(Fate_binary = ifelse(is.na(Fate), 0,1),.after=Fate) %>%
  # arrange(territory, Autumn)



dim(territoriesToModel) # 642  49
length(unique(territoriesToModel$territory)) # 291
nrow(territoriesToModel) # 642 only one individual per territory
unique(territoriesToModel$Fate) #  NA         "illegal"  "censored"
