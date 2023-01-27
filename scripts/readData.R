## Read Wolf Territory Dataset ##
## Andre P. Silva ##

data <- readr::read_csv("data/rawData/rawData.csv")
dataClean <- dplyr::select(data, 1:13, 23) %>%
  mutate(mod_id = 1:nrow(data)) %>% #
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  # keep only the latest record for each individual
  group_by(id) %>%
  arrange(desc(Spring)) %>% # distinct keeps the first row
  distinct(id, .keep_all=TRUE)

# check only one row per individual
#unique(dataClean$id)
# check missing values
#sapply(dataClean, function(y) sum(length(which(is.na(y)))))

#subset <- dataClean %>% dplyr::filter(Fate %in% NA)


   
