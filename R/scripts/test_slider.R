library(dplyr)
library(tidyr)
library(slider)

# dataframe without NAs

data <- data.frame(
    kommun = c(rep("A",3), rep("B",3), rep("C",3)),
    kill_n = c(0,1,2,2,3,5,7,8,0)
)

# solution 1 - with subset and for loop

join.data <- list()

for (i in data$kommun){
    group <- data %>% subset(kommun %in% i)
    new.data <- group %>%
    mutate(kill_last5y = slider::slide_dbl(kill_n, sum, .before = 2, .after = 0),
         .after = kill_n)
         join.data[[i]] <- new.data
}

join.data <- bind_rows(join.data)
(join.data)

# solution 2 - slider with group_by and mutate

newdata2 <- data %>%
 group_by(kommun) %>%
  mutate (kill_last5y = slider::slide_dbl(kill_n, sum, .before = 2, .after = 0),
         .after = kill_n)

(newdata2)

# dataframe with NAs -------------------------------------------------
dataNA <- data.frame(
    kommun = c(rep("A",4), rep("B",4), rep("C",4)),
    kill_n = c(0,1,2,NA,2,3,5,NA,7,8,0,NA)
)

# solution 3 - with subset and for loop

join.data2 <- list()

for (i in dataNA$kommun){
    group <- dataNA %>% subset(kommun %in% i)
    new.data <- group %>% 
        mutate_at(c('kill_n'), ~tidyr::replace_na(.,0)) %>%
        mutate(kill_last5y = slider::slide_dbl(kill_n, sum, .before = 3, .after = 0),
         .after = kill_n)
         join.data2[[i]] <- new.data
}

join.data2 <- bind_rows(join.data2)
(join.data2)

# solution 4 - slider with group_by and mutate
newdata3 <- dataNA %>%
 group_by(kommun) %>% 
 mutate_at(c('kill_n'), ~tidyr::replace_na(.,0)) %>%
 mutate (kill_last5y = slider::slide_dbl(kill_n, sum, .before = 3, .after = 0),
         .after = kill_n)

(newdata3)