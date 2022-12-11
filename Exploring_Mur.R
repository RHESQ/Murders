###### Exporing the Murders Dataset ######
### Date : 12 / 12 / 2022
### Goals :
#          Finding out what we can get from the data
#          what kind of Data do we have
#          is the data clean
#          add, changing and removing features to get more from the data

getwd()
setwd("C:/Users/KELVIN/Desktop/Analytics/Murders")

library(dslabs)
library(tidyverse)

data("murders")

glimpse(murders)
view(murders)

murders %>% 
  select(everything()) %>% 
  arrange(-total) %>% 
  view()

murders[is.na(murders$total)]

murders %>% 
  select(everything()) %>% 
  filter(!complete.cases(.))

murders %>% 
  distinct()

murders %>% 
  select(everything()) %>% 
  mutate(status = if_else(total > 50,
                          "not safe", "safe"))

