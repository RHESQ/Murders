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
  select(everything) %>% 
  arrange()