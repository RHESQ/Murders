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
library(statip)
library(ggthemes)

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

murders %>% 
  group_by(region) %>% 
  summarise(Average = mean(total),
            upper = max(total),
            lower = min(total),
            Median = median(total),
            Mode = mfv(total),
            iqr = IQR(total)) %>% 
  arrange(Average) %>% 
  view()

## 1. Data component
      
## 2. Geometric component
      #boxplot, scatter plot

## 3. aesthetics mapping
      # 

## 4. scale component
      #log scale

## 5. Labeling, Title, Legend




rate <- murders %>% 
  sumarise(rate = sum(total) / sum(poplation) * 10^6) %>% 
  .$rate


region_rate <- murders %>% 
  select(everything()) %>% 
  group_by(region) %>% 
  summarise(region_rate = sum(total) / sum(population) * 10^6) %>% 
  .$region_rate

region_rate

S_r <- murders %>% 
  select(everything()) %>% 
  filter(region %in% "South") %>% 
  summarise(S_r = sum(total) / sum(population) * 10^6) %>% 
  .$S_r
S_r

NC_r <- murders %>% 
  select(everything()) %>% 
  filter(region %in% "North Central") 
  summarise(NC_r = sum(total) / sum(population) * 10^6) %>% 
  .$Nc_r
NC_r

W_r <- murders %>% 
  select(everything()) %>% 
  filter(region %in% "West") %>% 
  summarise(W_r = sum(total) / sum(population) * 10^6) %>% 
  .$W_r
W_r

murders %>% 
  ggplot(aes(region, total, col = region)) +
  geom_boxplot() +
  
  geom_hline(yintercept = (region_rate), 
             linetype = "dashed", color = "darkgrey") +
  
  
  scale_y_log10() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
    ) +
  xlab("Total count of Murder") +
  ylab("Region of States") +
  ggtitle("Distribution of Murders in the U. S") +
  theme_economist()


