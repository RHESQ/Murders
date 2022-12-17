###### Exploring the Murders Dataset ######
### Date : 12 / 12 / 2022
### Goals :
#          Finding out what we can get from the data
#          what kind of Data do we have
#          is the data clean
#          adding, changing and removing features to get more from the data

getwd()
setwd("C:/Users/KELVIN/Desktop/Analytics/Murders")

library(dslabs)
library(tidyverse)
library(statip)
library(ggthemes)
library(ggrepel)

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
                          "not safe", "safe"),
         rate = total / population * 10^6)

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

summary(murders)

murders %>% 
  select(region, total) %>% 
  summary(murders) %>% 
  view()

murders %>% 
  select(region, total) %>% 
  summary(murders) %>% 
  arrange()

## 1. Data component
      
## 2. Geometric component
      #boxplot, scatter plot, smooth density

## 3. aesthetics mapping
      # 

## 4. scale component
      #log scale

## 5. Labeling, Title, Legend




rate <- murders %>% 
  summarise(rate = sum(total) / sum(population) * 10^6) %>% 
  .$rate


region_rate <- murders %>% 
  select(everything()) %>% 
  group_by(region) %>% 
  summarise(region_rate = sum(total) / sum(population) * 10^6) %>% 
  .$region_rate


murders %>% 
  ggplot(aes(x = reorder(region, total, FUN = mean), total)) +
  geom_boxplot(aes(col = region)) +
  geom_jitter() +
  
  geom_hline(yintercept = (region_rate), 
             linetype = "dashed", color = "darkgrey") +
  
  
  scale_y_log10() +
  
  ylab("Total count of Murder") +
  xlab("Region of States") +
  ggtitle("Distribution of Murders in the U. S") +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )



murders %>% 
  ggplot(aes(population / 10^6, total, label = abb)) + 
  geom_point(aes(col = region)) +
  geom_text_repel() +
  
  scale_x_log10() +
  scale_y_log10() +
  
  xlab("Population of states(log10)") +
  ylab("Total count of Murders(log10)") +
  ggtitle("Distribution of Murders in the U. S") +
  
  theme_economist() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    
    panel.grid.major.y = element_line(color = "white", size = 1)
  )


#take a look at the distribution values by mean  
murders %>% 
  select(region, total) %>% 
  group_by(region) %>% 
  summarise(mean_murder = mean(total),
            median_murder = median(total)) %>% 
  arrange(median_murder) %>% view()

# Research question:    Is the overall total murder in
#                       these four regions of the states
#                       different

# Hypothesis testing:   H0: Mean total murder is the same
#                       H1: Mean total murder is not the same

# Observation:          Difference in mean is observed in the
#                       sample data, but is this statistically
#                       significant (alpha = 0.05)

# Anova model

murders %>% 
  select(region, total) %>% 
  aov(total ~ region, data = .) %>% 
  summary()

murders %>% 
  select(region, total) %>% 
  aov(total ~ region, data = .) %>% 
  TukeyHSD()
