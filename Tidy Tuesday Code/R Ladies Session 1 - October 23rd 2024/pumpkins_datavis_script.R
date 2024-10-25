# R-Ladies viz -----
library(tidyverse)
library(geofacet)

#get data, read directly from github
pumpkins_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

#separate year and type because currently in one col
pumpkins <- pumpkins_raw %>% 
  separate(id, into =c("year", "type"))

#change all the initials for pumpkin type into their actual labels 
giant_pumpkins <-
  pumpkins %>% 
  mutate(across(c(year, weight_lbs, ott, place), parse_number),
         type_fct = factor(type, levels = c("F", "P", "S", "W", "L", "T"),
                           labels = c("Field Pumpkin",
                                      "Giant Pumpkin",
                                      "Giant Squash",
                                      "Giant Watermelon",
                                      "Long Gourd",
                                      "Tomato"))) %>% #mutate a selection of cols, characters to numeric
  select(-variety) # drop variety 

#get the pumpkin summary statistics
pumpkin_summaries <- giant_pumpkins %>% 
  group_by(type_fct, year) %>% 
  summarise(min_weight = min(weight_lbs),
            max_weight = max(weight_lbs),
            median_weight = median(weight_lbs),
            lq_weight = quantile(weight_lbs, .25),
            uq_weight = quantile(weight_lbs, .75)) %>% 
  ungroup()

#make your own cute colour palette to use. 
#note from Sophie: chocolate1 is not the one so use chocolate4
pumpkin_palette <- c("#F75F1C", "#000000", "#881EE4", "#85E21F", 
                     "#118A9A", "chocolate4")

#line graph max pumpkin weight over the years
ggplot(pumpkin_summaries) +
  geom_line(aes(x = year, y = max_weight, colour = type_fct)) + 
  scale_colour_manual(name = "Pumpkin Type", values = pumpkin_palette) +
  scale_x_continuous(name = "Year", breaks = seq(2013, 2021, by = 2)) +
  ylab("Maximum Weight")+
  theme_minimal()

#ribbon plot, median pumpkin weight over the years with shading for range
ggplot(pumpkin_summaries) +
  geom_line(aes(x = year, y = median_weight, colour = type_fct)) +
  geom_ribbon(aes(x=year, ymin=lq_weight, ymax = uq_weight, fill=type_fct), alpha=0.3) +
  scale_colour_manual(name = "Pumpkin Type", values = pumpkin_palette) +
  scale_fill_manual(name = "Pumpkin Type", values = pumpkin_palette) +
  scale_x_continuous(name = "Year", breaks = seq(2013, 2021, by = 2)) +
  ylab("Median Weight")+
  theme_minimal()



