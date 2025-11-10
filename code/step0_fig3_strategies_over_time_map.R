##########################
### Plots             ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
##########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2", 'rnaturalearth', 'rnaturalearthdata')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


setwd("")
data= read.csv('data/inaugural_cybstrategies_2000_2024.csv', header = TRUE)
data = data %>% 
  filter(Year <= 2020) %>%
  dplyr::select(Countries, Year, cyb_strategy)
head(data)
tail(data)

##########################
# Figure 3
##########################

data = data %>%
  mutate(interval = cut(Year,
                        breaks = c(2000, 2005, 2010, 2015, 2020),
                        labels = c("2001–2005", "2006–2010", "2011–2015", "2016–2020"),
                        include.lowest = TRUE))


# Load world map with ISO3 codes
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# checking if any ISO3 are missing from this list: 
iso_list = c(sort(unique(data$Countries)))
missing_iso <- iso_list[!iso_list %in% world$iso_a3]
missing_iso
# Fix missing ISO3 codes for France and Norway
world$iso_a3[world$name == "France"]  <- "FRA"
world$iso_a3[world$name == "Norway"]  <- "NOR"


# Merge strategy data with world map data
world_map <- world %>%
  left_join(data, by = c("iso_a3" = "Countries")) 

world_map$interval <- ifelse(is.na(world_map$interval), "Not Adopted", as.character(world_map$interval))

## Reorder factor levels so "Not Adopted" is first
world_map$interval <- factor(world_map$interval, levels = c( "2001–2005", "2006–2010", "2011–2015", "2016–2020", "Not Adopted"))

# Plot the map
ggplot(data = world_map) +
  geom_sf(aes(fill = interval), color = "black", size = 0.2, linetype = "dotted") +
  scale_fill_manual(
    values = c("Not Adopted" = "white",
               "2001–2005" = "gray80",
               "2006–2010" = "gray55",
               "2011–2015" = "gray30",
               "2016–2020" = "black"),
    name = "Adoption Interval"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Blue ocean
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) 

#ggsave("output/fig3_strategies_over_time_map.pdf", width = 8, height = 6)

