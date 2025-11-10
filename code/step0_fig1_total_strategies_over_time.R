##########################
### Plots             ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
##########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2","stringr","coda","ndtv","chron","countrycode",
                      "tidyr","scales",
                      "xtable","Hmisc","WDI","plm","reporttools",
                      "stargazer","psData","reshape2","maps","mapdata",
                      "countrycode","corrplot","sjPlot","rworldmap",
                      "readstata13",'base', 'qdapTools')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


# set working directory:
setwd("")
load('data/inaugural_cybstrategies_2000_2024.RData')


##########################
# Figure 1
##########################

# creating a ggplot to display the total number of the first national cybersecurity strategies adopted over time.

# Clean and process
df_clean <- data %>%
  filter(!is.na(Year)) %>%        # Remove rows with missing years
  group_by(Year) %>%
  # subssetting to 2024 only
  filter(Year < 2025) %>%
  summarise(yearly_adoptions = sum(cyb_strategy)) %>%
  arrange(Year) %>%
  mutate(cumulative = cumsum(yearly_adoptions)) %>%
  ungroup()
head(df_clean)

# Plot
ggplot(df_clean, aes(x = Year, y = cumulative)) +
  geom_line(aes(group = 1), color = "grey50", size = 1.2) +     # Grey line
  geom_point(size = 2, color = "grey50") +
  geom_label(aes(label = cumulative), fill = "white", color = "black", size = 4) +
  scale_y_continuous(breaks = seq(0, max(df_clean$cumulative), by = 25)) +  # Y-axis in 25s
  labs(
    x = "Year",
    y = "Cumulative Number of Countries Adopting \n First National Cybersecurity Strategies"
  ) +
  theme_minimal()
#ggsave("output/fig1_cyb_strategies_over_time.pdf", width = 8, height = 6)

