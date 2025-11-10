##########################
## Summary Statistics ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
##########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2",'survival', 'corrplot')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


setwd("")

# loading data:
load('data/strategies_survival_data_2000_2020.RData')

# checking correlation here:
cor_M <- policies_tv %>%
  # selecting only the main variables: 
  dplyr::select(
    # vis: 
   total_cybattacks_1ylag, regional_threatz_1ylag, w_rival_strategy_lag,
   orgs_wcyber_cumulative, w_defense_strategy_lag, 
   # controls:
   DEMOCRACY, LOG_INT_USERS, 
    LOG_GDP_PERCAP
     )
names(cor_M) <- c(
  'Country-specific cyber-attacks (lag)',
  'Regional cyber-attacks (lag)',
  'Strategies adopted by adversaries (lag)',
  'Cyber-relevant IGO Memberships',
  'Strategies adopted by allies (lag)',
  'Democracy',
  "Internet Users per capita (log)",
  'GDP per capita (log)'
  )
corrplot(cor(cor_M, use = "complete.obs"),
         method="number", # 'shade'
         number.cex=0.8,
         tl.cex=0.8)

# summary stats:
main_varz = c(
  'ADOPTION',
  'total_cybattacks_1ylag', 
  'regional_threatz_1ylag',
  'w_rival_strategy_lag', 
  'orgs_wcyber_cumulative',
  'w_defense_strategy_lag',
  'DEMOCRACY', 
  'LOG_INT_USERS',
  'LOG_GDP_PERCAP'
)
tab = summary(policies_tv[, main_varz])
dim(tab)
# creating latex table: 
tab = tab[c(1, 3, 4, 6),]
tab
#colnames(tab) = names(main_varz)
tab
class(tab)
dim(tab)
tab = as.data.frame(tab)
tab = tab %>%
  mutate(Freq=as.numeric(sapply(strsplit(Freq, ":"),"[[",2)))%>%
  mutate(Freq=round(Freq, digits = 2))
tab

# from long to wide:
tab_wide = tab %>%
  mutate(ID=seq(1, 36, by=1))%>%
  tidyr::spread(Var2, Freq)%>%
  dplyr::select(-c(--ID, Var1))
 # vertically
#tab_wide = sapply(tab_wide, function(x) {x[!is.na(x)]})
#rownames(tab_wide) = c('Minimum', 'Median', 'Mean', 'Maximum')
# horizontally:
tab_wide = lapply(tab_wide, function(x) {x[!is.na(x)]}) 
tab_wide = do.call(rbind, tab_wide)
colnames(tab_wide) = c('Minimum', 'Median', 'Mean', 'Maximum')
rownames(tab_wide) = c(
  'Adoption',
  'Country-specific cyber-attacks (lag)',
  'Regional cyber-attacks (lag)',
  'Strategies adopted by adversaries (lag)',
  'Cyber-relevant IGO Memberships',
  'Strategies adopted by allies (lag)',
  'Democracy',
  "Internet Users per Capita (log)",
  'GDP per capita (log)'
    )
tab_wide

library(xtable)
xtable(tab_wide)
#writeLines(capture.output(xtable(tab_wide)), paste0('output/summary_stats_v3.tex'))

