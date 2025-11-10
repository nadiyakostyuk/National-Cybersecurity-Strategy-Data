##########################
## Create Tables      ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
#########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2",'survival', 'reshape')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


setwd("")


#########
# Step 1: saving all output as a stargazer tables


# reading results
#rez <- readRDS('output/tab1_threatz.RDS')
#rez <- readRDS('output/tab2_all.RDS')
#rez <- readRDS('output/tab3_rob_checks_mod1-2.RDS')
rez <- readRDS('output/tab4_rob_checks_oecd.RDS')
length(rez) # 4
rez

# creating model list: 
# NOTE: this line should be adjusted depending on the length of rez (some rez have only 3 or 2 models)
mod =list( rez[[1]], rez[[2]], rez[[3]], rez[[4]])
#mod =list( rez[[1]], rez[[2]], rez[[3]])
#mod =list( rez[[1]], rez[[2]])
i=1; mod[[i]]
mod; class(mod)
resultz <- list()
library(broom)
for(i in 1:length(mod)){
  
  mod1 <- mod[[i]]%>%
    tidy%>%
    mutate(
      term=rownames(coef(summary(mod[[i]]))),
      estimate_exp=round(exp(estimate), digits = 2),
      conf.int.low_exp=round(exp(confint(mod[[i]])[,1]), digits = 2),
      conf.int.up_exp=round(exp(confint(mod[[i]])[,2]), digits = 2),
      model=i,
      concordance=round(summary(mod[[i]])$concordance[1], digits = 2)
    )%>%
    select(term, estimate_exp, conf.int.low_exp, conf.int.up_exp, p.value, concordance, model)
  mod1
  
  resultz[[i]] <- mod1
  print(i)
  
}
mod_resultz <- do.call(rbind, lapply(resultz, as.data.frame))
mod_resultz
dim(mod_resultz)

mod_resultz = mod_resultz%>%
  mutate(starz=case_when(
    p.value>=.1~'',
    p.value>=0.05&p.value<0.1~'^',
    p.value>=0.01&p.value<0.05~'*',
    p.value>=0.001&p.value<0.01~'**',
    p.value<0.001~'***'
  )
  )%>%
  mutate(ESTIMATE=paste0(estimate_exp, starz))%>%
  mutate(est_conf_int=paste0(ESTIMATE, '(', conf.int.low_exp, '; ', conf.int.up_exp, ')'))%>%
  dplyr::select(term, est_conf_int, concordance, model)
mod_resultz 

con_tab = mod_resultz %>%
  dplyr::select(concordance, model)%>%
  filter(!duplicated(model))%>%
  mutate(ID = seq(1, length(model), by = 1))
con_tab
con_tab = tidyr::spread(con_tab, 'model', 'concordance')
con_tab = sapply(con_tab, function(x) x[!is.na(x)])
con_tab = do.call(cbind, con_tab)
con_tab = as.data.frame(con_tab)
con_tab = con_tab[1,]
con_tab; class(con_tab)
con_tab = con_tab %>%  
  dplyr::rename(term=ID)%>%
  mutate(term='Concordance')
con_tab

##########################
mod_resultz2 = mod_resultz %>%
  dplyr::select(-concordance)
mod_resultz2

# from long to wide:
library(reshape)
melt(mod_resultz2, id.vars=c('est_conf_int'))
mod_res_wide = tidyr::spread(mod_resultz2, 'model', 'est_conf_int') # est_conf_int
mod_res_wide
mod_res_wide = mod_res_wide %>%
  mutate(term = case_when(
    # threats: 
    term == 'total_cybattacks_1ylag_sc' ~ 'Country-specific cyber-attacks (lag,sc) (DCID)',
    term == 'ridge(total_cybattacks_1y' ~ 'Country-specific cyber-attacks (lag,sc) (DCID)',
    term == 'total_cybattacks_erc_1ylag_sc' ~ 'Country-specific cyber-attacks (lag,sc) (ERC)',
    term == 'regional_threatz_1ylag_sc' ~ 'Regional Cyber-attacks (lag,sc) (DCID)', 
    term == 'regional_threatz_erc_1ylag_sc' ~ 'Regional Cyber-attacks (lag,sc) (ERC)', 
    term == 'w_rival_strategy_lag_sc' ~ 'Strategies adopted by adversaries (lag,sc)', 
    # other predictors:
    term == 'ridge(orgs_wcyber_cumulat'~'Cyber-Relevant IGO Memberships (lag)',
    term == 'w_defense_strategy_lag_sc' ~ 'Strategies adopted by allies_sc (lag)',
    term == 'ridge(w_defense_strategy_' ~ 'Strategies adopted by allies_sc (lag)',
    # controls:
    term == 'DEMOCRACY' ~ 'Democracy', 
    term == 'LOG_INT_USERS_sc' ~ 'Internet Users (log,sc)',
    term == 'NATO' ~ 'NATO',
    # regions: 
    term == 'as.factor(region)Asia' ~ 'Asia',
    term == 'as.factor(region)Europe' ~ 'Europe',
    term == 'as.factor(region)North America' ~ 'North America',
    term == 'as.factor(region)North Am' ~ 'North America',
    term == 'as.factor(region)Oceania' ~ 'Oceania',
    term == 'as.factor(region)South America' ~ 'South America',
    term == 'as.factor(region)South Am' ~ 'South America'
  )
  )%>%
  dplyr::arrange(match(term, c(
    # threats:
    'Country-specific cyber-attacks (lag) (DCID)',
    'Country-specific cyber-attacks (lag) (ERC)',
    'Regional Cyber-attacks (lag) (DCID)',
    'Regional Cyber-attacks (lag) (ERC)',
    'Strategies adopted by adversaries (lag)', 
    # other predictors:
    'Cyber-Relevant IGO Memberships', 
    'Strategies adopted by allies (lag)',
    # controls:
    'NATO',
    'Democracy', 
    'Internet Users (log)',
    # regions:
    'Asia', 'Europe', 'North America', 'Oceania', 'South America'
  ))
  )%>%
  replace(is.na(.), '------')
mod_res_wide

# merging concordate with the rest of the results
tab1 = mod_res_wide %>%
  rbind(con_tab)
library(xtable)
xtable(tab1)

# saving the model:
#writeLines(capture.output(xtable(tab1)), paste0('output/tab1_threatz.tex'))
#writeLines(capture.output(xtable(tab1)), paste0('output/tab2_all.tex'))
#writeLines(capture.output(xtable(tab1)), paste0('output/tab3_robchecks.tex'))
#writeLines(capture.output(xtable(tab1)), paste0('output/tab4_robchecks2.tex'))
