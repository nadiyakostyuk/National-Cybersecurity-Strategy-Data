##########################
## Analysis           ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
#########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2",'survival', 'countrycode', 'splines',
                      'sandwich', 'lmtest', 'glmnet')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


setwd("")


load('data/strategies_survival_data_2000_2020.RData')
#rescaling variable for the ease of the result interpretation:
policies_tv = policies_tv %>%
  mutate(
    # threats:
    total_cybattacks_1ylag_sc = scale(total_cybattacks_1ylag),
    total_cybattacks_erc_1ylag_sc = scale(total_cybattacks_erc_1ylag),
    regional_threatz_1ylag_sc = scale(regional_threatz_1ylag),
    regional_threatz_erc_1ylag_sc = scale(regional_threatz_erc_1ylag),
    w_rival_strategy_lag_sc = scale(w_rival_strategy_lag),
    # other predictors:
    orgs_wcyber_cumulative_1ylag_sc = scale(orgs_wcyber_cumulative_1ylag),
    w_defense_strategy_lag_sc = scale(w_defense_strategy_lag),
    LOG_INT_USERS_sc = scale(LOG_INT_USERS)
  )
policies_tv = policies_tv %>%
  mutate(YEAR_actual = tstart + 1999)

#######################
# TABLE1: threats
#######################


# models: 
mod_list_threatz = vector(mode='list')
mod_list_threatz

# DCID actual target
form_t1 <- paste0('Surv(tstart,tstop, ADOPTION)~total_cybattacks_1ylag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_t1
mod_t1 <- coxph(as.formula(form_t1), data=policies_tv, x=TRUE)
mod_t1
summary(mod_t1)
mod_list_threatz[[1]] = mod_t1


# ERC actual threats:
form_t2 <- paste0('Surv(tstart,tstop, ADOPTION)~total_cybattacks_erc_1ylag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_t2
mod_t2 <- coxph(as.formula(form_t2), data=policies_tv, x=TRUE)
mod_t2
summary(mod_t2)
mod_list_threatz[[2]] = mod_t2


# DCID perceived/regional threats:
form_t3 <- paste0('Surv(tstart,tstop, ADOPTION)~regional_threatz_1ylag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_t3
mod_t3 <- coxph(as.formula(form_t3), data=policies_tv, x=TRUE)
mod_t3
summary(mod_t3)
mod_list_threatz[[3]] = mod_t3


# ERC perceived/regional threats:
form_t4 <- paste0('Surv(tstart,tstop, ADOPTION)~regional_threatz_erc_1ylag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_t4
mod_t4 <- coxph(as.formula(form_t4), data=policies_tv, x=TRUE)
mod_t4
summary(mod_t4)
mod_list_threatz[[4]] = mod_t4


# saving models: 
#saveRDS(mod_list_threatz, file = 'output/tab1_threatz.RDS')


##############################
# TABLE 2: 
##############################

mod_list_all = vector(mode='list')
mod_list_all

# igo influence:
form_igo <- paste0('Surv(tstart,tstop, ADOPTION)~ridge(orgs_wcyber_cumulative_1ylag_sc) + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_igo
mod_igo <- coxph(as.formula(form_igo), data=policies_tv, x=TRUE)
mod_igo
summary(mod_igo)
mod_list_all[[1]] = mod_igo


# ally influence:
form_ally <- paste0('Surv(tstart,tstop, ADOPTION)~w_defense_strategy_lag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_ally
mod_ally <- coxph(as.formula(form_ally), data=policies_tv, x=TRUE)
mod_ally
summary(mod_ally)
mod_list_all[[2]] = mod_ally


# all predictors:
form_all <- paste0('Surv(tstart,tstop, ADOPTION)~ridge(total_cybattacks_1ylag_sc) + ridge(orgs_wcyber_cumulative_1ylag_sc) + ridge(w_defense_strategy_lag_sc) + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_all
mod_all <- coxph(as.formula(form_all), data=policies_tv, x=TRUE)
mod_all
summary(mod_all)
mod_list_all[[3]] = mod_all

#saveRDS(mod_list_all, file = 'output/tab2_all.RDS')



#######################################
# Robustness Checks
#######################################

##########################
# Table 3 Online Appendix
##########################


mod_list_robcheck = vector(mode='list')
mod_list_robcheck

# 1. influence of NATO
form_nato <- paste0('Surv(tstart,tstop, ADOPTION)~w_defense_strategy_lag_sc + NATO + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_nato
mod_nato <- coxph(as.formula(form_nato), data=policies_tv, x=TRUE)
mod_nato
summary(mod_nato)
mod_list_robcheck[[1]] = mod_nato


# 2. alt threat specification: 
form_t5 <- paste0('Surv(tstart,tstop, ADOPTION)~w_rival_strategy_lag_sc + DEMOCRACY + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_t5
mod_t5 <- coxph(as.formula(form_t5), data=policies_tv, x=TRUE)
mod_t5
summary(mod_t5)
mod_list_robcheck[[2]] = mod_t5
# saving the output now, as I am using code to create tables from the CPH models outputs:
#saveRDS(mod_list_robcheck, file = 'output/tab3_rob_checks_mod1-2.RDS')


# 3. glm + ridge: 
# Prepare data
# STEP 1: Drop rows with missing values in relevant variables
clean_data <- policies_tv %>%
  dplyr::select(ADOPTION, total_cybattacks_1ylag_sc, w_defense_strategy_lag_sc,
                DEMOCRACY, LOG_INT_USERS_sc, orgs_wcyber_cumulative_1ylag_sc,
                YEAR_glm, region) %>%
  na.omit()

# STEP 2: Build X and y from the same clean dataset
X <- model.matrix(ADOPTION ~ total_cybattacks_1ylag_sc +
                    orgs_wcyber_cumulative_1ylag_sc + w_defense_strategy_lag_sc +
                    DEMOCRACY + LOG_INT_USERS_sc + ns(YEAR_glm, df = 3) + factor(region), 
                  data = clean_data)[, -1]  # remove intercept

y <- clean_data$ADOPTION

# STEP 3: Fit ridge logistic regression (alpha = 0)
ridge_mod <- glmnet(X, y, family = "binomial", alpha = 0)

cv_ridge <- cv.glmnet(X, y, family = "binomial", alpha = 0)
# Extract coefficients that are used in the table
coef(cv_ridge, s = "lambda.min")


######################################
# OECD countries
# issue of under-reporting of cyber operations
# for other countries
#######################################
# TABLE 4 (Online Appendix)
######################################

# selecting only these countries and years in which they became oecd members: 
policies_tv = policies_tv %>%
  mutate(YEAR_actual = tstart + 2000)
sort(unique(policies_tv$YEAR_actual))
oecd_list = read.csv('data/oecd_countries.csv', header = TRUE)
# if NA, countries became OECD members before 2000 (when our analysis starts)
oecd_list = oecd_list %>%
  mutate(
    OECD_YEAR = ifelse(is.na(OECD_YEAR), 2000, OECD_YEAR), 
    oecd_member = 1
  ) %>%
  dplyr::rename(YEAR_actual = OECD_YEAR) %>%
  mutate(ISO_ADOPTING = countrycode(Country, 'country.name', 'iso3c')) %>%
  dplyr::select(-Country)

policies_tv = policies_tv %>%
  left_join(oecd_list, by = c('ISO_ADOPTING', 'YEAR_actual'))

policies_tv =  policies_tv %>%
  arrange(ISO_ADOPTING, YEAR_actual) %>%
  group_by(ISO_ADOPTING) %>%
  mutate(
    first_oecd_year = if (any(oecd_member == 1, na.rm = TRUE)) {
      min(YEAR_actual[oecd_member == 1], na.rm = TRUE)
    } else {
      Inf
    },
    oecd_member = ifelse(YEAR_actual >= first_oecd_year, 1, 0)
  ) %>%
  ungroup() %>%
  select(-first_oecd_year)  # optional: remove helper column

# selecting only oecd countries and ru-running the model: 
policies_tv_oecd = policies_tv %>%
  filter(oecd_member == 1) %>%
  mutate(LOG_INT_USERS_sc = scale(LOG_INT_USERS))

# Note: this model does not control for democracy because of perfect separation:
# all countries in this sample are democracies

mod_list_robcheck2 = vector(mode='list')
mod_list_robcheck2


# 1. dcid actual
form_oecd1 <- paste0('Surv(tstart,tstop, ADOPTION)~total_cybattacks_1ylag_sc + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_oecd1
mod_oecd1 <- coxph(as.formula(form_oecd1), data=policies_tv_oecd, x=TRUE)
mod_oecd1
summary(mod_oecd1)
mod_list_robcheck2[[1]] = mod_oecd1

# 2. erc actual
form_oecd2 <- paste0('Surv(tstart,tstop, ADOPTION)~total_cybattacks_erc_1ylag_sc + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_oecd2
mod_oecd2 <- coxph(as.formula(form_oecd2), data=policies_tv_oecd, x=TRUE)
mod_oecd2
summary(mod_oecd2)
mod_list_robcheck2[[2]] = mod_oecd2

# 3. dcid_regional
form_oecd3 <- paste0('Surv(tstart,tstop, ADOPTION)~regional_threatz_1ylag_sc + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_oecd3
mod_oecd3 <- coxph(as.formula(form_oecd3), data=policies_tv_oecd, x=TRUE)
mod_oecd3
summary(mod_oecd3)
mod_list_robcheck2[[3]] = mod_oecd3

# 4. erc_regional
form_oecd4 <- paste0('Surv(tstart,tstop, ADOPTION)~regional_threatz_erc_1ylag_sc + LOG_INT_USERS_sc + as.factor(region) + cluster(ISO_ADOPTING)')
form_oecd4
mod_oecd4 <- coxph(as.formula(form_oecd4), data=policies_tv_oecd, x=TRUE)
mod_oecd4
summary(mod_oecd4)
mod_list_robcheck2[[4]] = mod_oecd4


#saveRDS(mod_list_robcheck2, file = 'output/tab4_rob_checks_oecd.RDS')




