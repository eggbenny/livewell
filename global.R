# global.R
# Benedito Chou
# May 12 2021


# --- Load packages ---------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(DT)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readxl)
library(corrplot)
library(knitr)
library(kableExtra)
library(arsenal)
library(GGally)
library(broom)
library(jtools) # plot and visualize regression
library(relaimpo)
library(MASS, exclude = "select")
library(sjPlot)
library(olsrr)
library(janitor)
library(QuantPsyc)
library(EnvStats) # For Outlier detection
library(bestglm)
library(urbnmapr)


# --- Import Processed data ----------------------------------

# OlD not use
# load("www/temp_shiny_data.RData")
# load("www/temp_ana_data_extra_measure.RData")
# load("www/temp_ana_data_v2.RData")
# load("www/temp_extra_data.RData")

# Load Main Data used in Regression Modeling
load("www/main_ana_data_df.RData")

# Load Extra Data
load("www/temp_extra_data_labour.RData")
load("www/temp_extra_data_medicare.RData")

# load Domain Map
load("www/domain_map.RData")

# load Region Map
load("www/region_lkup.RData")

# For each additional Index, load the regression model table
# Play Index & 2nd Layers
load("www/m_step_df_play.RData")
load("www/m_step_df_fp_health.RData")
load("www/m_step_df_grad.RData")
load("www/m_step_df_diabetes.RData")

# Rest Index & 2nd Layers
load("www/m_step_df_rest.RData")
load("www/m_step_df_doc_checkup.RData")
load("www/m_step_df_ypll.RData")
load("www/m_step_df_avg_mdays.RData")


# --- Calculate Index Score ----------------------------------

# Add region
ana_data_wgeo <- left_join(ana_data_full_wgeo, region_lkup, by = c("fips" = "FIPS")) %>%
  dplyr::mutate(
    RegionOrg = Region,
    Region = ifelse(is.na(Region), county, Region))

# Pivot to long format for easy standardization
ana_data_wgeo_long <- ana_data_wgeo %>%
  pivot_longer(cols = years_of_potential_life_lost_rate:public_health_fund, 
               names_to = c("var_name"))

# Join Domain map
ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(domain_map, by = "var_name")

ana_data_criterion <- ana_data_full_wgeo %>%
  dplyr::select(fips, state, county, 
         # years_of_potential_life_lost_rate,
         # average_number_of_physically_unhealthy_days,
         avg_no_of_physically_unhealthy_days,
         avg_no_of_mentally_unhealthy_days,
         # preventable_hospitalization_rate,
         # per_adults_with_diabetes,
         primary_care_physicians_ratio,
         per_unemployed,
         per_single_parent_households,
         age_adjusted_death_rate,
         social_association_rate,
         severe_housing_cost_burden,
         violent_crime_rate,
         x20th_perile_income,
         age_adjusted_death_rate,
         social_association_rate
  )

# Join with step-wise full table to get the weight
play_ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(m_step_df_play, by = "var_name")

# Join with step-wise full table to get the weight
rest_ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(m_step_df_rest, by = "var_name")


# Domain lst
domain_lst <- filter(domain_map, !is.na(Domain)) %>%
  distinct(Domain) %>% unlist() %>% as.character()

# Measure lst for Play Index
measure_lst_play <- filter(m_step_df_play, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_all_lst_play <- measure_lst_play
measure_top3_lst_play <- measure_lst_play[c(1:3)]
measure_lst_play <- measure_lst_play[c(-1:-3)]

# Measure lst for Rest Index
measure_lst_rest <- filter(m_step_df_rest, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_all_lst_rest <- measure_lst_rest
measure_top3_lst_rest <- measure_lst_rest[c(1:3)]
measure_lst_rest <- measure_lst_rest[c(-1:-3)]

# Measure lst for Fair and Poor Health as Outcome
measure_lst_fp_health <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_fp_health <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_fp_health <- measure_lst_fp_health 
measure_top3_lst_fp_health <- measure_lst_fp_health[c(1:3)]
measure_lst_fp_health <- measure_lst_fp_health[c(-1:-3)]

# Measure lst for Grad as Outcome
measure_lst_grad <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_grad <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_grad <- measure_lst_grad
measure_top3_lst_grad <- measure_lst_grad[c(1:3)]
measure_lst_grad <- measure_lst_grad[c(-1:-3)]

# Measure lst for Diabetes as Outcome
measure_lst_diabetes <- filter(m_step_df_diabetes, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_diabetes <- filter(m_step_df_diabetes, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_diabetes <- measure_lst_diabetes
measure_top3_lst_diabetes <- measure_lst_diabetes[c(1:3)]
measure_lst_diabetes <- measure_lst_diabetes[c(-1:-3)]

# Measure lst for Routine Doc Checkup as Outcome
measure_lst_doc_checkup <- filter(m_step_df_doc_checkup, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_doc_checkup <- filter(m_step_df_doc_checkup, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_doc_checkup <- measure_lst_doc_checkup
measure_top3_lst_doc_checkup <- measure_lst_doc_checkup[c(5,7,8)]
measure_lst_doc_checkup <- measure_lst_doc_checkup[c(-1:-8)]


# Measure lst for YPLL as Outcome
measure_lst_ypll <- filter(m_step_df_ypll, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_ypll <- filter(m_step_df_ypll, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_ypll <- measure_lst_ypll
measure_top3_lst_ypll <- measure_lst_ypll[c(4, 5, 7)]
measure_lst_ypll <- measure_lst_ypll[c(-1:-7)]

# Measure lst for Avg # of Mentally Unhealty Days as Outcome
measure_lst_avg_m_days <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_avg_m_days <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_avg_m_days <- measure_lst_avg_m_days
measure_top3_lst_avg_m_days <- measure_lst_avg_m_days[c(1:3)]
measure_lst_avg_m_days <- measure_lst_avg_m_days[c(-1:-3)]


# Calculate Index with Fixed Slider values
play_fixed_z_data_wgeo_long <- play_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          rank_value = rank(-score),
          per_rank_value = percent_rank(score) * 100,
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
# Convert back to wide format
play_fixed_z_data_wgeo <- play_fixed_z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        ungroup() %>%
        mutate(
          score = play_w_per_fair_or_poor_health,
          quintile = ntile(score, 5)) # Use fair and poor health as a proxy
      
names(play_fixed_z_data_wgeo) <- str_replace_all(names(play_fixed_z_data_wgeo), "^value_", "")

# Data All out
data_out <- dplyr::select(play_fixed_z_data_wgeo, fips,	state,	county, years_of_potential_life_lost_rate:copd_18plus, score, quintile)

# Add physical_inactivity_back
phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)

data_out <- left_join(data_out, phy_inactive_wgeo, by = c("fips", "state", "county"))

# Save fixed play index score into csv
# write_csv(data_out, "../Beta/data/play_index_score_all_counties_all_stepwise_measure.csv", na = "")

# Rest (aka Home Index dump data outs)
rest_fixed_z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          rank_value = rank(-score),
          per_rank_value = percent_rank(score) * 100,
          rest = ifelse(!is.na(b), 1, 0)) %>%
        filter(rest == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          rest_uw = mean(score, na.rm = T),
          rest_w = weighted.mean(score, pratt, na.rm = T)
        )
      
# Convert back to wide format
rest_fixed_z_data_wgeo <- rest_fixed_z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, rest_uw, rest_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, rest_uw, rest_w)) %>%
        ungroup() %>%
        mutate(
          score = rest_w_routine_doctor_checkup_past_years_18plus,
          quintile = ntile(score, 5)) # routine_doctor_checkup_past_years_18plus
      
names(rest_fixed_z_data_wgeo) <- str_replace_all(names(rest_fixed_z_data_wgeo), "^value_", "")

# Data All out
data_out <- dplyr::select(rest_fixed_z_data_wgeo, fips,	state,	county, years_of_potential_life_lost_rate:routine_doctor_checkup_past_years_18plus, score, quintile)

# Add percent insufficient sleep back
per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)

data_out <- left_join(data_out, per_insufficient_sleep_wgeo, by = c("fips", "state", "county"))

# # Save fixed play index score into csv
# write_csv(data_out, "../Beta/data/rest_index_score_all_counties_all_stepwise_measure.csv", na = "")

