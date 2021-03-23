# global.R
# Benedito Chou
# Mar 20 2021


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

load("www/temp_shiny_data.RData")
load("www/temp_ana_data_v2.RData")
load("www/temp_extra_data.RData")
load("www/temp_ana_data_extra_measure.RData")

# For each additional Index, load the regression model table
load("www/m_step_df_home.RData")
load("www/m_step_df_fp_health.RData")

# --- Calculate Index Score ----------------------------------

# Pivot to long format for easy standardization
ana_data_1_wgeo_long <- ana_data_1_wgeo %>%
  pivot_longer(cols = percent_fair_or_poor_health:percent_rural, 
               names_to = c("var_name"))

ana_data_1_criterion <- ana_data_1_wgeo %>%
  dplyr::select(fips, state, county, 
         years_of_potential_life_lost_rate,
         # average_number_of_physically_unhealthy_days,
         average_number_of_mentally_unhealthy_days,
         preventable_hospitalization_rate,
         percent_adults_with_diabetes)

# Join with step-wise full table to get the weight
home_ana_data_1_wgeo_long <- ana_data_1_wgeo_long %>%
  left_join(m_step_df_home, by = "var_name")

# Join with step-wise full table to get the weight
ana_data_1_wgeo_long <- ana_data_1_wgeo_long %>%
  left_join(m_step_df, by = "var_name")


# Measure lst for Play Index
measure_lst <- filter(m_step_df, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_top5_lst <- measure_lst[c(1:5)]
measure_lst <- measure_lst[c(-1:-5)]

# Measure lst for Home Index
measure_lst_home <- filter(m_step_df_home, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_top5_lst_home <- measure_lst_home[c(1:2, 4, 7:8)]
measure_lst_home <- measure_lst_home[c(-1,-2, -4, -7, -8)]

# Measure lst for Fair and Poor Health as Outcome
measure_lst_fp_health <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_top5_lst_fp_heelth <- measure_lst_fp_health[c(1:5)]
measure_lst_fp_health <- measure_lst_fp_health[c(-1:-5)]

# Calculate Index with Fixed Slider values
fixed_z_data_1_wgeo_long <- ana_data_1_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = 100 - score,
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
fixed_z_data_1_wgeo <- fixed_z_data_1_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        ungroup() %>%
        mutate(
          score = play_w_percent_fair_or_poor_health,
          quintile = ntile(score, 5)) # Use fair and poor health as a proxy
      
names(fixed_z_data_1_wgeo) <- str_replace_all(names(fixed_z_data_1_wgeo), "^value_", "")

data_out <- dplyr::select(fixed_z_data_1_wgeo, fips,	state,	county, percent_fair_or_poor_health,	percent_smokers,	percent_adults_with_obesity,	percent_excessive_drinking,	percent_insufficient_sleep, score, quintile)

# Add physical_inactivity_back
phy_inactive_wgeo <- dplyr::select(ana_data_1_wgeo, fips, state, county, population, percent_physically_inactive)

data_out <- left_join(data_out, phy_inactive_wgeo, by = c("fips", "state", "county"))

# Save fixed play index score into csv
# write_csv(data_out, "../Beta/data/play_index_score_all_counties.csv", na = "")
