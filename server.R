# server.R
# Benedito Chou
# May 12 2021

# --- Server ----------------------------------------------

# --- Server
shinyServer(function(input, output, session) {
  
    # --- measure_lst Switch ---
  
   observe({
     
     # if (input$iv_top5 == "percent_fair_or_poor_health") {
     #   updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
     # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
     #   updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
     # }
     
   })
    
    # --- Filter and Control ---
  
    # Block one or the other
   # Play Index
    observe({
      
      if (input$iv_top5 == "per_fair_or_poor_health") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_fp_health) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
        } else {
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
        }
        
        
      } else if (input$iv_top5 == "per_w_grad_or_prof_degree") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_grad) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
      } else {
       updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_grad))
        
      }
        
      } else if (input$iv_top5 == "per_adults_with_diabetes") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_diabetes) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
      } else {
       updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_diabetes))
      }
        
      } else if (input$iv_top5 != "Select a Measure") {
        shinyjs::disable("change")
        shinyjs::enable("change_top5")
        updateSliderInput(session, "change", value = 0)
        updateSelectizeInput(session, "iv", selected = "Select a Measure")
        
       # if (input$iv_top5 == "percent_fair_or_poor_health") {
       # updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
       # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
       # # updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
       # }
        
      }
    })
    
    observe({
      
       if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        shinyjs::disable("change_top5")
        shinyjs::enable("change")
        updateSliderInput(session, "change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")

       # if (input$iv_top5 == "percent_fair_or_poor_health") {
       # # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
       # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
       # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_play))
       # }

      } else if (!input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        shinyjs::disable("change_top5")
        shinyjs::enable("change")
        updateSliderInput(session, "change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      }
    })
    
    
    # Rest Index
    observe({
      
      if (input$rest_iv_top5 == "routine_doctor_checkup_past_years_18plus") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_doc_checkup) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_doc_checkup))
        }
        
        
      } else if (input$rest_iv_top5 == "years_of_potential_life_lost_rate") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_ypll) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_ypll))
          
        }
        
      } else if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_avg_m_days) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_avg_m_days))
        }
        
      } else if (input$rest_iv_top5 != "Select a Measure") {
        shinyjs::disable("rest_change")
        shinyjs::enable("rest_change_top5")
        updateSliderInput(session, "rest_change", value = 0)
        updateSelectizeInput(session, "rest_iv", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
        # }
      
       }
      
    })
  
    observe({
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        shinyjs::disable("rest_change_top5")
        shinyjs::enable("rest_change")
        updateSliderInput(session, "rest_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_play))
        # }
        
      } else if (!input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        shinyjs::disable("rest_change_top5")
        shinyjs::enable("rest_change")
        updateSliderInput(session, "rest_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      }
    })
  
    # Update county and region filter based on state
    observe({
        
      data <- filter(ana_data_wgeo, state == input$state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "county",
        choices = county_lst
      )
      
      Rdata <- filter(ana_data_wgeo, state == input$state)  
      region_lst <- sort(unique(Rdata$RegionOrg))
      
      updateSelectInput(session, "region", 
        choices = c("--", region_lst))
        
    })
    
    observe({
        
      data <- filter(ana_data_full_wgeo, state == input$rest_state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "rest_county",
        choices = county_lst
      )
      
      Rdata <- filter(ana_data_wgeo, state == input$rest_state)  
      region_lst <- sort(unique(Rdata$RegionOrg))
      
      updateSelectInput(session, "rest_region", 
                        choices = c("--", region_lst))
        
    })
    
    # --- Data Process ---
    
    # Play Index b change
    # Calculate the b to use for change from slider
    play_slider_change_b <- reactive({
      
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
          
          b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b1 * bm
          
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
          
        } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
          
          b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b2 * bm
          
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
          
        } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
          
          b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b3 * bm
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        
        } else {
          
          real_b <- slider_data$b
          print(paste0("Normal Route: ", input$iv_top5, "):", round(real_b, 3)))
          
        }
        
        print(paste0("slider change b: ", round(real_b), 3))
        
        return(real_b)
      
    })
    
    # Rest Index b change
    # Calculate the b to use for change from slider
    rest_slider_change_b <- reactive({
      
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        if (input$rest_iv_top5 == "routine_doctor_checkup_past_years_18plus" & input$rest_iv != "Select a Measure") {
          
          b1 <- filter(slider_data, var_name == "routine_doctor_checkup_past_years_18plus") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b1 * bm
          
          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
          
        } else if (input$rest_iv_top5 == "years_of_potential_life_lost_rate" & input$rest_iv != "Select a Measure") {
          
          b2 <- filter(slider_data, var_name == "years_of_potential_life_lost_rate") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b2 * bm
          
          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
          
        } else if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days" & input$rest_iv != "Select a Measure") {
          
          b3 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b3 * bm
          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        
        } else {
          
          real_b <- slider_data$b
          print(paste0("Normal Route: ", input$rest_iv_top5, "):", round(real_b, 3)))
          
        }
        
        print(paste0("slider change b: ", round(real_b), 3))
        
        return(real_b)
      
    })
  
    # Calculate Index County
    play_z_geo_df <- reactive({
      # Calculate play index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
        # Change value?
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          value_change <- input$change_top5
        } else {
          value_change <- input$change
        }
      
       z_data_wgeo_long <- play_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$state & county == input$county, 1, 0))
      
      if (input$region != "--") {
      z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, 0),
              focus = ifelse(is.na(focus), focusOrg, focus))
      }
       
       
      # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
       
      if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        
        print("Second Layer")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + (input$change * real_b), value))
        
      } else if (input$iv_top5 == "Select a Measure" & input$iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv, value + input$change_top5, value))
        
      } else {
        
        
        print("Main Layer - Top 3 Measures")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        # ungroup() %>%
        # group_by(var_name) %>%
        mutate(
        #   focus = ifelse(state == input$state & county == input$county, 1, 0),
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_per_fair_or_poor_health) # Use fair and poor health as a proxy
    
      # Add physical_inactivity_back
      phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate, 
      # # TODO: fix it for real so index changes
      # per_american_indian_alaska_native, per_asian, per_hispanic, avg_no_of_mentally_unhealthy_days, per_adults_with_diabetes, number_single_parent_households, per_severe_housing_problems, overcrowding, per_homeowners) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, phy_inactive_wgeo, by = c("fips", "state", "county")) %>% ungroup()
      # left_join(additional_wgeo, by = c("fips", "state", "county")) %>%
      #     ungroup()
     
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      return(z_data_wgeo)
      
    })
    
    rest_z_geo_df <- reactive({
      # Calculate play index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        value_change <- input$rest_change_top5
      } else {
        value_change <- input$rest_change
      }
      
      
      z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0))
      
      if (input$rest_region != "--") {
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, 0),
            focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      
      # Get real_b from slider
      rest_slider_change_b.check <<- rest_slider_change_b()
      real_b <- rest_slider_change_b()
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + (input$rest_change * real_b), value))
        
      } else if (input$rest_iv_top5 == "Select a Measure" & input$rest_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change_top5, value))
        
      } else {
        
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0),
          # value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_routine_doctor_checkup_past_years_18plus) # Use routine doc checkup as a proxy
      
      # Add per_insufficient_sleep back
      per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, primary_care_physicians_ratio) #violent_crime_rate, severe_housing_cost_burden x20th_percentile_income percent_uninsured percent_single_parent_households percent_unemployed social_association_rate
      
      # rest_z_data_1_wgeo <- left_join(rest_z_data_1_wgeo, additional_wgeo, by = c("fips", "state", "county")) %>%
      #   ungroup()
      
      z_data_wgeo <- left_join(z_data_wgeo, per_insufficient_sleep_wgeo, by = c("fips", "state", "county")) %>% ungroup()
      # left_join(additional_wgeo, by = c("fips", "state", "county")) %>%
      
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      return(z_data_wgeo)
      
    })
    
    # Calculate Index Region
    play_z_geo_region_df <- reactive({
      # Calculate play index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
        # Change value?
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          value_change <- input$change_top5
        } else {
          value_change <- input$change
        }
      
      z_data_wgeo_long <- play_ana_data_wgeo_long %>%
        mutate(focus = ifelse(state == input$state & Region == input$region, 1, 0))
      
      # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
      
      if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + (input$change * real_b), value))
        
      } else if (input$iv_top5 == "Select a Measure" & input$iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv, value + input$change_top5, value))
        
      } else {
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, Region, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_per_fair_or_poor_health) # Use fair and poor health as a proxy
      
      # Add physical_inactivity_back
      phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, phy_inactive_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      #left_join(additional_wgeo, by = c("fips", "state", "county")) 
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      
      return(z_data_wgeo)
      
    })
    
    rest_z_geo_region_df <- reactive({
      # Calculate play index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        value_change <- input$rest_change_top5
      } else {
        value_change <- input$rest_change
      }
      
      z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
        mutate(focus = ifelse(state == input$rest_state & Region == input$rest_region, 1, 0))
      
      # Get real_b from slider
      rest_slider_change_b.check <<- rest_slider_change_b()
      real_b <- rest_slider_change_b()
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + (input$rest_change * real_b), value))
        
      } else if (input$rest_iv_top5 == "Select a Measure" & input$rest_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change_top5, value))
        
      } else {
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, Region, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_routine_doctor_checkup_past_years_18plus) # Use routine doc checkup as a proxy
      
      # Add per_insufficient_sleep back
      per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, per_insufficient_sleep_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      #left_join(additional_wgeo, by = c("fips", "state", "county")) 
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      
      return(z_data_wgeo)
      
    })
    

  
    # Create data subset
    play_select_geo_df <- reactive({
      
        data <- filter(play_z_geo_df(), state == input$state, county == input$county)
        return(data)
        
    })
    
    play_select_geo_region_df <- reactive({
      
        data <- filter(play_z_geo_region_df(), state == input$state, Region == input$region)
        return(data)
        
    })
    
    rest_select_geo_df <- reactive({
        data <- filter(rest_z_geo_df(), state == input$rest_state, county == input$rest_county)
        
        return(data)
    })
    
    rest_select_geo_region_df <- reactive({
      
      data <- filter(rest_z_geo_region_df(), state == input$rest_state, Region == input$rest_region)
      return(data)
      
    })
    
    # Get weight for iv
    play_slider_data <- reactive({
      
      data <- filter(m_step_df_play, var_name == input$iv | var_name == input$iv_top5)
        # data <- filter(m_step_df_play, var_name == input$iv | var_name %in% c(
        #    "percent_fair_or_poor_health",
        #    "percent_adults_with_obesity",
        #    "percent_insufficient_sleep",
        #    "percent_smokers",
        #    "percent_excessive_drinking"
        # ))
    })
    
    play_slider_med_data <- reactive({
      
      # double b if fair and poor health as mediator
      if (input$iv_top5 == "per_fair_or_poor_health") {
        data <- filter(m_step_df_fp_health, var_name == input$iv)
      } else if (input$iv_top5 == "per_w_grad_or_prof_degree") {
        data <- filter(m_step_df_grad, var_name == input$iv)      
      } else if (input$iv_top5 == "per_adults_with_diabetes") {
        data <- filter(m_step_df_diabetes, var_name == input$iv)
      } else {
        data <- data.frame(NA)
      }
      
      return(data)
      
    })
    
    rest_slider_data <- reactive({
      
      data <- filter(m_step_df_rest, var_name == input$rest_iv | var_name == input$rest_iv_top5)

        # data <- filter(m_step_df_rest, var_name == input$iv | var_name %in% c(
        #    "percent_fair_or_poor_health",
        #    "percent_adults_with_obesity",
        #    "percent_insufficient_sleep",
        #    "percent_smokers",
        #    "percent_excessive_drinking"
        # ))
    })
    
    rest_slider_med_data <- reactive({
      
      # double b if fair and poor health as mediator
      if (input$rest_iv_top5 == "routine_doctor_checkup_past_years_18plus") {
        data <- filter(m_step_df_doc_checkup, var_name == input$rest_iv)
      } else if (input$rest_iv_top5 == "years_of_potential_life_lost_rate") {
        data <- filter(m_step_df_ypll, var_name == input$rest_iv)      
      } else if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        data <- filter(m_step_df_avg_m_days, var_name == input$rest_iv)
      } else {
        data <- data.frame(NA)
      }
      
      return(data)
      
    })
    
    
    # Get weight for criterion
    criterion_slider_data <- reactive({
         # years_of_potential_life_lost_rate,
         # avg_no_of_physically_unhealthy_days,
         # avg_no_of_mentally_unhealthy_days,
         # preventable_hospitalization_rate
    })
    
    # Chart data
    plot_data <- reactive({
        data <- filter(play_z_geo_df(), state == input$state)
        data <- mutate(data, 
          focus = ifelse(state == input$state & county == input$county, 1, 0))
        return(data)
    })
    
   plot_region_data <- reactive({
        data <- filter(z_geo_region_df(), state == input$state)
        data <- mutate(data, 
          focus = ifelse(state == input$state & Region == input$region, 1, 0))
        return(data)
    })
    
    rest_plot_data <- reactive({
        data <- filter(rest_z_geo_df(), state == input$rest_state)
        
        data <- mutate(data, 
          focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0))
        return(data)
    })
    
    # --- InfoBox ---
    output$population <- renderInfoBox({
        
      # Region Pop
        pop_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(population, sum, na.rm = T)) %>%
          dplyr::select(population) %>%
            unlist() %>%
            as.character()
      
      # County Pop
        pop_county <- play_select_geo_df() %>%
           dplyr::select(population) %>%
            unlist() %>%
            as.character()
        
        if (input$region != "--") {
          pop <- pop_region
          label <- "Region"
        } else {
          pop <- pop_county
          label <- 'County'
        }
        
        infoBox(
          paste(label, " Population"), pop,
          fill = TRUE,
          color = "teal"
        )
        
    })
    
    output$rest_population <- renderInfoBox({
        
      # Region Pop
      pop_region <- rest_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      # County Pop
      pop_county <- rest_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      if (input$rest_region != "--") {
        pop <- pop_region
        label <- "Region"
      } else {
        pop <- pop_county
        label <- 'County'
      }
      
      infoBox(
        paste(label, " Population"), pop,
        fill = TRUE,
        color = "teal"
      )
        
    })
    
    
    output$pop_impact <- renderInfoBox({
      
        # Region Pop
        pop_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(population, sum, na.rm = T)) %>%
          dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
        pop_county <- play_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
        if (input$region != "--") {
          value <- pop_region
        } else {
          value <- pop_county
        }
        
         # slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        slider_data.check <<-  slider_data
        slider_med_data.check <<- slider_med_data
        
        # b <- filter(slider_data, var_name == input$iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        #   # if (input$iv_top5 == "percent_fair_or_poor_health") {
        #   #  b <- filter(slider_med_data, var_name == input$iv) %>%
        #   #    dplyr::select(b) %>% unlist() %>% as.numeric()
        #   # }
        # 
        # b_top5 <- filter(slider_data, var_name == input$iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        
        # b1 <- filter(slider_data, var_name == "per_adults_with_obesity") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_with_access_to_exercise_oppurtunities") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "per_excessive_drinking") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_insufficient_sleep") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b1 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b1))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>%
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #  
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b3 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b3))
        #  
        # } else {
        #   # print(paste("Normal Route: ", b))
        # }
        
       # Get real_b from slider
       play_slider_change_b.check <<- play_slider_change_b()
       real_b <- play_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$change_top5)/100), 0)
       
        # value2_0 <- round(value * ((b * input$change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$change5)/100), 0))
        
        value2 <- abs(sum(value2_0, value2_top5, na.rm = T))
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        


        value2_per = round((value2 / value) * 100, 1)
        # 
        # print(paste0("pop impact:", value2, "      ", value, "      ", value2_per, "%"))
        # 
        value3 <- paste0(value2, " (", value2_per, "%)")
        
        infoBox(
          "Est' Population Impacted", value3, 
          fill = TRUE,
          color = "teal"
        )
    })
    
    output$rest_pop_impact <- renderInfoBox({
        
      # Region Pop
      pop_region <- rest_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      pop_county <- rest_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      if (input$rest_region != "--") {
        value <- pop_region
      } else {
        value <- pop_county
      }
        
         # slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        slider_data.check <<-  slider_data
        slider_med_data.check <<- slider_med_data
        
        # b <- filter(slider_data, var_name == input$rest_iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b_top5 <- filter(slider_data, var_name == input$rest_iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_smokers") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_black") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "average_daily_pm2_5") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_non_hispanic_white") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
       # Get real_b from slider
       rest_slider_change_b.check <<- rest_slider_change_b()
       real_b <- rest_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$rest_change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$rest_change_top5)/100), 0)
        # value2_0 <- round(value * ((b * input$rest_change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$rest_change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$rest_change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$rest_change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$rest_change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$rest_change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$rest_change5)/100), 0))
        
        value2 <- abs(sum(value2_0, value2_top5, na.rm = T))
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)

        value2_per = round((value2 / value) * 100, 1)
        
        value3 <- paste0(value2, " (", value2_per, "%)")
        
        infoBox(
          "Est' Population Impacted", value3, 
          fill = TRUE,
          color = "teal"
        )
    })
    
    output$iv_echo <- renderInfoBox({
       
        value_region <- play_select_geo_region_df() %>% 
          group_by(Region) %>% 
          summarise(across(input$iv, mean, na.rm = T)) %>% 
          dplyr::select(input$iv) %>%
            unlist() %>%
            as.numeric()  
      
        value_county <- play_select_geo_df() %>%
            dplyr::select(input$iv) %>%
            unlist() %>%
            as.numeric()
        
        if (input$region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        value <- round(value, 1)
        
        infoBox(
          input$iv, value, 
          fill = TRUE,
          color = "yellow"
        )
        
    })
    
    output$iv_change <- renderInfoBox({
        
        value <- play_select_geo_df() %>%
            dplyr::select(input$iv) %>%
            unlist() %>%
            as.character()
        
        infoBox(
          input$iv, value, 
          fill = TRUE,
          color = "yellow"
        )
        
    })
    
    output$iv_rank <- renderInfoBox({
        
        rank_df <- ana_data_wgeo %>%
            dplyr::select("fips", "state", "county", input$iv) %>%
            rename("focus_iv" = input$iv) %>%
            mutate(
             focus = ifelse(state == input$state & county == input$county, 1, 0),
             focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv),
             rank_value = rank(focus_iv),
             per_rank_value = (1 - percent_rank(focus_iv)) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
        
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "teal"
        )   
        
    })
    
    output$dv_echo <- renderInfoBox({
        
        # value <- play_select_geo_df() %>%
        #     dplyr::select(percent_physically_inactive) %>%
        #     unlist() %>%
        #     as.numeric()
        # 
        # value <- round(value, 1)
        
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
        # if (input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
        # }
        
        # if (input$iv != "Select a Measure") {
        # 
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b1 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b2 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b3 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        # 
        # } else {
        #   real_b <- slider_data$b
        #   print(paste0("Normal Route: ", input$iv_top5, "):", round(real_b, 3)))
        # }
        
       # Get real_b from slider
       play_slider_change_b.check <<- play_slider_change_b()
       real_b <- play_slider_change_b()
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county, "\n", state),
          label = ifelse(focus == 1, label, NA),
          per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive))
        
        # cat(print(paste0(slider_data$var_name, " - ", slider_data$b, "\n", real_b)))
        
        if (input$region != "--") {
          
          # data <- left_join(data, region_lkup, by = c("fips" = "FIPS")) %>%
          #   mutate(
          #     focusOrg = focus,
          #     focus = ifelse(Region == input$region, 1, focus),
          #     focus = ifelse(is.na(focus), focusOrg, focus))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$region) %>%
            mutate(label = paste0(Region, "  - ", input$state),
                   label = ifelse(focus == 1, label, NA))
          
         data <- region_data
          
        } else {
        
        data <- data %>%
          filter(state == input$state, county == input$county)
        
        }
        
        value <- round(data$per_physically_inactive, 1)
        
        infoBox(
          HTML(paste("% of Population",br(), "Physically Inactive")),
          value, 
          fill = TRUE,
          color = "navy"
        )
        
    })
    
    output$rest_dv_echo <- renderInfoBox({
        
        # Get data
        data <- rest_plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$rest_region != "--") {
          data <- data %>% mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, focus),
            focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
        # if (input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$rest_iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$rest_change_top5, top5_iv))
        # }

        # if (input$rest_iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$rest_iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$rest_change, focus_iv))
        # }
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
       # Get real_b from slider
       rest_slider_change_b.check <<- rest_slider_change_b()
       real_b <- rest_slider_change_b()
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county, "\n", state),
          label = ifelse(focus == 1, label, NA),
          per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (real_b * xchange)), per_insufficient_sleep))
        
        if (input$rest_region != "--") {
          
          # data <- left_join(data, region_lkup, by = c("fips" = "FIPS")) %>%
          #   mutate(
          #     focusOrg = focus,
          #     focus = ifelse(Region == input$region, 1, focus),
          #     focus = ifelse(is.na(focus), focusOrg, focus))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$rest_region) %>%
            mutate(label = paste0(Region, "  - ", input$rest_state),
                   label = ifelse(focus == 1, label, NA))
          
          data <- region_data
          
        } else {
          
          data <- data %>%
            filter(state == input$rest_state, county == input$rest_county)
          
        }
        
        value <- round(data$per_insufficient_sleep, 1)
        
        infoBox(
          HTML(paste("% of Population", br(), "with Insufficient Sleep")),
          value, 
          fill = TRUE,
          color = "navy"
        )
        
    })
    
    output$dv_rank <- renderInfoBox({
      
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
          b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b2 * bm
          # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
        } else {
          real_b <- slider_data$b
        }
        
        if (nrow(slider_data) < 1) {real_b <- 0}
        
        rank_df <-  ana_data_wgeo %>%
            dplyr::select(fips, state, county, per_physically_inactive) %>%
            mutate(
             focus = ifelse(state == input$state & county == input$county, 1, 0),
             per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive),
              rank_value = rank(per_physically_inactive),
              per_rank_value = (1 - percent_rank(per_physically_inactive)) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "navy"
        )   
        
    })
    
    output$rest_dv_rank <- renderInfoBox({
      
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
        rank_df <-  ana_data_wgeo %>%
            dplyr::select(fips, state, county, per_insufficient_sleep) %>%
            mutate(
             focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0),
             per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (slider_data$b * xchange)), per_insufficient_sleep),
              rank_value = rank(per_insufficient_sleep),
              per_rank_value = (1 - percent_rank(per_insufficient_sleep)) * 100
            ) %>%
            dplyr::filter(state == input$rest_state, county == input$rest_county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "navy"
        )   
        
    })
        
      output$index_echo <- renderInfoBox({
        
        value_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(score, mean, na.rm = T)) %>%
           dplyr::select(score) %>%
            unlist() %>%
            as.numeric()
        
        value_county <- play_select_geo_df() %>%
            dplyr::select(score) %>%
            unlist() %>%
            as.numeric()
        
        if (input$region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        value <- round(value, 1)
        
        infoBox(
          "Play Index", value, 
          fill = TRUE,
          color = "blue"
        )
        
     })
      
      output$rest_index_echo <- renderInfoBox({
        
        value_region <- rest_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(score, mean, na.rm = T)) %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        value_county <- rest_select_geo_df() %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        if (input$rest_region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        value <- round(value, 1)
        
        infoBox(
          "Rest Index", value, 
          fill = TRUE,
          color = "blue"
        )
        
     })
        
      output$index_rank <- renderInfoBox({
        
        rank_df <- play_z_geo_df() %>%
            dplyr::select(fips, state, county, score) %>%
            mutate(
              rank_value = rank(-score),
              per_rank_value = percent_rank(score) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "blue"
        )   
      })
      
      output$rest_index_rank <- renderInfoBox({
        
        rank_df <- rest_z_geo_df() %>%
            dplyr::select(fips, state, county, score) %>%
            mutate(
              rank_value = rank(-score),
              per_rank_value = percent_rank(score) * 100
            ) %>%
            dplyr::filter(state == input$rest_state, county == input$rest_county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "blue"
        )   
      })
      
      
    # Card that show correlation between Play Index and the 4 Criterion
      
     output$criterion1_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$years_of_potential_life_lost_rate, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Year of Potential",br(), "Life Lost")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion2_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Avg # of",br(), "Physically Unhealthy Days")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion3_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Avg # of",br(), "Mentally Unhealthy Days")),
          value,
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion4_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$preventable_hospitalization_rate, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Preventable",br(), "Hospitalization Rate")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })
     
    
    # --- Extra Row Card ---
     
    # Physical Inactivity change for card
    phy_inactive_change <- reactive({
      
        # Physical Inactivity
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
       #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "top5_iv" = input$iv_top5)  %>%
       #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
       #  }
       #  
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # 
        # if (input$iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
        # } else {
        #   real_b <- slider_data$b
        # }
        
       # Get real_b from slider
       real_b <- play_slider_change_b()
        
        change <- real_b * xchange
        
        return(change)
        
    })
    
    
    # Extra data grouping
    extra_data_for_card <- reactive({
        
        select_data <- play_z_geo_df()
        data <- left_join(select_data, ana_data_criterion, by = c("fips", "state", "county"))
        
        data <- data %>%
          left_join(data_labour_1, by = c("fips" = "FIPS"))
  
        if (input$region != "--") {
          
          data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
          
          data <- data %>% 
            filter(Region == input$region) %>%
            ungroup() %>%
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T))
           
        } else {
          
          data <- filter(data, state == input$state, county == input$county)
          
        } 
        
        return(data)
    })

     
    # New cards (4):  
     # Years of Potential Life Lost Rate, 
     # Average Number of Physically Unhealthy Days, 
     # Average Number of Mentally Unhealthy Days, 
     # Premature age-adjusted mortality
     
    output$play_impact_card_1 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
      
        # value <- cor(data$score, data$years_of_potential_life_lost_rate, 
        #     method = "pearson", use = "complete.obs")
        
        model <- lm(years_of_potential_life_lost_rate ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
              
        # b.check <<- b
        # select_data.check <<- data
        # phy_inactive_change.check <<- phy_inactive_change
        
        final_value <- round(data$years_of_potential_life_lost_rate - (b * phy_inactive_change), 1)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste(" Years of Potential", br(), "Life Lost Rate")),
          final_value,
          fill = TRUE,
          color = "olive"
        )
      
    })
    
    output$play_impact_card_2 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(avg_no_of_physically_unhealthy_days ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
        
        final_value <- round(data$avg_no_of_physically_unhealthy_days - (b * phy_inactive_change), 2)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste("Average # of", br(), "Physically Unhealthy Days")),
          final_value,
          fill = TRUE,
          color = "olive"
        )
      
    })
    
    output$play_impact_card_3 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(avg_no_of_mentally_unhealthy_days ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
        
        final_value <- round(data$avg_no_of_mentally_unhealthy_days - (b * phy_inactive_change), 2)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste("Average # of", br(), "Mentally Unhealthy Days")),
          final_value,
          fill = TRUE,
          color = "olive"
        )
      
    })
    
    output$play_impact_card_4 <- renderInfoBox({

        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
        tidy()

        b <- model$estimate[2]

        data <- extra_data_for_card()
        
        final_value <- round(data$age_adjusted_death_rate - (b * phy_inactive_change), 1)

        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))

        infoBox(
          HTML(paste("Premature Age Adjusted", br(), "Mortality")),
          final_value,
          fill = TRUE,
          color = "olive"
        )

    })

    # --- Extra Impact Card ---
    # Optional for some Clients
     
    output$play_extra_impact_card <- renderInfoBox({
      
      # annual_avg_emplvl * 166 (from literature per capita cost)
      
        # data <- play_select_geo_df()
        
        data <- extra_data_for_card()
        
        # Join with labour data
        # data <- data %>%
        #   left_join(data_labour_1, by = c("fips" = "FIPS"))
        
        # Pop change impact repeat here
        # TODO: move to reactive
        
        if (input$region == "--") {
          value <-  play_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        } else {
          value <-  play_select_geo_region_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
          value <- sum(value)
        }
        
         # slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        # b <- filter(slider_data, var_name == input$iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b_top5 <- filter(slider_data, var_name == input$iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        # # b1 <- filter(slider_data, var_name == "per_adults_with_obesity") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b3 <- filter(slider_data, var_name == "per_with_access_to_exercise_oppurtunities") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b4 <- filter(slider_data, var_name == "per_excessive_drinking") %>%
        # #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b5 <- filter(slider_data, var_name == "per_insufficient_sleep") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        # 
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b1 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>%
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #  
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b3 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        #  
        # } else {
        #   print(paste("Normal Route: ", b))
        # }
        
        # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$change_top5)/100), 0)
        
        # value2_0 <- round(value * ((b * input$change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$change_top5)/100), 0)
        # value2_1 <- round(value * ((b1 * input$change1)/100), 0)
        # value2_2 <- round(value * ((b2 * input$change2)/100), 0)
        # value2_3 <- round(value * ((b3 * input$change3)/100), 0)
        # value2_4 <- round(value * ((b4 * input$change4)/100), 0)
        # value2_5 <- round(value * ((b5 * input$change5)/100), 0)
        
        value2 <- sum(value2_0, value2_top5, na.rm = T)
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        
        # print(data$annual_avg_emplvl / value)
         
        # estimate % of employee impacted based on proportion of employed population
        pop_change_value <- (data$annual_avg_emplvl / value) * value2
        
        final_value <- (data$annual_avg_emplvl + pop_change_value ) * 166
        
        final_value <- paste0("$", formatC(round(final_value, 1), format="f", big.mark=",", digits=1))
        
        infoBox(
          HTML(paste("Physical Inactivity", br(), " Absenteeism Cost to Employer")),
          final_value, 
          fill = TRUE,
          color = "olive"
        )
      
      
    })
    
    
    output$rest_extra_impact_card <- renderInfoBox({
      
      # Companies lose an estimated $2,280 per employee each year due to sleep deprivation
      
        data <- rest_select_geo_df()
        
        # Join with labour data
        data <- data %>%
          left_join(data_labour_1, by = c("fips" = "FIPS"))
        
        # Pop change impact repeat here
        # TODO: move to reactive
        value <-  rest_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
         # slider data
        slider_data <- rest_slider_data()
        
        b <- filter(slider_data, var_name == input$rest_iv) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        b_top5 <- filter(slider_data, var_name == input$rest_iv_top5) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        # b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_smokers") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_black") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "average_daily_pm2_5") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_non_hispanic_white") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
        
        value2_0 <- round(value * ((b * input$rest_change)/100), 0)
        value2_top5 <- round(value * ((b_top5 * input$rest_change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$rest_change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$rest_change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$rest_change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$rest_change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$rest_change5)/100), 0))
        
        value2 <- sum(value2_0, value2_top5, na.rm = T)
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        
        # print(data$annual_avg_emplvl / value)
        
        # estimate % of employee impacted based on proportion of employed population
        pop_change_value <- (data$annual_avg_emplvl / value) * value2
        
        final_value <- (data$annual_avg_emplvl - pop_change_value ) * 2280 
        
        final_value <- paste0("$", formatC(round(final_value, 1), format="f", big.mark=",", digits=1))
        
        infoBox(
          HTML(paste("Insufficient Sleep", br(), "Cost")),
          final_value, 
          fill = TRUE,
          color = "olive"
        )
      
      
    })

        
    
    # --- Chart and Plot ----
     
     z_geo_w_criterion_df <- reactive({
      
       data <- play_z_geo_df()
       
       # Join with criterion variable
       data <- left_join(data, ana_data_criterion, by = c("fips", "state", "county"))
       
       return(data)
       
     })
      
      
    # Simple between the Play index and the four criterion
      
    output$test_criterion_plot_1 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$years_of_potential_life_lost_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, years_of_potential_life_lost_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
      
    output$test_criterion_plot_2 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_physically_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
      labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_3 <- renderPlot({
      
     data <- z_geo_w_criterion_df()
    
     value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_mentally_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
      labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_4 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$preventable_hospitalization_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, preventable_hospitalization_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Play index correlation with additional measure
    # % Uninsured (rank), 
    # Primary Care Physicians Rate (rank),
    # % Unemployed (rank), 
    # 20th Percentile Income (rank), 
    # % Single-Parent Households (rank), 
    # Severe Housing Cost Burden (rank), 
    # Violent Crime Rate (rank), and 
    # Social Association Rate (rank).  Please post the result to the 'Play Index' view
    
    output$test_criterion_plot_a1 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_uninsured, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_uninsured)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a2 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$primary_care_physicians_ratio, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, primary_care_physicians_ratio)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a3 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_unemployed, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_unemployed)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a4 <- renderPlot({
          
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$x20th_perile_income, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, x20th_perile_income)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a5 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_single_parent_households, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_single_parent_households)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a6 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$severe_housing_cost_burden, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, severe_housing_cost_burden)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a7 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$violent_crime_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, violent_crime_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a8 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$social_association_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_association_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a9 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_adults_with_diabetes, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_adults_with_diabetes)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a10 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
     value <- cor(data$score, data$medicare_spending_age_sex_race_adjusted_4, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_age_sex_race_adjusted_4)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a11 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
     value <- cor(data$score, data$medicare_spending_price_age_sex_race_adjusted_5, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_price_age_sex_race_adjusted_5)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Play index scatter grid plot
    output$play_grid_plot <- renderPlot({
      
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        

        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (input$region != "--") {
          slider_data <- bind_rows(slider_data, slider_med_data)
        }
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        
        # Make focus generic
       #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "top5_iv" = input$iv_top5)  %>%
       #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
       #  }
       #  
       #  
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # 
        # if (input$iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
      # Get real_b from slider
      real_b <- play_slider_change_b()
      
      
        # Modify the data
        data <- mutate(data,
          label = paste0(county),
          label = ifelse(focus == 1, label, NA),
          per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive))
        

        
        if (input$region != "--") {
          
          data <- mutate(data,
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus),
                 label = ifelse(focus == 1, label, NA))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$region) %>%
            mutate(label = paste0(Region),
                   label = ifelse(focus == 1, label, NA))
          
          
        } else {
           data <- data
        }
        
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xlabel <- input$iv_top5
        } else {
          xlabel <- input$iv
        }
        
        # Make plot
        
        if (input$region == "--") {
          ggplot(data, 
            aes(per_physically_inactive, score, color = focus)) +
            geom_point(size = 5) +
            geom_label_repel(aes(label = label),
                       color = "black") +
            labs(y = "Play Index (0 to 100)", x = " % of Population Physically Inactive") +
            theme_minimal() +
            theme(legend.position = "none")   
        } else {
          ggplot(data, 
            aes(per_physically_inactive, score, color = focus)) +
            geom_point(size = 5) +
            geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
            geom_label_repel(aes(label = label),
                       color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
            geom_label_repel(data = region_data, aes(label = label),
                       color = "black", size = 7) +
            labs(y = "Play Index (0 to 100)", x = " % of Population Physically Inactive") +
            theme_minimal() +
            theme(legend.position = "none")     
        }

        
    })
    
    # Rest index scatter grid plot
    output$rest_grid_plot <- renderPlot({
      
        # Get data
        data <- rest_plot_data()
        
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$rest_region != "--") {
          data <- data %>% mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, focus),
            focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (input$rest_region != "--") {
          slider_data <- bind_rows(slider_data, slider_med_data)
        }
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        
        # Make focus generic
        # if (input$rest_iv_top5 != "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$rest_iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$rest_change_top5, top5_iv))
        # }
        # 
        # if (input$rest_iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$rest_iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$rest_change, focus_iv))
        # }
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
        # Get real_b from slider
        real_b <- rest_slider_change_b()
        
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county),
          label = ifelse(focus == 1, label, NA),
          per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (real_b * xchange)), per_insufficient_sleep))
        
        
        if (input$rest_region != "--") {
          
          data <- mutate(data,
                         focusOrg = focus,
                         focus = ifelse(Region == input$rest_region, 1, focus),
                         focus = ifelse(is.na(focus), focusOrg, focus),
                         label = ifelse(focus == 1, label, NA))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$rest_region) %>%
            mutate(label = paste0(Region),
                   label = ifelse(focus == 1, label, NA))
          
          
        } else {
          data <- data
        }
        
        
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xlabel <- input$rest_iv_top5
        } else {
          xlabel <- input$rest_iv
        }
        
        # Make plot
        if (input$rest_region == "--") {
          
          ggplot(data, 
                 aes(per_insufficient_sleep, score, color = focus)) +
            geom_point(size = 5) +
            geom_label_repel(aes(label = label),
                             color = "black") +
            labs(y = "Rest Index (0 to 100)", x = "% of Population with Insufficient Sleep") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else {
          
          ggplot(data, 
                 aes(per_insufficient_sleep, score, color = focus)) +
            geom_point(size = 5) +
            geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
            geom_label_repel(aes(label = label),
                             color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
            geom_label_repel(data = region_data, aes(label = label),
                             color = "black", size = 7) +
            labs(y = "Rest Index (0 to 100)", x = "% of Population with Insufficient Sleep") +
            theme_minimal() +
            theme(legend.position = "none")     
        }
        

        
    })
    
    
    # --- Test Table ---
    
    output$play_model_table <- DT::renderDataTable({
       datatable(m_step_df_play,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_play)[-1], digits = 2)
    })
    
    output$fp_health_model_table <- DT::renderDataTable({
       datatable(m_step_df_fp_health,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_fp_health)[-1], digits = 2)
    })
    
    output$grad_model_table <- DT::renderDataTable({
       datatable(m_step_df_grad,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_grad)[-1], digits = 2)
    })
    
    output$diabetes_model_table <- DT::renderDataTable({
       datatable(m_step_df_diabetes,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_diabetes)[-1], digits = 2)
    })
    
    output$rest_model_table <- DT::renderDataTable({
       datatable(m_step_df_rest,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_rest)[-1], digits = 2)
    })
    
    output$doc_checkup_model_table <- DT::renderDataTable({
       datatable(m_step_df_doc_checkup,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_doc_checkup)[-1], digits = 2)
    })
    
    output$ypll_model_table <- DT::renderDataTable({
       datatable(m_step_df_ypll,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_ypll)[-1], digits = 2)
    })
    
    output$avg_m_days_model_table <- DT::renderDataTable({
       datatable(m_step_df_avg_m_days,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_avg_m_days)[-1], digits = 2)
    })
    
    
    # --- IV Contribution ---
    output$iv_contr_stack_plot <- renderPlot({
      
 data <- m_step_df_play %>% arrange(pratt) %>% 
        mutate(
          rank = rank(-pratt),
          order_var_name = ifelse(rank <= 3, var_name, "other")
        ) %>%
      filter(rank <= 3)
      
      # Create fake other
      data_other <- tibble(
        var_name = "Other",
        pratt = 1 - sum(data$pratt))
      
      data <- bind_rows(data, data_other)
      
      ggplot(data, aes(1, pratt, fill = reorder(var_name, pratt))) + 
              geom_bar(position = "fill", stat = "identity") +
              geom_text(aes(label = 
              str_replace(paste(var_name, "\n",            
                round(pratt * 100), 1), "percent_", "% ")), position = "fill", hjust = 0.5, angle = 90, vjust = 0) +
              coord_flip() + 
              theme_minimal() +
                theme(
                legend.position = "none",          
                axis.title = element_blank(),
                axis.text = element_blank()) +
      scale_fill_brewer(palette = "YlGnBu")
      
    })
    
    output$iv_contr_plot <- renderPlot({
      
      data <- m_step_df_play %>% arrange(pratt) %>% 
        mutate(
          rank = rank(-pratt),
          order_var_name = ifelse(rank <= 3, var_name, "other")
        ) %>%
      filter(rank <= 3)
      
      # Create fake other
      data_other <- tibble(
        var_name = "Other",
        pratt = 1 - sum(data$pratt))
      
      data <- bind_rows(data, data_other)
      
      ggplot(data, aes(reorder(var_name, -pratt), 1, fill = pratt)) + 
        geom_tile() + 
        geom_text(aes(label = 
            str_replace(paste(var_name, "\n",            
            round(pratt * 100), 1), "percent_", "% "))) +
        theme_minimal() +
        theme(
          legend.position = "none",          
          axis.title = element_blank(),
          axis.text = element_blank()) +
        scale_fill_gradient(low = "grey", high = "yellow")
      
    })
    
    # --- Test Play Index ---
    output$play_index_hist <- renderPlot({

      data <- play_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_histogram() +
        theme_minimal()
      
    })
    
    output$play_index_boxplot <- renderPlot({

      data <- play_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_boxplot() +
        theme_minimal()
      
    })
    
    output$play_table <- DT::renderDataTable({
      
       datatable(play_z_geo_df,
         options = list(
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_play)[-1], digits = 2)
    })
    
    # Map of Play Index Quintiles
    output$map <- renderPlot({
      # see https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
    map_data <- left_join(play_fixed_z_data_wgeo, countydata, by = c("fips" = "county_fips")) %>%
      left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
    
    ggplot(map_data, aes(long, lat, group = fips, fill = quintile)) +
      geom_polygon(color = NA) +
      coord_map() +
      labs(fill = "Play Index") +
      theme_minimal() +
      scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
      guides(fill = guide_colourbar(nbbin = 100)) +
      theme(legend.position = "bottom",
            legend.key.width = unit(7, "cm"))
    
    })
    
})
