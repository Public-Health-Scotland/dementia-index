# functions for publication code

# get patient level prevalence data
prev_pl_df <- function(df, date_end_yr){
  # function takes in the index data (df) and date at the end of the year (either calendar or fy)
  
  df <- df %>%
    # calculate age at end of the financial year
    mutate(age_at_eoy = floor(time_length(interval(date_of_birth, date_end_yr), 'years')),
           # inv dash looks like it use one instance of age at times and not calculating at each year from SLF
           # age_group = create_age_groups(age_at_diagnosis, from = 0, to = 90, by = 5, as_factor = TRUE),
           age_group = case_when(age_at_eoy < 60 ~ "18-59",
                                 .default = create_age_groups(age_at_eoy, from = 60, to = 90, by = 5, as_factor = TRUE)),
           age_group = factor(age_group, levels = age_order, ordered = T),
           sex = case_when(sex == 1 ~ 'Male',
                              sex == 2 ~ "Female",
                              .default = NA),
           diag_year = extract_fin_year(diagnosis_date))
  
  
  fy_yr1 <- substr(date_end_yr, 1, 4)
  fy_date <- paste0(as.character(as.numeric(fy_yr1)-1), "/", substr(fy_yr1, 3, 4))
  
  prev_year <- df %>% 
    filter(diagnosis_date <= date_end_yr,
           # keep those with a date of death greater than the end of that year
           # or those who have not died but were diagnosed by that point
           (date_of_death > date_end_yr | is.na(date_of_death))
    ) %>% 
    mutate(year = fy_date,
           hscp2019name = factor(hscp2019name, levels = area_order, ordered = T),
           hbres = factor(hbres, levels = area_order, ordered = T),
           cal_year = as.numeric(substr(year, 1, 4))) %>% 
    select(year, cal_year, postcode, hscp2019name, hbres, age_group, sex, simd2020v2_sc_quintile, source)
}

theme_dash <- function() {
  theme(plot.title = element_text(face = "bold", size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 20, margin = margin(0,0,25,0), hjust = 0.5), axis.title = element_text(size = 20),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25),
        axis.line.x.bottom = element_line(colour = 'black', linewidth = 0.25),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 18))
}

theme_bar <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      # margin - t, r, b , l
      plot.margin = margin(5, 25, 5, 5),
      plot.title = element_text(face = "bold", size = 25, hjust = 0.5), 
      plot.subtitle = element_text(size = 20, margin = margin(0,0,25,0), hjust = 0.5), axis.title = element_text(size = 20),
      legend.position = "none",
      panel.grid.major.y = element_line(color = "#878787", linewidth = 0.25, linetype = 2),
      axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_text(vjust = 0.5, hjust = 0.5, face = "bold", size = 18), #angle = 50),
      axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_text(face = "bold", size = 18)
    )
}

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

remove_modebar_buttons <- function() {
  config(modeBarButtonsToRemove = bttn_remove, displaylogo = FALSE)
}


## EASR Functions ####
# Outputs prevalence data in 5 year age groups upto 90+
prev_pl_df_easr <- function(df, date_end_yr){
  # function takes in the index data (df) and date at the end of the year (either calendar or fy)
  
  df <- df %>%
    # calculate age at end of the financial year
    mutate(age_at_eoy = floor(time_length(interval(date_of_birth, date_end_yr), 'years')),
           # inv dash looks like it use one instance of age at times and not calculating at each year from SLF
           # age_group = create_age_groups(age_at_diagnosis, from = 0, to = 90, by = 5, as_factor = TRUE),
           age_group = create_age_groups(age_at_eoy, as_factor = TRUE),
           # age_group = factor(age_group, levels = age_order, ordered = T),
           diag_year = extract_fin_year(diagnosis_date))
  
  
  fy_yr1 <- substr(date_end_yr, 1, 4)
  fy_date <- paste0(as.character(as.numeric(fy_yr1)-1), "/", substr(fy_yr1, 3, 4))
  
  prev_year <- df %>% 
    filter(diagnosis_date <= date_end_yr,
           # keep those with a date of death greater than the end of that year
           # or those who have not died but were diagnosed by that point
           (date_of_death > date_end_yr | is.na(date_of_death))
    ) %>% 
    mutate(year = fy_date,
           hscp2019name = factor(hscp2019name, levels = area_order, ordered = T),
           hbres = factor(hbres, levels = area_order, ordered = T),
           cal_year = as.numeric(substr(year, 1, 4))) %>% 
    select(year, cal_year, postcode, hscp2019name, hbres, age_group, sex, simd2020v2_sc_quintile, source)
}

# Functions below adapted from ScotPHO code
# https://github.com/Public-Health-Scotland/scotpho-indicator-production/blob/0ee39f19bf3b4960a12d1a50eccc1976f8d29888/functions/helper%20functions/calculate_easr.R
calculate_easr <- function(data, epop_total,
                           area_type,
                           epop_age = c("normal", "18+", "16+")){
  
  # check function arguments
  if(!is.numeric(epop_total)){
    cli::cli_abort(c(
      "{.var epop_total} must be a number",
      "x" = "You've supplied a {.cls {class(epop_total)}} vector."))
  }
  
  epop_age <- rlang::arg_match(epop_age)
  
  
  if (epop_age == "normal") {
    data$epop <- recode(as.character(data$epop), 
                        "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                        "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                        "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                        "13" = 6000, "14" = 5500, "15" = 5000,
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } else if (epop_age == "18+") {
    data$epop <- recode(as.character(data$epop), 
                        "4" = 2200, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000) # 80,700
  }  else if (epop_age == "16+") {
    data$epop <- recode(as.character(data$age_group), 
                        "4" = 4400, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  }
  
  # Calculating individual easr and variance
  data <- data |>
    mutate(easr_first = numerator * epop/denominator, # easr population
           var_dsr = (numerator * epop^2)/denominator^2) |>  # variance
    # Converting Infinites to NA and NA's to 0s to allow proper functioning
    mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # Caused by a denominator of 0 in an age group with numerator >0
           var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) |>
    mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0))
  
  
  # aggregating by year, code and time
  data <- data |>
    select(-c(age_group, sex))|>
    group_by(across(any_of(c("year", "area", "area_type", "quintile", "quint_type")))) |>
    summarise_all(sum, na.rm =T) |>
    ungroup()
  
  # Calculating rates and confidence intervals
  data <- data |>
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           o_lower = numerator * (1 - (1/(9 * numerator)) - (1.96/(3 * sqrt(numerator))))^3,  # Lower CI
           o_upper = (numerator + 1)*(1 - (1/(9 * (numerator + 1))) +
                                        (1.96/(3 * sqrt(numerator+1))))^3, # Upper CI
           var = (1/epop_total^2) * var_dsr, # variance
           rate = easr * 100000,  # rate calculation
           lowci = (easr + sqrt(var/numerator) * (o_lower - numerator)) * 100000, # Lower CI final step
           upci = (easr + sqrt(var/numerator) * (o_upper - numerator)) * 100000) # Upper CI final step
  
  
  # remove temporary variables
  data <- data |>
    select(-c(var, easr, epop_total, o_lower, o_upper, easr_first, epop, var_dsr))
  
  
  data <- data |>
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) |>
    mutate(lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci))
  
  data <- data  |>
    relocate(numerator, .after = area) |> 
    mutate(area_type = area_type) |> 
    relocate(area_type, .after = area)
  
  return(data)
  
}


calculate_easr_sex <- function(data, epop_total,
                           area_type,
                           epop_age = c("normal", "18+", "16+")){
  
  # check function arguments
  if(!is.numeric(epop_total)){
    cli::cli_abort(c(
      "{.var epop_total} must be a number",
      "x" = "You've supplied a {.cls {class(epop_total)}} vector."))
  }
  
  epop_age <- rlang::arg_match(epop_age)
  
  
  if (epop_age == "normal") {
    data$epop <- recode(as.character(data$epop), 
                        "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                        "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                        "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                        "13" = 6000, "14" = 5500, "15" = 5000,
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } else if (epop_age == "18+") {
    data$epop <- recode(as.character(data$epop), 
                        "4" = 2200, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000) # 80,700
  } else if (epop_age == "16+") {
    data$epop <- recode(as.character(data$age_group), 
                        "4" = 4400, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } 
  
  # Calculating individual easr and variance
  data <- data |>
    mutate(easr_first = numerator * epop/denominator, # easr population
           var_dsr = (numerator * epop^2)/denominator^2) |>  # variance
    # Converting Infinites to NA and NA's to 0s to allow proper functioning
    mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # Caused by a denominator of 0 in an age group with numerator >0
           var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) |>
    mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0))
  
  
  # aggregating by year, code and time
  data <- data |>
    select(-c(age_group))|>
    group_by(across(any_of(c("year", "area", "area_type", "sex")))) |>
    summarise_all(sum, na.rm =T) |>
    ungroup()
  
  # Calculating rates and confidence intervals
  data <- data |>
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           o_lower = numerator * (1 - (1/(9 * numerator)) - (1.96/(3 * sqrt(numerator))))^3,  # Lower CI
           o_upper = (numerator + 1)*(1 - (1/(9 * (numerator + 1))) +
                                        (1.96/(3 * sqrt(numerator+1))))^3, # Upper CI
           var = (1/epop_total^2) * var_dsr, # variance
           rate = easr * 100000,  # rate calculation
           lowci = (easr + sqrt(var/numerator) * (o_lower - numerator)) * 100000, # Lower CI final step
           upci = (easr + sqrt(var/numerator) * (o_upper - numerator)) * 100000) # Upper CI final step
  
  
  # remove temporary variables
  data <- data |>
    select(-c(var, easr, epop_total, o_lower, o_upper, easr_first, epop, var_dsr))
  
  
  data <- data |>
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) |>
    mutate(lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci))
  
  data <- data  |>
    relocate(numerator, .after = sex) |> 
    mutate(area_type = area_type) |> 
    relocate(area_type, .after = area)
  
  return(data)
  
}


calculate_easr_age <- function(data, epop_total,
                               area_type,
                               epop_age = c("normal", "18+", "16+")){
  
  # check function arguments
  if(!is.numeric(epop_total)){
    cli::cli_abort(c(
      "{.var epop_total} must be a number",
      "x" = "You've supplied a {.cls {class(epop_total)}} vector."))
  }
  
  epop_age <- rlang::arg_match(epop_age)
  
  
  if (epop_age == "normal") {
    data$epop <- recode(as.character(data$epop), 
                        "1" = 74500, "13" = 6000, "14" = 5500, "15" = 5000,
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } else if (epop_age == "18+") {
    data$epop <- recode(as.character(data$epop), 
                        "4" = 2200, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000) # 80,700
  } else if (epop_age == "16+") {
    data$epop <- recode(as.character(data$age_group), 
                        "4" = 4400, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  }
  
  # Calculating individual easr and variance
  data <- data |>
    mutate(easr_first = numerator * epop/denominator, # easr population
           var_dsr = (numerator * epop^2)/denominator^2) |>  # variance
    # Converting Infinites to NA and NA's to 0s to allow proper functioning
    mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # Caused by a denominator of 0 in an age group with numerator >0
           var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) |>
    mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0))
  
  
  # aggregating by year, code and time
  data <- data |>
    select(-c(sex))|>
    group_by(across(any_of(c("year", "area", "area_type", "age_group")))) |>
    summarise_all(sum, na.rm =T) |>
    ungroup()
  
  # Calculating rates and confidence intervals
  data <- data |>
    mutate(epop_total = epop*2,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           o_lower = numerator * (1 - (1/(9 * numerator)) - (1.96/(3 * sqrt(numerator))))^3,  # Lower CI
           o_upper = (numerator + 1)*(1 - (1/(9 * (numerator + 1))) +
                                        (1.96/(3 * sqrt(numerator+1))))^3, # Upper CI
           var = (1/epop_total^2) * var_dsr, # variance
           rate = easr * 100000,  # rate calculation
           lowci = (easr + sqrt(var/numerator) * (o_lower - numerator)) * 100000, # Lower CI final step
           upci = (easr + sqrt(var/numerator) * (o_upper - numerator)) * 100000) # Upper CI final step
  
  
  # remove temporary variables
  data <- data |>
    select(-c(var, easr, epop_total, o_lower, o_upper, easr_first, epop, var_dsr))
  
  
  data <- data |>
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) |>
    mutate(lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci))
  
  data <- data  |>
    relocate(numerator, .after = age_group) |> 
    mutate(area_type = area_type) |> 
    relocate(area_type, .after = area)
  
  return(data)
  
}
