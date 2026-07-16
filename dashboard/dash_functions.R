# functions for GP dashboard

# function to get prevalence by year
prev_df <- function(df, date_end_yr){
  
  df <- df %>%
    # calculate age at end of the financial year
    mutate(age_at_eoy = as.integer(time_length(interval(date_of_birth, date_end_yr), 'years')),
           # age_at_eoy = age_calculate(date_of_birth), date_end_yr,
           # inv dash looks like it use one instance of age at times and not calculating at each year from SLF
           # age_group = create_age_groups(age_at_diagnosis, from = 0, to = 90, by = 5, as_factor = TRUE),
           age_group = create_age_groups(age_at_eoy, from = 0, to = 90, by = 5, as_factor = TRUE),
           age_group = case_when(age_group < "60-64" ~ "0-59",
                                 .default = as.character(age_group)),
           age_group = factor(age_group, levels = age_order, ordered = T),
           gender = case_when(sex == 1 ~ 'Male',
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
           hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
    summarise(individuals = n(),
              # reporting from 2017/18 so use simd 2020
              .by = c(year, hscp2019name, age_group, gender, simd2020v2_sc_quintile, source)) %>% 
    arrange(hscp2019name, year, age_group)
  
  return(prev_year)
}

prev_pl_df <- function(df, date_end_yr){
  
  df <- df %>%
    # calculate age at end of the financial year
    mutate(age_at_eoy = floor(time_length(interval(date_of_birth, date_end_yr), 'years')),
           # inv dash looks like it use one instance of age at times and not calculating at each year from SLF
           # age_group = create_age_groups(age_at_diagnosis, from = 0, to = 90, by = 5, as_factor = TRUE),
           age_group = case_when(age_at_eoy < 60 ~ "18-59",
                                 .default = create_age_groups(age_at_eoy, from = 60, to = 90, by = 5, as_factor = TRUE)),
           age_group = factor(age_group, levels = age_order, ordered = T),
           gender = case_when(sex == 1 ~ 'Male',
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
           hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
    select(year, postcode, hscp2019name, age_group, gender, simd2020v2_sc_quintile, ur6_2022_name, ur8_2022_name, source)
}

theme_dash <- function() {
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25),
        axis.line.x.bottom = element_line(colour = 'black', linewidth = 0.25),
        axis.ticks = element_blank())
}

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

remove_modebar_buttons <- function() {
  config(modeBarButtonsToRemove = bttn_remove, displaylogo = FALSE)
}

knit_rmd <- function(){
  rmarkdown::render(paste0(here(), "/dashboard/Dementia Prevalence.Rmd"),
                    output_dir = "/PHI_conf/Dementia_Index/outputs/HSCP dashboard/",
                    output_file = paste0("Dementia Prevalence_", format(Sys.Date(), "%Y_%m_%d"), ".html"))
}

prev_pl_df_gp <- function(df, date_end_yr){
  
  df <- df %>%
    filter(age_at_diagnosis >= 25) %>% 
    # calculate age at end of the financial year
    mutate(age_at_eoy = floor(time_length(interval(date_of_birth, date_end_yr), 'years')),
           # inv dash looks like it use one instance of age at times and not calculating at each year from SLF
           # age_group = create_age_groups(age_at_diagnosis, from = 0, to = 90, by = 5, as_factor = TRUE),
           
           age_group = create_age_groups(age_at_eoy, from = 25, to = 85, by = 10, as_factor = TRUE),
           age_group = case_when(age_group %in% c("25-34", "35-44") ~ "25-44",
                                 age_group %in% c("45-54", "55-64") ~ "45-64",
                                 .default = age_group),
           age_group = factor(age_group, levels = age_order_gp, ordered = T),
           gender = case_when(sex == 1 ~ 'Male',
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
           hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
    select(year, postcode, hscp2019name, age_group, gender, simd2020v2_sc_quintile, ur6_2022_name, ur8_2022_name, source)
}