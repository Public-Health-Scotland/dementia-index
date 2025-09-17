library(here)
library(odbc)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(purrr)
library(hablar)
library(readxl)
library(arrow)
library(lubridate)
library(phsmethods)
# remotes::install_github("Public-Health-Scotland/phslookups") 
library(phslookups)
library(ggplot2)
library(plotly)

library(crosstalk)
library(DT)
library(bsicons)

# Dementia index
index <- readRDS('/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds') %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name))

index_first <- readRDS('/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds') %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name))


area_order <- c("Scotland", sort(unique(index$hscp2019name)[1:31]))
# index_first %>%
#   mutate(year = extract_fin_year(diagnosis_date)) %>% 
#   summarise(individuals = n(),
#             .by = c(year, ca2019name, source)) %>%
#   filter(year>=2014, ca2019name == 'Inverclyde') %>% 
#   arrange(source, year, ca2019name)
# 
# index %>%
#   mutate(year = extract_fin_year(diagnosis_date)) %>% 
#   summarise(individuals = n(),
#             .by = c(year, ca2019name, source)) %>%
#   filter(year>=2014, ca2019name == 'Inverclyde') %>% 
#   arrange(source, year, ca2019name)

# Number of individuals identified as having dementia by financial year, age group, gender & HSCP ####
age_order <- c("18-59", create_age_groups(seq(60, 90, by=5), 60, 90, by = 5, as_factor = F))

dates <- dmy(31032018)
dates <- seq.Date(dates, dates+years(6), by = 'year')
fy_dates <- sort(unique(extract_fin_year(index$diagnosis_date)))[9:15]

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

## bind all years of interest together ####
### number with source ####
# prevalence_ca_source <- bind_rows(lapply(dates, prev_df, df = index))
# prev_ca_source_first <- bind_rows(lapply(dates, prev_df, df = index_first))
prevalence_ca_source <- bind_rows(lapply(dates, prev_pl_df, df = index))
prev_ca_source_first <- bind_rows(lapply(dates, prev_pl_df, df = index_first))

prev_sc_totals <- prev_ca_source_first %>% 
  mutate(hscp2019name = 'Scotland') %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name)) %>% 
  mutate(source = 'Patient Total') %>% 
  arrange(year, hscp2019name, source) %>% 
  pivot_wider(names_from = source, values_from = individuals, values_fill = 0)

prev_ca_totals <- prev_ca_source_first %>% 
      summarise(individuals = n(),
                .by = c(year, hscp2019name)) %>% 
      mutate(source = 'Patient Total') %>% 
      arrange(hscp2019name, year, source) %>% 
      pivot_wider(names_from = source, values_from = individuals, values_fill = 0)

# Scotland figures
prev_sc_source <- prevalence_ca_source %>%
  mutate(hscp2019name = "Scotland") %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name, source)) %>%
  arrange(year, hscp2019name, source) %>% 
  pivot_wider(names_from = source, values_from = individuals, values_fill = 0) %>% 
  left_join(prev_sc_totals)

prev_all_source <- prev_sc_source %>% 
  bind_rows(
    prevalence_ca_source %>%
      filter(!is.na(hscp2019name)) %>% 
      summarise(individuals = n(),
                .by = c(year, hscp2019name, source)) %>%
      mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
      arrange(year, hscp2019name, source) %>% 
      pivot_wider(names_from = source, values_from = individuals, values_fill = 0) %>% 
      left_join(prev_ca_totals)
    ) %>% 
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = TRUE)) %>% 
  arrange(hscp2019name, year)

### numbers w/o source ####
prevalence_ca <- prev_ca_source_first %>% 
  select(-source) %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name, age_group, gender)) %>% 
  arrange(hscp2019name, year, age_group)

## check inverclyde against LIST dashboard ####
prevalence_ca %>% filter(grepl("nverc", hscp2019name)) %>% summarise(n = sum(individuals), .by = c(year, hscp2019name, gender)) %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  adorn_totals(where = 'col') %>% clean_names() %>% 
  relocate(female, .before = male)

prevalence_sc <- prevalence_ca %>%
  mutate(hscp2019name = 'Scotland',
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = TRUE)) %>%
  summarise(individuals = sum(individuals),
            .by = c(year, hscp2019name, age_group, gender)) %>% 
  arrange(hscp2019name, year, age_group)

## check against previous IR - numbers seem a little lower ####
# especially in 2018/19
prevalence_sc %>% 
  summarise(individuals = sum(individuals),
            .by = c(year, hscp2019name))

prevalence_all <- prevalence_sc %>%
  bind_rows(prevalence_ca %>% filter(!is.na(hscp2019name))) %>%
  mutate(lookup = paste0(year, hscp2019name, age_group, gender)) %>% 
  relocate(lookup, .before = year)

prevalence_all_65plus <- prevalence_all %>%
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+") %>% 
  summarise(individuals = sum(individuals), 
            .by = c(year, hscp2019name, age_group, gender)) %>% 
  arrange(hscp2019name, year, age_group)

# population data (2014-2024) ####
pops <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HSCP2019_pop_est_1981_2024.rds") %>% 
  filter(year >= 2014)

# all ages
pops_age_groups_ca <- pops %>% 
  mutate(age_group = create_age_groups(age, from = 0, to = 90, by = 5, as_factor = TRUE),
         age_group = case_when(age_group < "60-64" ~ "0-59",
                               .default = as.character(age_group)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), .by = c(year, hscp2019name, age_group, sex_name))

# 18+
pops_age_groups_ca <- pops %>%
  filter(age >= 18) %>% 
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), .by = c(year, hscp2019name, age_group, sex_name))

pops_age_groups_sc <- pops_age_groups_ca %>% 
  mutate(hscp2019name = "Scotland") %>% 
  summarise(pop = sum(pop), .by = c(year, hscp2019name, age_group, sex_name))

pops_age_groups_all <- pops_age_groups_sc %>%
  bind_rows(pops_age_groups_ca) %>% 
  mutate(sex_name = case_match(sex_name,
                               "M" ~ "Male",
                               .default = "Female"),
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = TRUE)) %>% 
  rename(c(cal_year = year, gender = sex_name))

pop_65plus <- pops_age_groups_all %>% 
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+",
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = TRUE)) %>% 
  summarise(pop = sum(pop), 
            .by = c(cal_year, hscp2019name, age_group, gender))

# Join population data for rates ####
prevalence_all_rates <- prevalence_all %>% 
  select(-lookup) %>% 
  mutate(cal_year = as.numeric(substr(year, 1, 4))) %>%
  relocate(cal_year, .before = hscp2019name) %>% 
  left_join(pops_age_groups_all) %>% 
  mutate(rate_100000 = (individuals/pop)*100000)

prevalence_all_65plus_rates <- prevalence_all_65plus %>%
  mutate(cal_year = as.numeric(substr(year, 1, 4))) %>%
  relocate(cal_year, .before = hscp2019name) %>% 
  left_join(pop_65plus) %>% 
  mutate(rate_100000 = (individuals/pop)*100000)

# Deaths ####
# deaths statistics usually go by date (and year) of registration, will use date of death here though
# as extracts only have data for date of death but can add in date of registration later

# connect to SMRA
cohort_start_date <- dmy(01012017)

# keyring::keyring_unlock(keyring = "DATABASE",
#                         password = source("~/database_keyring.R")[["value"]])

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], # Assumes the user's SMR01 username is the same as their R server username
                            # pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))
                            pwd = .rs.askForPassword("What is your LDAP password?"))


deaths <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
    SELECT AGE, SEX, DATE_OF_DEATH, DATE_OF_REGISTRATION, YEAR_OF_REGISTRATION, UNDERLYING_CAUSE_OF_DEATH, HSCP_2019
    FROM ANALYSIS.GRO_DEATHS_C GRO
    WHERE GRO.DATE_OF_REGISTRATION >= TO_DATE('", cohort_start_date,"', 'yyyy-mm-dd')
    AND country_of_residence ='XS'
    AND age is not NULL")
  )) %>% 
  clean_names()

dbDisconnect(SMRAConnection)

keyring::keyring_lock("DATABASE")

all_deaths_ca <- deaths %>% 
  filter(between(date_of_death, cohort_start_date, dmy(31032024)),
         age >= 65) %>% 
  mutate(hscp2019name = factor(phsmethods::match_area(hscp_2019), 
                               levels = area_order, ordered = TRUE),
         # fy_registration = extract_fin_year(date_of_registration),
         age_group = create_age_groups(age, from = 65, to = 90, by = 5, as_factor = TRUE),
         year = extract_fin_year(date_of_death)) %>%
  # filter(date_of_registration >= cohort_start_date)
  summarise(deaths = n(), .by = c(hscp2019name, year, age_group, sex)) 

all_deaths <- bind_rows(
  all_deaths_ca %>% 
    mutate(hscp2019name = factor("Scotland", levels = area_order, ordered = TRUE)) %>% 
    summarise(deaths = sum(deaths), .by = c(hscp2019name, year, age_group, sex)),
  
  all_deaths_ca
) %>% 
  arrange(hscp2019name, year, age_group, sex)

# spd <- get_spd(col_select = c("pc7", "hscp2019name")) %>% 
#   # transmute(pc7, hscp2019name = match_area(hscp2019), ) %>% 
#   rename(postcode = pc7)

## Died with dementia ####
died_with_dem <- index_first %>% 
  filter(!is.na(date_of_death)) %>% 
  mutate(age_at_death = floor(time_length(interval(date_of_birth, date_of_death), 'years'))) %>% 
  filter(age_at_death >= 65) %>% 
  mutate(year = extract_fin_year(date_of_death),
         age_group = create_age_groups(age_at_death, from = 65, to = 90, by = 5, as_factor = TRUE))


died_with_dem_ca <- died_with_dem %>% 
  summarise(dementia = n(),
            .by = c(hscp2019name, year, age_group, sex))

died_with_dem_all <- bind_rows(
  died_with_dem_ca %>% 
    mutate(hscp2019name = 'Scotland') %>% 
    summarise(dementia = sum(dementia),
              .by = c(hscp2019name, year, age_group, sex)),
  
  died_with_dem_ca %>% filter(!is.na(hscp2019name))
  ) %>% 
  mutate(sex = as.character(sex),
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = TRUE))

deaths_with_data <- all_deaths %>% 
  left_join(died_with_dem_all) %>% 
  mutate(proportion_to_dementia = dementia/deaths) %>% 
  filter(year > "2016/17")

# R markdown objects ####
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

## Prevalence ####

shared_prev_65plus_chart <- SharedData$new(
  prevalence_all_65plus_rates %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 1"
)

shared_prev_65plus_table <- SharedData$new(
  prevalence_all_65plus %>% 
    transmute(year, area = hscp2019name, gender, individuals) %>% 
    pivot_wider(names_from = gender, values_from = individuals) %>% 
    adorn_totals(where = 'col') %>% clean_names() %>% 
    relocate(female, .before = male),
  key = ~area, group = "Group 1"
)

shared_prev_65plus_rate_table <- SharedData$new(
  prevalence_all_65plus_rates %>% 
    transmute(year, area = hscp2019name, gender, rate_100000) %>% 
    mutate(rate = round_half_up(rate_100000, 0)) %>% 
    select(-rate_100000) %>% 
    pivot_wider(names_from = year, values_from = rate),
  key = ~area, group = "Group 1"
)

shared_prev_all_total <- SharedData$new(
  prevalence_all_rates %>% 
    transmute(year, area = hscp2019name, age_group, individuals) %>%
    summarise(individuals = sum(individuals),
              .by = c(year, area, age_group)) %>% 
    pivot_wider(names_from = age_group, values_from = individuals) %>%
    adorn_totals(where = 'col'),
  key = ~area, group = "Group 1"  
)

shared_prev_all_rate <- SharedData$new(
  prevalence_all_rates %>% 
    transmute(year, area = hscp2019name, age_group, pop, individuals) %>%
    summarise(individuals = sum(individuals),
              pop = sum(pop),
              .by = c(year, area, age_group)) %>% 
    mutate(rate = round_half_up((individuals/pop)*100000, 0)) %>% 
    select(-c(individuals, pop)) %>%
    pivot_wider(names_from = age_group, values_from = rate, values_fill = 0),
  key = ~area, group = "Group 1"
)  

shared_prev_source <- SharedData$new(prev_all_source %>% rename(area = hscp2019name),
                                     key = ~area, group = "Group 1")

prev_filter_select <- filter_select("area_select1", "Select an Area",
                                    shared_prev_65plus_chart, group = ~area, multiple = F)

## Deaths ####
# deaths_data

# all by year
# shared_all_deaths_table <- SharedData$new(
#   deaths_of_data %>% 
#     summarise(dementia = sum(dementia_deaths, na.rm = T),
#               all_deaths = sum(deaths, na.rm = T),
#               non_dementia = all_deaths - dementia,
#               proportion_to_dementia = round_half_up((dementia/all_deaths)*100, digits = 0),
#               .by = c(year, hscp2019name)) %>% 
#     # mutate() %>% 
#     rename(area = hscp2019name) %>% 
#     select(year, area, dementia, non_dementia, all_deaths, proportion_to_dementia),
#   key = ~area, group = "Group 2"
# )

shared_all_deaths_with_table <- SharedData$new(
  deaths_with_data %>% 
    summarise(dementia = sum(dementia, na.rm = T),
              all_deaths = sum(deaths, na.rm = T),
              non_dementia = all_deaths - dementia,
              proportion_to_dementia = round_half_up((dementia/all_deaths)*100, digits = 0),
              .by = c(year, hscp2019name)) %>% 
    # mutate() %>% 
    rename(area = hscp2019name) %>% 
    select(year, area, dementia, non_dementia, proportion_to_dementia),
  key = ~area, group = "Group 2"
)


deaths_filter_select1 <- filter_select("area_select2", "Select an Area",
                                      shared_all_deaths_with_table, group = ~area, multiple = F)
# 
# deaths_filter_select2 <- filter_select("area_select3", "Select an Area",
#                                        shared_all_deaths_table, group = ~area, multiple = F)

# shared_deaths_chart_age <- SharedData$new(
#   deaths_of_data %>%
#     summarise(dementia = sum(dementia_deaths, na.rm = T),
#               all_deaths = sum(deaths, na.rm = T),
#               non_dementia = all_deaths - dementia,
#               proportion = round_half_up((dementia/all_deaths)*100, digits = 0),
#               .by = c(year, hscp2019name, age_group)) %>%
#     rename(area = hscp2019name) %>%
#     select(year, area, age_group, dementia, non_dementia, all_deaths, proportion),
#   key = ~area, group = "Group 2"
# )

shared_deaths_with_chart_age <- SharedData$new(
  deaths_with_data %>%
    summarise(dementia = sum(dementia, na.rm = T),
              all_deaths = sum(deaths, na.rm = T),
              non_dementia = all_deaths - dementia,
              proportion = round_half_up((dementia/all_deaths)*100, digits = 0),
              .by = c(year, hscp2019name, age_group)) %>%
    rename(area = hscp2019name) %>%
    mutate(age_group = factor(age_group, levels = rev(age_order), ordered = T)) %>% 
    select(year, area, age_group, dementia, non_dementia, proportion),
  key = ~area, group = "Group 2"
)

# shared_deaths_chart_gender <- SharedData$new(
#   deaths_of_data %>%
#     mutate(gender = case_when(sex == 1 ~ 'Male',
#                               .default = 'Female')) %>% 
#     summarise(dementia = sum(dementia_deaths, na.rm = T),
#               all_deaths = sum(deaths, na.rm = T),
#               non_dementia = all_deaths - dementia,
#               proportion = round_half_up((dementia/all_deaths)*100, digits = 0),
#               .by = c(year, hscp2019name, gender)) %>%
#     rename(area = hscp2019name) %>%
#     select(year, area, gender, dementia, non_dementia, all_deaths, proportion),
#   key = ~area, group = "Group 2"
# )

shared_deaths_with_chart_gender <- SharedData$new(
  deaths_with_data %>%
    mutate(gender = case_when(sex == 1 ~ 'Male',
                              .default = 'Female')) %>% 
    summarise(dementia = sum(dementia, na.rm = T),
              all_deaths = sum(deaths, na.rm = T),
              non_dementia = all_deaths - dementia,
              proportion = round_half_up((dementia/all_deaths)*100, digits = 0),
              .by = c(year, hscp2019name, gender)) %>%
    rename(area = hscp2019name) %>%
    select(year, area, gender, dementia, non_dementia, proportion),
  key = ~area, group = "Group 2"
)

knit_rmd <- function(){
  rmarkdown::render(paste0(here(), "/dashboard/Dementia Prevalence.Rmd"),
                    output_dir = "/PHI_conf/Dementia_Index/outputs/HSCP dashboard/",
                    output_file = paste0("Dementia Prevalence_", format(Sys.Date(), "%Y_%m_%d"), ".html"))
}

source(paste0(here::here(), "/dashboard/gp_prev_comparison.R"))
source(paste0(here::here(), "/dashboard/simd_prev.R"))

knit_rmd()
