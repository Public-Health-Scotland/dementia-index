# Incidence data from Dementia Index
# following same rules as prevalence by filtering out any records that
# don't have a residence in Scotland on their first record

library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(purrr)
library(lubridate)
library(phsmethods)
library(phsstyles)
# remotes::install_github("Public-Health-Scotland/phslookups") 
library(phslookups)
library(ggplot2)
library(scales)
library(plotly)

# Dementia Index - Publication Charts and Tables
# Years 2020/21-2024/25

# import data ####
code_folder <- paste0(here::here(), "/Publications/2026_02 - Identified Dementia Population/")
outputs_folder <- "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/"

# import functions
source(paste0(code_folder, "/functions.R"))

# Index data
index <- readRDS(paste0('/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds')) %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name))

# end dates for required years for age calculations
year_end_date <- dmy(31032021)
year_end_dates <- seq.Date(year_end_date, year_end_date+years(4), by = 'year')

# set age groups and areas as factors
age_order <- c("18-59", create_age_groups(seq(60, 90, by=5), 60, 90, by = 5, as_factor = F))
area_order <- c("Scotland", sort(unique(index$hbres)), sort(unique(index$hscp2019name)))

# population data ####
source(paste0(code_folder, 'populations.R'))

# max epop for all ages - (100000 per sex group) see calculate_easr function/pops_easr for breakdowns
# pops_easr
max_epop <- 200000
# max epop for 18+ - (80700 per sex group)
max_epop_18plus <- 161400

# Incidence ####
# use easr for rates
incidence <- index %>%
  mutate(year = extract_fin_year(diagnosis_date),
         cal_year = as.numeric(substr(year, 1, 4)),
         age_group = create_age_groups(age_at_diagnosis, as_factor = T),
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = T),
         hbres = factor(hbres, levels = area_order, ordered = T)) %>% 
  select(year, cal_year, postcode, hscp2019name, hbres, age_group, sex, simd2020v2_sc_quintile, ur6_2022_name, ur8_2022_name, source) %>%
  # remove those with no Scottish residence data
  filter(!is.na(hscp2019name))

## Scotland ####
# Total, Age, Sex & Deprivation 

### Total #### 
scotland_total <- scot_pops_easr_18plus %>%
  # use population figures for years 2020:2024
  filter(between(cal_year, 2020, 2024)) %>% 
  left_join(incidence %>%
              mutate(area = factor('Scotland', levels = area_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex))) %>%
  relocate(year, .before = area) %>%
  mutate(area_type = 'National', .after = area) %>%
  # set numerator (individuals) and denominator (pop) for use in calculate_easr function
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = sex) %>% 
  select(-c(cal_year, individuals, pop)) %>% 
  # fill in any year that is NA (numerator = 0) in the data
  fill(year, .direction = 'up')

scotland_total_chart <- scotland_total %>%
  # join for use in calculate_easr function
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_total_table <- scotland_total_chart %>%
  select(Year = year, Area = area, Individuals = numerator, `Rate` = rate)

# rates
scotland_table_rate <- scotland_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

# counts
scotland_table_count <- scotland_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)

# Save out data ####

table_data <- list(
  "Incidence Rate - All" = scotland_table_rate,
  "Incidence count - All" = scotland_table_count,
  
  "Incidence Rate - resident" = scotland_table_rate_res,
  "Incidence Count - resident" = scotland_table_count_res
)

writexl::write_xlsx(table_data, paste0(outputs_folder, 'incidence_easr.xlsx'))
