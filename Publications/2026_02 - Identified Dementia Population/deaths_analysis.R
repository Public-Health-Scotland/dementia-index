# Death figures for publication
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(purrr)
library(lubridate)
library(odbc)
library(phsmethods)
library(phslookups)

code_folder <- paste0(here::here(), "/Publications/2026_02 - Identified Dementia Population/")
outputs_folder <- "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/"

# Index data
index <- readRDS(paste0('/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds')) %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name)) %>%
  # flag Scottish residency data
  mutate(scottish_resident = case_when(is.na(hscp2019name) ~ 0,
                                       .default = 1))

index_all <- readRDS(paste0('/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds')) %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name)) %>%
  # flag Scottish residency data
  mutate(scottish_resident = case_when(is.na(hscp2019name) ~ 0,
                                       .default = 1))

# set age groups and areas as factors
age_order <- c("18-59", create_age_groups(seq(60, 90, by=5), 60, 90, by = 5, as_factor = F))
area_order <- c("Scotland", sort(unique(index$hbres)), sort(unique(index$hscp2019name)))

# import functions
source(paste0(code_folder, "/functions.R"))

# population data ####
source(paste0(code_folder, 'populations.R'))

# max epop for all ages - (100000 per sex group) see calculate_easr function/pops_easr for breakdowns
# pops_easr
max_epop <- 200000
# max epop for 18+ - (80700 per sex group)
max_epop_18plus <- 161400

# use only people who have had a Scottish residence
# at any time, previously people who's first diag
# has no Scottish residence data they were removed
# entirely but some may have residency in later
# diagnosis. Alternative is to use only those
# who have residency data on their last diag
prev_residence <- index_all %>%
# prev_residence <- index %>%
  arrange(upi_number, diagnosis_date) %>% 
  filter(scottish_resident == 1) %>% 
  filter(row_number() == first(row_number()),
         .by = upi_number)

# Deaths ####
# deaths statistics usually go by date (and year) of registration, will use date of death here though
# as extracts only have data for date of death but can add in date of registration later

# connect to SMRA
cohort_start_date <- dmy(01012010)

keyring::keyring_unlock(keyring = "DATABASE",
                        password = source("~/database_keyring.R")[["value"]])

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], # Assumes the user's SMR01 username is the same as their R server username
                            pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))
# pwd = .rs.askForPassword("What is your LDAP password?"))

deaths <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
    SELECT UPI_NUMBER, AGE, SEX, DATE_OF_DEATH, DATE_OF_REGISTRATION, YEAR_OF_REGISTRATION, HSCP_2019,
    PLACE_OF_DEATH_POSTCODE, UNDERLYING_CAUSE_OF_DEATH, CAUSE_OF_DEATH_CODE_0 ,CAUSE_OF_DEATH_CODE_1,
    CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3, CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5,
    CAUSE_OF_DEATH_CODE_6, CAUSE_OF_DEATH_CODE_7, CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9,
    HEALTH_BOARD_OF_OCCURENCE, POSTCODE, COUNTRY_OF_RESIDENCE
    FROM ANALYSIS.GRO_DEATHS_C GRO
    WHERE GRO.DATE_OF_REGISTRATION >= TO_DATE('", cohort_start_date,"', 'yyyy-mm-dd')
    
    AND age is not NULL")
  )) %>% 
  clean_names()
# AND country_of_residence ='XS'
dbDisconnect(SMRAConnection)

keyring::keyring_lock("DATABASE")

spd <- get_spd(col_select = c("pc7", "datazone2011", "hscp2019name")) %>%
  rename(postcode = pc7)

deaths %>% left_join(spd) %>% 
  filter(is.na(hscp2019name)) %>% 
  select(place_of_death_postcode, year_of_registration) %>% #View()
  left_join(spd, by = c('place_of_death_postcode' = 'postcode')) %>% 
  filter(is.na(hscp2019name))

# Total number of deaths of people in the Dementia Index (regardless of cause) ####
# i.e. anyone with a date of death within the index
# there will be a lag as the index will have deaths only until the date the index was refreshed
# so need to join onto deaths to get more recently added records
# use index all and their last Scot residency info
# to align with prevalence methodology (check this is ok)
index_deaths <- prev_residence %>% 
  # remove old date_of_death data
  select(-date_of_death) %>%
  left_join(deaths %>%
              select(upi_number, date_of_death, age)) %>%
  filter(!is.na(date_of_death)) %>%
  mutate(year = extract_fin_year(date_of_death),
         cal_year = as.numeric(substr(year, 1, 4)),
         age_group = create_age_groups(age),
         area = 'Scotland') %>% 
  select(upi_number, year, cal_year, upi_number, area, age_group, sex)

# totals 
index_deaths_count <- index_deaths %>% 
  summarise(deaths = n(), .by = year) %>% 
  arrange(year)

# rates
scotland_death_total <- scot_pops_easr_18plus %>%
  # use population figures for years 2020:2024
  filter(between(cal_year, 2020, 2024)) %>% 
  left_join(index_deaths %>%
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

scotland_death_total_chart <- scotland_death_total %>%
  # join for use in calculate_easr function
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_death_total_table <- scotland_death_total_chart %>%
  select(Year = year, Area = area, Individuals = numerator, `Rate` = rate)

# rates
scotland_death_table_rate <- scotland_death_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

# counts
scotland_death_table_count <- scotland_death_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)

# Cause and mentions of dementia on death certificate ####
# cause is underlying only, mention is any cause field
# follows NRS methodology, codes F01, F03 & G30
icd10_dementia_deaths <- c("F01", "F03", "G30")

deaths_flagged <- deaths %>% 
  # filter(age >= 65) %>%
  mutate(flag_mention = case_when(substr(underlying_cause_of_death,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_0,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_1,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_2,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_3,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_4,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_5,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_6,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_7,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_8,1,3) %in% icd10_dementia_deaths ~1,
                                   substr(cause_of_death_code_9,1,3) %in% icd10_dementia_deaths ~1,
                                   T~0),
         flag_cause = case_when(substr(underlying_cause_of_death,1,3) %in% icd10_dementia_deaths ~1,
                                T~0),
         fin_year = extract_fin_year(date_of_registration),
         hscp2019name = match_area(hscp_2019)) 

## check against NRS figures for yearly totals ####
# https://www.nrscotland.gov.uk/publications/alzheimers-disease-and-other-dementia-deaths-in-scotland-2024/#
# latest year (2024) NRS - 10,618, us 10,607, difference of -11 people
# update: NRS use all deaths not just those to Scottish residents
# latest year (2024) NRS - 10,618, us 10,625, difference of +7 people
nrs_metioned <- deaths_flagged %>% 
  filter(flag_mention == 1) %>% 
  summarise(nrs_deaths = n(), .by = year_of_registration)

# latest year NRS - 6,612, us 6,601, difference of -11 people
# update: NRS use all deaths not just those to Scottish residents
# numbers match with NRS
nrs_caused <- deaths_flagged %>% 
  filter(flag_cause == 1) %>% 
  summarise(nrs_deaths = n(), .by = year_of_registration)

## index mentions ####
# does not add up probably because there are some removed from the index
# 3 less than index
prev_residence %>% 
  left_join(deaths_flagged %>% 
              filter(flag_mention == 1) %>% 
              select(upi_number, flag_mention, year_of_registration)) %>% 
  filter(flag_mention == 1) %>% 
  # tabyl(year)
  tabyl(year_of_registration)

# index is missing just 1 person for latest year
index %>%
  left_join(deaths_flagged %>% 
              filter(flag_mention == 1) %>% 
              select(upi_number, flag_mention, year_of_registration)) %>% 
  filter(flag_mention == 1) %>% 
  # tabyl(year)
  tabyl(year_of_registration)

## index caused ####
# numbers are close to NRS some difference in some years, latest year matches
index %>%
  left_join(deaths_flagged %>% 
              filter(flag_cause == 1) %>% 
              select(upi_number, flag_cause, year_of_registration)) %>% 
  filter(flag_cause == 1,
         year_of_registration == 2024) %>% 
  filter(is.na(ca2019))
  # tabyl(year)
  tabyl(year_of_registration)

# will be missing non scottish residents
index_caused <- prev_residence %>% 
  select(-date_of_death) %>% 
  left_join(deaths_flagged %>% 
              filter(flag_cause == 1) %>% 
              select(upi_number, age, flag_cause, date_of_death, date_of_registration, year_of_registration)) %>% 
  filter(flag_cause == 1) %>%
  # tabyl(year_of_registration)
  mutate(year = extract_fin_year(date_of_death),
         cal_year = as.numeric(substr(year, 1, 4)),
         age_group = create_age_groups(age),
         area = 'Scotland') %>% 
  select(upi_number, year, cal_year, upi_number, area, age_group, sex)

# missing records (2) (latest cal year)
deaths_flagged %>% 
  filter(flag_cause == 1,
         year_of_registration == 2024) %>% 
  filter(!upi_number %in% index_caused$upi_number)

## Caused EASR ####
scotland_death_caused_total <- scot_pops_easr_18plus %>%
  # use population figures for years 2020:2024
  filter(between(cal_year, 2020, 2024)) %>% 
  left_join(index_caused %>%
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

scotland_death_caused_total_chart <- scotland_death_caused_total %>%
  # join for use in calculate_easr function
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_death_caused_total_table <- scotland_death_caused_total_chart %>%
  select(Year = year, Area = area, Individuals = numerator, `Rate` = rate)

# rates
scotland_death_caused_table_rate <- scotland_death_caused_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

# counts
scotland_death_caused_table_count <- scotland_death_caused_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)

# Save out data
table_data_deaths <- list(
  "Scotland total index - Rate" = scotland_death_table_rate,
  "Scotland total index - Count" = scotland_death_table_count,
  "Scotland caused index - Rate" = scotland_death_caused_table_rate,
  "Scotland caused index - Count" = scotland_death_caused_table_count
)

writexl::write_xlsx(table_data_deaths, paste0(outputs_folder, 'deaths_analysis_figures.xlsx'))

table_data_deaths_check <- list(
  "NRS Caused Figures" = nrs_caused,
  "NRS Mentioned Figures" = nrs_metioned
)

writexl::write_xlsx(table_data_deaths_check, paste0(outputs_folder, 'NRS_deaths_figures.xlsx'))
