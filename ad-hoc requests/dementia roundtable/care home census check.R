
library(tidyverse)
library(readxl)
library(janitor)
library(phsmethods)
library(skimr)
library(hablar)

chc1415 <- readRDS("/PHI_conf/Dementia_Index/data/ad-hoc/care home census check/chc_dementia_anytime_201415.rds") %>%
  clean_names() %>%
  mutate(upi_number = as.character(upi_number)) %>%
  mutate(upi_number = chi_pad(upi_number)) %>%
  mutate(census = "2014/15") %>%
  rename(age_at_census = age_at31march2015) %>%
  mutate(date_of_birth = as_date(date_of_birth))

chc1516 <- readRDS("/PHI_conf/Dementia_Index/data/ad-hoc/care home census check/chc_dementia_anytime_201516.rds") %>%
  clean_names() %>%
  mutate(upi_number = as.character(upi_number)) %>%
  mutate(upi_number = chi_pad(upi_number)) %>%
  mutate(census = "2015/16") %>%
  rename(age_at_census = age_at31march2016) %>%
  mutate(date_of_birth = as_date(date_of_birth))

chc1617 <- read.csv("/PHI_conf/Dementia_Index/data/ad-hoc/care home census check/chc_dementia_anytime_201617.csv") %>%
  clean_names() %>%
  rename(upi_number = chi_seed) %>%
  mutate(upi_number = as.character(upi_number)) %>%
  mutate(upi_number = chi_pad(upi_number)) %>%
  mutate(census = "2016/17") %>%
  select(-end_date) %>%
  rename(age_at_census = age_at31mar2017) %>%
  mutate(date_of_birth = as_date(date_of_birth))

chc_old <- bind_rows(chc1415, chc1516, chc1617)

chc_old %>% 
  group_by(census) %>% 
  summarise(
    rows          = n(),
    unique_people = n_distinct(upi_number),
    .groups = "drop"
  )








chc_dementia_index <- read_excel("/PHI_conf/Dementia_Index/data/extracts/dementia_CareHomeCensus_2017_18_to_2023_24.xlsx") %>%
  clean_names() %>%
  select(upi_number, everything()) %>%
  arrange(upi_number) %>%
  mutate(upi_number = phsmethods::chi_pad(upi_number)) %>% 
  mutate(valid_chi = phsmethods::chi_check(upi_number)) %>% 
  group_by(upi_number) %>%
  mutate(duplicate_upi = n() > 1) %>%
  ungroup() %>%
  # ── add a TRUE/FALSE column for every financial year ───────────
  mutate(
    present_1718 =  date_of_admission <= ymd("2018-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2017-04-01")),
    present_1819 =  date_of_admission <= ymd("2019-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2018-04-01")),
    present_1920 =  date_of_admission <= ymd("2020-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2019-04-01")),
    present_2021 =  date_of_admission <= ymd("2021-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2020-04-01")),
    present_2122 =  date_of_admission <= ymd("2022-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2021-04-01")),
    present_2223 =  date_of_admission <= ymd("2023-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2022-04-01")),
    present_2324 =  date_of_admission <= ymd("2024-03-31") & (is.na(date_of_discharge) | date_of_discharge >= ymd("2023-04-01"))
  )





# ── headline counts (also written out long so you can inspect) ──
census_counts <- chc_simple %>% summarise(
  residents_1718 = n_distinct(upi_number[present_1718]),
  residents_1819 = n_distinct(upi_number[present_1819]),
  residents_1920 = n_distinct(upi_number[present_1920]),
  residents_2021 = n_distinct(upi_number[present_2021]),
  residents_2122 = n_distinct(upi_number[present_2122]),
  residents_2223 = n_distinct(upi_number[present_2223]),
  residents_2324 = n_distinct(upi_number[present_2324])
)





duplicate_chi <- chc_2324 %>%
  group_by(upi_number) %>%
  filter(n() > 1) %>%
  ungroup()









