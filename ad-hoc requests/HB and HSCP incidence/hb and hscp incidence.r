# =============================================================================
# Dementia Incidence Analysis
# Description: Calculates annual incidence of new dementia cases and rates per
#              100,000 population by financial year (2020/21 - 2024/25).
#              Outputs produced for National, Health Board, and HSCP levels.
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(phsmethods)
library(writexl)

# Directories
input_path    <- "/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds"
pop_hscp_path <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/HSCP2019_pop_est_1981_2024.rds"
pop_hb_path   <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2024.rds"
output_dir    <- "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/Publication - extra analysis/HB and HSCP incidence"

# Ensure output directory exists (creates it if it doesn't)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -------------------------------------------------------------------------
# 1. Load and Clean Dementia Index
# -------------------------------------------------------------------------
dementia_index <- readRDS(input_path) %>%
  mutate(
    # Handle missing Health Board values (including Rest of UK / Outside UK)
    hbres = if_else(is.na(hbres), "Unknown / Rest of UK / Outside UK", hbres),
    
    # Create the HSCP field based on Local Authority (ca2019name)
    # Handle Clackmannanshire/Stirling merge, naming mismatches, and NAs
    HSCP = case_when(
      is.na(ca2019name) ~ "Unknown / Rest of UK / Outside UK",
      ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
      ca2019name == "City of Edinburgh" ~ "Edinburgh",
      ca2019name == "Na h-Eileanan Siar" ~ "Western Isles",
      TRUE ~ ca2019name
    )
  )

# Identify the earliest diagnosis date per person
dementia_initial_diagnoses <- dementia_index %>%
  group_by(upi_number) %>%
  arrange(diagnosis_date, .by_group = TRUE) %>%
  mutate(
    event_number = row_number(),
    # Flag the first chronological record
    initial_diagnosis_event = event_number == 1
  ) %>%
  ungroup() %>%
  filter(initial_diagnosis_event)

# Create base dataset of new cases in the requested financial years
base_new_cases <- dementia_initial_diagnoses %>%
  filter(!is.na(diagnosis_date)) %>%
  mutate(financial_year = extract_fin_year(diagnosis_date)) %>%
  filter(
    financial_year %in% c(
      "2020/21", "2021/22", "2022/23", "2023/24", "2024/25"
    )
  )

# Aggregate counts at all three geographic levels
counts_national <- base_new_cases %>%
  count(financial_year, name = "n_new_cases") %>%
  arrange(financial_year)

counts_hb <- base_new_cases %>%
  count(financial_year, hbres, name = "n_new_cases") %>%
  arrange(financial_year, hbres)

counts_hscp <- base_new_cases %>%
  count(financial_year, HSCP, name = "n_new_cases") %>%
  arrange(financial_year, HSCP)


# -------------------------------------------------------------------------
# 2. Load and Aggregate Population Data
# -------------------------------------------------------------------------
# Load HSCP population estimates (used for HSCP and National totals)
pops_hscp_raw <- readRDS(pop_hscp_path) %>% 
  filter(year >= 2019) %>% 
  rename(cal_year = year) %>%
  mutate(financial_year = paste0(cal_year, "/", substr(cal_year + 1, 3, 4)))

# Load Health Board population estimates
pops_hb_raw <- readRDS(pop_hb_path) %>% 
  filter(year >= 2019) %>% 
  rename(cal_year = year) %>%
  mutate(financial_year = paste0(cal_year, "/", substr(cal_year + 1, 3, 4)))

# Aggregate population to National level (using HSCP file)
pop_national <- pops_hscp_raw %>%
  group_by(financial_year) %>%
  summarise(total_pop = sum(pop), .groups = "drop")

# Aggregate population to Health Board level (using HB file)
pop_hb <- pops_hb_raw %>%
  group_by(financial_year, hb2019name) %>%
  summarise(total_pop = sum(pop), .groups = "drop") %>%
  rename(hbres = hb2019name) # Rename to match dementia index for joining

# Aggregate population to HSCP level (using HSCP file)
pop_hscp <- pops_hscp_raw %>%
  group_by(financial_year, hscp2019name) %>%
  summarise(total_pop = sum(pop), .groups = "drop") %>%
  rename(HSCP = hscp2019name) # Rename to match dementia index for joining


# -------------------------------------------------------------------------
# 3. Calculate Incidence Rates
# -------------------------------------------------------------------------
inc_national <- counts_national %>%
  left_join(pop_national, by = "financial_year") %>%
  mutate(rate_per_100k = (n_new_cases / total_pop) * 100000)

inc_hb <- counts_hb %>%
  left_join(pop_hb, by = c("financial_year", "hbres")) %>%
  mutate(rate_per_100k = (n_new_cases / total_pop) * 100000)

inc_hscp <- counts_hscp %>%
  left_join(pop_hscp, by = c("financial_year", "HSCP")) %>%
  mutate(rate_per_100k = (n_new_cases / total_pop) * 100000)


# -------------------------------------------------------------------------
# 4. Save Outputs
# -------------------------------------------------------------------------
# Save Data Tables as XLSX with multiple sheets
write_xlsx(
  list(
    "National" = inc_national,
    "Health Board" = inc_hb,
    "HSCP" = inc_hscp
  ),
  path = file.path(output_dir, "annual_incidence_rates.xlsx")
)

message("Incidence analysis completed. Excel output saved to: ", output_dir)