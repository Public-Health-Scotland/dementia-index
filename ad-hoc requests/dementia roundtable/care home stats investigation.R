# Detailed Care Home Breakdown Script ====
# Purpose: To provide a multi-level breakdown of patients in the dementia index
# with care home records, by data source.


# Setup =====

# Load the main Dementia Index dataset from a saved .rds file.
# This index contains records for all patients identified with dementia.
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# Parameters from your main script (for reference)
analysis_fy_start <- ymd("2023-04-01")
analysis_fy_end   <- ymd("2024-03-31")
age_threshold     <- 65
fy_interval <- interval(analysis_fy_start, analysis_fy_end)

test <- dementia_index %>%
  filter(source == "Care home census")


# 1. Total Unique Patients Ever in Care Homes (All Time) =====

# Count unique patients with any record from the Care Home Census
total_chc_ever <- dementia_index %>%
  filter(source == "Care home census") %>%
  summarise(n = n_distinct(upi_number)) %>%
  pull(n)

# Count unique patients with any 'care home' record from Social Care
total_sc_ever <- dementia_index %>%
  filter(type_of_care == "care home") %>%
  summarise(n = n_distinct(upi_number)) %>%
  pull(n)

# Create a summary table for this level
summary_all_time <- tibble(
  breakdown_level = "1. All Time (Ever on Index)",
  source_dataset = c("Care Home Census", "Social Care"),
  unique_patients = c(total_chc_ever, total_sc_ever)
)

print(summary_all_time)


# 2. Total Unique Patients with a Care Home Spell in FY 2023/24 =====

# Get UPIs from CHC with a spell overlapping the financial year
chc_in_fy <- dementia_index %>%
  filter(source == "Care home census") %>%
  mutate(care_spell = interval(diagnosis_date, coalesce(CHC_last_discharge, today()))) %>%
  filter(int_overlaps(care_spell, fy_interval))

# Get UPIs from Social Care with a spell overlapping the financial year
sc_in_fy <- dementia_index %>%
  filter(type_of_care == "care home") %>%
  mutate(care_spell = interval(start_date_of_care, coalesce(end_date_of_care, today()))) %>%
  filter(int_overlaps(care_spell, fy_interval))

# Create a summary table for this level
summary_in_fy <- tibble(
  breakdown_level = "2. Active in Financial Year",
  source_dataset = c("Care Home Census", "Social Care"),
  unique_patients = c(n_distinct(chc_in_fy$upi_number), n_distinct(sc_in_fy$upi_number))
)

print(summary_in_fy)


# 3. Total Unique Patients Aged 65+ with a Spell in FY 2023/24 =====

# We need age information for this step. Let's create a helper table with
# a unique list of patients and their age flags from your main script.
patient_age_flags <- dementia_index %>%
  select(upi_number, date_of_birth) %>%
  distinct(upi_number, .keep_all = TRUE) %>%
  mutate(is_65_plus = trunc(interval(date_of_birth, analysis_fy_start) / years(1)) >= age_threshold)

# Filter the CHC group for those aged 65+
chc_65plus_in_fy <- chc_in_fy %>%
  inner_join(patient_age_flags, by = "upi_number") %>%
  filter(is_65_plus == TRUE)

# Filter the Social Care group for those aged 65+
sc_65plus_in_fy <- sc_in_fy %>%
  inner_join(patient_age_flags, by = "upi_number") %>%
  filter(is_65_plus == TRUE)

# Create a summary table for this final level
summary_65plus_in_fy <- tibble(
  breakdown_level = "3. Active in FY & Aged 65+",
  source_dataset = c("Care Home Census", "Social Care"),
  unique_patients = c(n_distinct(chc_65plus_in_fy$upi_number), n_distinct(sc_65plus_in_fy$upi_number))
)


# 4. Final Combined Summary Table =====

# Combine all three levels of breakdown into a single, clear table.
final_care_home_breakdown <- bind_rows(
  summary_all_time,
  summary_in_fy,
  summary_65plus_in_fy
)

# Print the final detailed breakdown
print("--- Detailed Breakdown of Care Home Patients ---")
print(final_care_home_breakdown)