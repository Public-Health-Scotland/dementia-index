# Dementia Project Analysis Script ====
# Purpose: To answer key questions about the dementia population for a roundtable.
# Financial Year of Analysis: 2023/24


# Load necessary R packages =====
library(tidyverse) # For data manipulation (dplyr, ggplot2, etc.)
library(lubridate) # For easy handling of dates and times
library(janitor)   # For cleaning data frames and column names
library(askpass)   # For securely prompting for passwords
library(odbc)      # A specific driver for connecting to databases via ODBC
library(writexl)   # For writing data frames to Excel files
library(glue)      # For safely building query strings
library(hablar)


# Setup and Parameters =====

# Define all key parameters for the analysis in one place.
analysis_fy_start <- ymd("2023-04-01")
analysis_fy_end   <- ymd("2024-03-31")
age_threshold     <- 65

# Create the financial year interval from the parameters
fy_interval <- interval(analysis_fy_start, analysis_fy_end)


# Load and Prepare Base Data =====

# Load the SLF data and create a flag for those with a care home spell in the FY
slf <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/SLF2324.rds") %>%
  mutate(care_spell = interval(ch_admission, coalesce(ch_discharge, today()))) %>%
  filter(int_overlaps(care_spell, fy_interval)) %>% # Find the cases with a stay during FY 23/24
  select(chi) %>%
  rename(upi_number = chi) %>%
  group_by(upi_number) %>%
  distinct() %>%
  ungroup() %>%
  mutate(slf_2324_carehome_spell = TRUE)

# Load the main Dementia Index and join the SLF flag.
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds") %>%
  left_join(slf, by = "upi_number")


# Question 1: Survivors by Care Home vs. Community ====
# 1. Number of people on the index (survivors) split between care home residents and 
#    those in the community. This analysis is for people who survived the entire financial year.


# Create the Q1 Cohort with Flags =====

# --- Step 1: Find the first diagnosis date for each patient ---
first_diagnosis_dates <- dementia_index %>%
  filter(!is.na(diagnosis_date)) %>%
  group_by(upi_number) %>%
  summarise(first_diagnosis_date = min(diagnosis_date, na.rm = TRUE))


# --- Step 2: Create a unique list of all patients and join their first diagnosis date ---
cohort_q1 <- dementia_index %>%
  select(upi_number, date_of_birth, date_of_death) %>%
  filter(!is.na(date_of_birth)) %>%
  distinct(upi_number, .keep_all = TRUE) %>%
  left_join(first_diagnosis_dates, by = "upi_number")


# --- Step 3: Add TRUE/FALSE flags for each condition ---
cohort_q1 <- cohort_q1 %>%
  mutate(
    diagnosed_before_end_of_fy = !is.na(first_diagnosis_date) & first_diagnosis_date <= analysis_fy_end,
    is_65_plus = trunc(interval(date_of_birth, analysis_fy_start) / years(1)) >= age_threshold,
    survived_fy = is.na(date_of_death) | date_of_death > analysis_fy_end
  )

# --- Step 4: Flag patients who had a care home spell during the year ---
# This section identifies anyone with a care home record from CHC, Social Care, or the SLF flag.
upis_with_care_home_spell_in_fy <- dementia_index %>%
  # Filter for rows from any of the three care home sources
  filter(source == "Care home census" | type_of_care == "care home" | slf_2324_carehome_spell == TRUE) %>%
  # Standardize the different start and end date columns from CHC and Social Care
  mutate(
    start = if_else(source == "Care home census", diagnosis_date, start_date_of_care),
    end   = if_else(source == "Care home census", CHC_last_discharge, end_date_of_care)
  ) %>%
  # Note: This logic correctly identifies spells for CHC/Social Care. SLF is included by the initial filter.
  # A more robust version might create a unified start/end date for all three sources.
  filter(!is.na(start)) %>%
  mutate(care_spell = interval(start, coalesce(end, today()))) %>%
  filter(int_overlaps(care_spell, fy_interval)) %>%
  pull(upi_number) %>%
  unique()

# Add the final flag to our cohort dataframe.
cohort_q1 <- cohort_q1 %>%
  mutate(
    had_care_home_spell = upi_number %in% upis_with_care_home_spell_in_fy 
  )


# Generate Final Answer for Q1 =====

q1_summary <- cohort_q1 %>%
  filter(
    diagnosed_before_end_of_fy == TRUE,
    is_65_plus == TRUE,
    survived_fy == TRUE
  ) %>%
  count(had_care_home_spell, name = "number_of_patients") %>%
  mutate(
    population_group = if_else(
      had_care_home_spell,
      "Dementia Survivors with Care Home Spell (65+)",
      "Dementia Survivors in Community (65+)"
    )
  ) %>%
  select(population_group, number_of_patients)

# Print the final result for Question 1.
print("--- Answer for Question 1 ---")
print(q1_summary)


# Question 1.1: Of those in the community, how many had Home Care hours >0? ====

# --- Step 1: Load and process the SLF Home Care data ---
# This creates a table with the total home care hours for each person during the FY.
slf_homecare <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/SLF2324HC.rds") %>%
  rename(upi_number = chi) %>%
  mutate(home_care_spell = interval(record_keydate1, record_keydate2)) %>%
  # Filter for spells that OVERLAP with the financial year.
  filter(int_overlaps(home_care_spell, fy_interval)) %>%
  # Calculate the total annual hours for each person from all of their overlapping spells.
  group_by(upi_number) %>%
  summarise(total_hc_hours = sum(hc_hours_annual, na.rm = TRUE)) %>%
  ungroup()

# --- Step 2: Identify the specific "Community Survivors" cohort from Q1 ---
# This gives us the exact list of people we need to check for home care.
community_survivors <- cohort_q1 %>%
  filter(
    diagnosed_before_end_of_fy == TRUE,
    is_65_plus == TRUE,
    survived_fy == TRUE,
    had_care_home_spell == FALSE # The key filter for "in the community"
  ) %>%
  select(upi_number)

# --- Step 3: Join home care hours to the community cohort and add a flag ---
community_homecare_analysis <- community_survivors %>%
  # Join the home care hours data. People with no hours will get NA.
  left_join(slf_homecare, by = "upi_number") %>%
  # Create a boolean flag to identify who had home care hours > 0.
  mutate(
    had_home_care = !is.na(total_hc_hours)
  )

# --- Generate Final Answer for Q1.1 ---
q1_1_summary <- community_homecare_analysis %>%
  # Count the number of people based on the home care flag
  count(had_home_care, name = "number_of_patients") %>%
  mutate(
    home_care_status = if_else(
      had_home_care,
      "Community Survivors with Home Care",
      "Community Survivors without Home Care"
    )
  ) %>%
  select(home_care_status, number_of_patients)

# Print the final result for Question 1.1
print("--- Answer for Question 1.1 (Home Care Breakdown) ---")
print(q1_1_summary)





# Question 2: Deaths by Dementia vs. General Population ====
# 2. Number of people 65+ from the index that died in the year and the number that died 
#    in the rest of the 65+ pop.


# Load Deaths Data =====

# Load the pre-queried national deaths data.
deaths <- read_rds("/PHI_conf/Dementia_Index/data/extracts/data for adhoc requests/deaths_from_2023-01-01.rds") %>%
  # Some records might have duplicates; this code ensures we only keep the earliest
  # recorded death for each person, giving us a clean, unique list of decedents.
  arrange(date_of_death) %>%
  group_by(upi_number) %>%
  slice(1) %>%
  ungroup()


# Create the Q2 Cohort with Flags =====

# --- Step 1: Create a reference list of all patients ever on the dementia index ---
# This will be used for the 'on_dementia_index' flag.
dementia_index_upis <- dementia_index %>%
  select(upi_number) %>%
  distinct()

# --- Step 2: Add TRUE/FALSE flags to the entire deaths file ---
deaths_q2_flags <- deaths %>%
  mutate(
    # Condition A: Did the person die within the financial year?
    died_in_fy = date_of_death %within% fy_interval,
    
    # Condition B: Was the person 65+ at their time of death?
    # We calculate age_at_death first, handling cases with no DOB.
    age_at_death = if_else(!is.na(date_of_birth), 
                           trunc(interval(date_of_birth, date_of_death) / years(1)), 
                           NA_real_),
    is_65_plus_at_death = !is.na(age_at_death) & age_at_death >= age_threshold,
    
    # Condition C: Was the person on the dementia index?
    on_dementia_index = upi_number %in% dementia_index_upis$upi_number
  )


# Generate Final Answer for Q2 =====

# Now we filter the flagged deaths data for our specific population
# and count the results.
q2_summary <- deaths_q2_flags %>%
  # Filter for deaths of people 65+ that occurred in the financial year
  filter(
    died_in_fy == TRUE &
    is_65_plus_at_death == TRUE
  ) %>%
  # Count how many of this group were on the dementia index vs. not
  count(on_dementia_index, name = "number_of_deaths") %>%
  # Make the labels more descriptive for the final table
  mutate(
    population_group = if_else(
      on_dementia_index,
      "Dementia Index Deaths (65+)",
      "Other General Population Deaths (65+)"
    )
  ) %>%
  select(population_group, number_of_deaths)

# Print the final result for Question 2.
print("--- Answer for Question 2 ---")
print(q2_summary)




# Question 3: Unplanned Admissions and Bed Days ====
# 3. For both 1) and 2) the number of unplanned admissions and bed days for the index 
#    survivors and decedents and for the rest of the 65+ pop.

# Setup for Q3: Define Final Analysis Cohorts =====

# To ensure maximum clarity, we redefine each of the analysis cohorts
# from the original source data, making the Q3 analysis self-contained.

# --- Cohort A: Dementia Index Survivors (65+) ---
dementia_survivors_q3 <- dementia_index %>%
  select(upi_number, date_of_birth, date_of_death) %>%
  distinct(upi_number, .keep_all = TRUE) %>%
  left_join(first_diagnosis_dates, by = "upi_number") %>%
  filter(
    !is.na(first_diagnosis_date) & first_diagnosis_date <= analysis_fy_end,
    trunc(interval(date_of_birth, analysis_fy_start) / years(1)) >= age_threshold,
    is.na(date_of_death) | date_of_death > analysis_fy_end
  )

# --- Cohort B: Dementia Index Decedents (65+) ---
dementia_decedents_q3 <- deaths %>%
  filter(
    date_of_death %within% fy_interval,
    trunc(interval(date_of_birth, date_of_death) / years(1)) >= age_threshold
  ) %>%
  filter(upi_number %in% dementia_index_upis$upi_number)

# --- Cohort C: Other General Population Decedents (65+) ---
other_decedents_q3 <- deaths %>%
  filter(
    date_of_death %within% fy_interval,
    trunc(interval(date_of_birth, date_of_death) / years(1)) >= age_threshold
  ) %>%
  filter(!upi_number %in% dementia_index_upis$upi_number)

# --- Cohort D: Other General Population Survivors (65+) ---
# This fourth group is defined implicitly within the SMR data. Any unplanned
# admission for a person 65+ not in the other three groups belongs to this cohort.


# Setup for Q3: Process SMR Data =====

process_smr <- function(path, cis_var, fy_start = analysis_fy_start, fy_end = analysis_fy_end) {
  read_rds(path) %>%
    mutate(
      age = trunc(interval(dob, fy_start) / years(1)),
      admission_type_description = case_when(
        admission_type %in% c(10,11,12,18,19) ~ "Routine Admission",
        admission_type %in% c(20,21,22,30:36,38,39) ~ "Urgent or Emergency Admission",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(upi_number, {{ cis_var }}) %>%
    summarise(
      stay_admission_date = min(admission_date,   na.rm = TRUE),
      stay_discharge_date = max(discharge_date,   na.rm = TRUE),
      stay_admission_type = first(admission_type_description),
      age                 = first(age),
      .groups = "drop"
    ) %>%
    # NEW LOGIC: Calculate bed days that fall ONLY within the financial year.
    mutate(
      # First, determine the correct end date for the calculation.
      # If the stay ends after the financial year, or is ongoing (NA),
      # we must cap the calculation at the financial year end date.
      effective_discharge_date = if_else(
        is.na(stay_discharge_date) | stay_discharge_date > fy_end,
        fy_end,
        stay_discharge_date
      ),
      
      # Now, calculate bed days using the original admission date and the new effective discharge date.
      # This correctly handles stays that cross the year-end boundary.
      bed_days = as.numeric(difftime(effective_discharge_date, stay_admission_date, units = "days"))
    )
}

# Process and combine all SMR data extracts
data_smr01  <- process_smr("/PHI_conf/Dementia_Index/data/extracts/data for adhoc requests/smr01_from_2023-01-01.rds", cis_marker)
data_smr01e <- process_smr("/PHI_conf/Dementia_Index/data/extracts/data for adhoc requests/smr01-1e_from_2023-01-01.rds", gls_cis_marker) %>% rename(cis_marker = gls_cis_marker)
data_smr04  <- process_smr("/PHI_conf/Dementia_Index/data/extracts/data for adhoc requests/smr04_from_2023-01-01.rds", cis_marker)

all_smr <- bind_rows(data_smr01, data_smr01e, data_smr04)


# Analysis for Q3 =====

# --- Step 1: Calculate Metrics for Stays STARTING in FY ---
# This summary will provide the count of new admissions and their associated bed days.
summary_stays_starting_in_fy <- all_smr %>%
  filter(
    age >= age_threshold,
    stay_admission_date %within% fy_interval,
    stay_admission_type == "Urgent or Emergency Admission"
  ) %>%
  # Calculate bed days for these specific stays, capped at the FY boundary
  mutate(
    effective_discharge_date = if_else(
      is.na(stay_discharge_date) | stay_discharge_date > analysis_fy_end,
      analysis_fy_end,
      stay_discharge_date
    ),
    bed_days = as.numeric(difftime(effective_discharge_date, stay_admission_date, units = "days"))
  ) %>%
  # Tag these stays with their population group
  mutate(
    population_group = case_when(
      upi_number %in% dementia_decedents_q3$upi_number ~ "Dementia Index Decedents (65+)",
      upi_number %in% dementia_survivors_q3$upi_number ~ "Dementia Index Survivors (65+)",
      upi_number %in% other_decedents_q3$upi_number    ~ "Other General Population Decedents (65+)",
      TRUE                                             ~ "Other General Population Survivors (65+)"
    )
  ) %>%
  group_by(population_group) %>%
  summarise(
    unplanned_admissions_starting_in_fy = n(),
    bed_days_from_stays_starting_in_fy = sum(bed_days, na.rm = TRUE)
  )


# --- Step 2: Calculate Total In-Year Bed Days from ALL OVERLAPPING Stays ---
# This summary will provide the total bed day usage, including from long stays
# that began before the financial year.
summary_overlapping_bed_days <- all_smr %>%
  filter(
    age >= age_threshold,
    stay_admission_date <= analysis_fy_end,
    is.na(stay_discharge_date) | stay_discharge_date >= analysis_fy_start,
    stay_admission_type == "Urgent or Emergency Admission"
  ) %>%
  # Calculate only the portion of bed days that falls within the FY
  mutate(
    effective_start_date = pmax(stay_admission_date, analysis_fy_start),
    effective_end_date = pmin(coalesce(stay_discharge_date, analysis_fy_end), analysis_fy_end),
    in_year_bed_days = as.numeric(difftime(effective_end_date, effective_start_date, units = "days"))
  ) %>%
  # Tag these stays with their population group
  mutate(
    population_group = case_when(
      upi_number %in% dementia_decedents_q3$upi_number ~ "Dementia Index Decedents (65+)",
      upi_number %in% dementia_survivors_q3$upi_number ~ "Dementia Index Survivors (65+)",
      upi_number %in% other_decedents_q3$upi_number    ~ "Other General Population Decedents (65+)",
      TRUE                                             ~ "Other General Population Survivors (65+)"
    )
  ) %>%
  group_by(population_group) %>%
  summarise(total_in_year_bed_days = sum(in_year_bed_days, na.rm = TRUE))


# --- Step 3: Combine metrics into a final summary table ---
q3_summary <- full_join(summary_stays_starting_in_fy, summary_overlapping_bed_days, by = "population_group") %>%
  # Replace any NAs with 0
  replace_na(list(
    unplanned_admissions_starting_in_fy = 0,
    bed_days_from_stays_starting_in_fy = 0,
    total_in_year_bed_days = 0
  ))

# Print the final result for Question 3
print("--- Answer for Question 3 ---")
print(q3_summary)


# Combine all summary tables into a single named list for output.
output_data <- list(
  "Q1 Survivors Breakdown" = q1_summary,
  "Q2 Homecare"            = q1_1_summary,
  "Q2 Deaths Breakdown"    = q2_summary,
  "Q3 Hospital Activity"   = q3_summary
)

# Define the name for the output file.
output_filename <- "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/Dementia_Carehome_Analysis.xlsx"

# Write the list of data frames to the specified Excel file.
write_xlsx(output_data, path = output_filename)


