# --- 1. SETUP: Load Libraries and Initial Data ---
library(tidyverse)
library(lubridate)
library(writexl)

# Load the original data from the file
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# --- 2. DATA PATCH: Add Missing 'NRS Deaths' Rows ---
# This section creates a corrected version of the dataframe.

# Create NRS deaths rows for everyone who has died
all_potential_nrs_rows <- dementia_index %>%
  filter(!is.na(date_of_death)) %>%
  group_by(upi_number) %>%
  summarise(death_date = first(date_of_death), .groups = "drop") %>%
  mutate(
    source = "NRS deaths",
    diagnosis_date = death_date
  ) %>%
  select(upi_number, source, diagnosis_date)

# Find which people ALREADY have a proper NRS deaths row
upis_already_correct <- dementia_index %>%
  filter(source == "NRS deaths") %>%
  distinct(upi_number) %>%
  pull(upi_number)

# Isolate only the new rows that are actually missing
new_nrs_rows_to_add <- all_potential_nrs_rows %>%
  filter(!(upi_number %in% upis_already_correct))

# Combine the original data with the new rows to create a final, corrected dataset
dementia_index_fixed <- bind_rows(dementia_index, new_nrs_rows_to_add)

cat("Data patching complete.", nrow(new_nrs_rows_to_add), "new 'NRS deaths' rows added.\n")


# --- 3. OVERALL ANALYSIS (Using Corrected Data) ---

# Create a summary table with source combinations for each UPI
upi_source_summary <- dementia_index_fixed %>%
  group_by(upi_number) %>%
  summarise(
    source_combination = paste(sort(unique(source)), collapse = ", "),
    .groups = 'drop'
  )

# Count people, sort, and add a percentage column
source_combination_counts <- upi_source_summary %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    percentage = (number_of_people / sum(number_of_people)) * 100
  )

print(source_combination_counts)


# --- 4. BY YEAR ANALYSIS (Using Corrected Data) ---

# Find the first record year for each person
upi_cohort_summary <- dementia_index_fixed %>%
  group_by(upi_number) %>%
  summarise(
    source_combination = paste(sort(unique(source)), collapse = ", "),
    first_record_year = year(min(diagnosis_date)),
    .groups = 'drop'
  )

# Count, calculate percentages, and sort the data by cohort year
cohort_combination_counts <- upi_cohort_summary %>%
  group_by(first_record_year) %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    percentage = (number_of_people / sum(number_of_people)) * 100
  ) %>%
  ungroup() %>%
  arrange(desc(first_record_year), desc(number_of_people))

# Display the results
print(cohort_combination_counts, n = 50)


# --- 5. EXPORT RESULTS TO A SINGLE EXCEL FILE ---

write_xlsx(
  list(
    "Overall Counts" = source_combination_counts,
    "Counts By Year" = cohort_combination_counts
  ),
  path = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/SMR01 and NRS Check/dementia_source_analysis.xlsx"
)

cat("Successfully exported both summaries to a single Excel file.\n")