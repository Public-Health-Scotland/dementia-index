library(tidyverse)
library(writexl)

dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

##### Create a summary table with source combinations for each UPI ####
upi_source_summary <- dementia_index %>%
  group_by(upi_number) %>%
  summarise(
    source_combination = paste(sort(unique(source)), collapse = ", "),
    .groups = 'drop'
  )

#### Count people, sort, and add a percentage column ####
source_combination_counts <- upi_source_summary %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    percentage = (number_of_people / sum(number_of_people)) * 100
  )

print(source_combination_counts)


####  Write the final breakdown to an Excel file ####
write_xlsx(
  list("Source Combination Counts" = source_combination_counts),
  path = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/SMR01 and NRS Check/source_combination_summary.xlsx"
)


#### By Year ####

# Load the main dataset
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# --- Step 1: Find the first record year for each person ---
# We group by person, find their earliest diagnosis_date, and extract the year.
# This 'first_record_year' will be our cohort year.
upi_cohort_summary <- dementia_index %>%
  group_by(upi_number) %>%
  summarise(
    # Create the overall source combination, just like before
    source_combination = paste(sort(unique(source)), collapse = ", "),
    # Find the earliest date for this person and extract its year
    first_record_year = year(min(diagnosis_date)),
    .groups = 'drop'
  )

# --- Step 2: Count, calculate percentages, and sort the data ---
# We now add the arrange() function at the end of the pipeline.
cohort_combination_counts <- upi_cohort_summary %>%
  group_by(first_record_year) %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    # Calculate the percentage WITHIN each year's group
    percentage = (number_of_people / sum(number_of_people)) * 100
  ) %>%
  ungroup() %>%
  # NEW: Sort by year (latest first), then by count (highest first)
  arrange(desc(first_record_year), desc(number_of_people))

# Display the results
print(cohort_combination_counts, n = 50) # Print more rows to see the sorted output


# --- Step 3: (Optional) Write the final sorted breakdown to an Excel file ---
write_xlsx(
  list("Cohort Combination Counts" = cohort_combination_counts),
  path = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/SMR01 and NRS Check/source_combination_summary_by_year.xlsx"
)

