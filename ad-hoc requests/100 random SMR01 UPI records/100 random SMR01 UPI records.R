# --- 1. SETUP: Load Libraries and Define Parameters ---
library(tidyverse)
library(lubridate)
library(writexl) # Reverted to writexl for simple saving
library(odbc)
library(janitor)

# Establish connection to the database (will ask for password)
SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]],
                            pwd = .rs.askForPassword("Password:"))

# Define the start and end dates for the analysis
cohort_start_date <- as.Date("2014-01-01")
cohort_end_date <- as.Date("2024-12-31")

# Define dementia ICD-10 codes
icd10_dementia <- c("F00", "F000", "F001", "F002", "F009",
                    "F01","F010", "F011", "F012",  "F013","F018","F019",
                    "F02","F020", "F021", "F022",  "F023", "F024","F028",
                    "F03", "F051","G318 D","F028 A", "F1073",  "F1173",  
                    "F1273",  "F1373",  "F1473",  "F1573",  "F1673",  "F1773", "F1873",  "F1973",
                    "G30")

# --- 2. QUERY AND IDENTIFY DEMENTIA-SPECIFIC DEATHS ---

# Query all deaths from the start date
all_deaths_raw <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "SELECT UPI_NUMBER, CHI, DATE_OF_DEATH,
    UNDERLYING_CAUSE_OF_DEATH,
    CAUSE_OF_DEATH_CODE_0 ,CAUSE_OF_DEATH_CODE_1 ,CAUSE_OF_DEATH_CODE_2,
    CAUSE_OF_DEATH_CODE_3,CAUSE_OF_DEATH_CODE_4,CAUSE_OF_DEATH_CODE_5,
    CAUSE_OF_DEATH_CODE_6,CAUSE_OF_DEATH_CODE_7,CAUSE_OF_DEATH_CODE_8,
    CAUSE_OF_DEATH_CODE_9
    FROM ANALYSIS.GRO_DEATHS_C SMR
    WHERE DATE_OF_DEATH >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
    ")
  )
) %>%
  clean_names() %>%
  mutate(upi_number = if_else(is.na(upi_number), chi, upi_number)) %>%
  filter(!is.na(upi_number)) %>%
  # Filter the deaths data to our end date
  filter(date_of_death <= cohort_end_date)

# Create the final, formatted death rows using a checkable process
new_dementia_death_rows <- all_deaths_raw %>%
  # Pivot all cause of death columns into one long column
  pivot_longer(
    cols = c(underlying_cause_of_death, starts_with("cause_of_death_code")),
    names_to = "cause_position",
    values_to = "icd10_code",
    values_drop_na = TRUE
  ) %>%
  # Step 2a: FLAG each row if it's a dementia code
  rowwise() %>%
  mutate(is_dementia_death = any(stringr::str_starts(icd10_code, icd10_dementia))) %>%
  ungroup() %>%
  
  # Step 2b: FILTER to keep all rows for any person who has a dementia death
  group_by(upi_number) %>%
  filter(any(is_dementia_death)) %>%
  ungroup() %>%
  
  # Step 2c: SUMMARISE to get one unique death record per person
  group_by(upi_number) %>%
  summarise(date_of_death = first(date_of_death), .groups = "drop") %>%
  
  # Step 2d: FORMAT the data to match the dementia_index structure
  mutate(
    source = "NRS deaths",
    diagnosis_date = date_of_death
  ) %>%
  select(upi_number, source, diagnosis_date)

cat("Identified", nrow(new_dementia_death_rows), "unique dementia-related deaths from NRS data up to", format(cohort_end_date, "%Y-%m-%d"), ".\n")

# --- 3. CREATE THE CORRECTED DEMENTIA INDEX ---
# Load the original index
dementia_index_raw <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# Filter the index to only include records up to the end date
dementia_index_filtered <- dementia_index_raw %>%
  filter(diagnosis_date <= cohort_end_date)

# Remove ALL existing "NRS deaths" rows from the now-filtered index
dementia_index_no_deaths <- dementia_index_filtered %>%
  filter(source != "NRS deaths")

# Bind the new, accurate dementia death rows (which are also date-filtered)
dementia_index_fixed <- bind_rows(dementia_index_no_deaths, new_dementia_death_rows)


# --- 4. OVERALL ANALYSIS (Using Corrected Data) ---

upi_source_summary <- dementia_index_fixed %>%
  group_by(upi_number) %>%
  summarise(
    source_combination = paste(sort(unique(source)), collapse = ", "),
    .groups = 'drop'
  )


# --- 5. DETERMINE UPIs WHICH ONLY APPEAR IN SMR01 ---

smr_only_upis <- upi_source_summary %>%
  filter(source_combination == "SMR01")

# --- 6. RANDOMLY SAMPLE 100 UPIs ---
# For reproducibility, set a seed.
set.seed(42)

# Use the sample_n() function from dplyr to randomly select 100 UPIs
random_sample_100_upis <- smr_only_upis %>%
  sample_n(100) %>%
  select(upi_number)


# --- 7. GET SMR01 DATA FOR THE SAMPLED UPIs ---

# First, create a comma-separated string of the UPI numbers for the SQL query.
# Each UPI number needs to be enclosed in single quotes for the SQL syntax.
upi_list_for_query <- paste0("'", paste(random_sample_100_upis$upi_number, collapse = "', '"), "'")

# Construct the full SQL query using the list of UPIs in an IN clause.
smr01_query <- paste0("
  SELECT 
    UPI_NUMBER, LINK_NO, CIS_MARKER, 
    ADMISSION_DATE, DISCHARGE_DATE, SEX, HBTREAT_CURRENTDATE, LOCATION, DOB,
    MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5
  FROM ANALYSIS.SMR01_PI SMR
  WHERE UPI_NUMBER IN (", upi_list_for_query, ")
  ORDER BY link_no, admission_date, discharge_date")

# Execute the query to get the data for our sample
data_smr01_sample <- as_tibble(
  dbGetQuery(SMRAConnection, smr01_query)
) %>%
  clean_names()

cat("Successfully retrieved", nrow(data_smr01_sample), "SMR01 records for the 100 sampled UPIs.\n")





# --- 8. LOAD ICD-10 LOOKUP TABLE ---
lookup_icd10 <- read_xlsx("/conf/linkage/output/lookups/Data Management/ICD10_OPCS4/ICD10 basic.xlsx") %>%
  clean_names() %>% 
  select(-codes)

cat("ICD-10 lookup table loaded successfully.\n")


# --- 9. TRUNCATE CODES AND JOIN ICD-10 DESCRIPTIONS ---

# First, truncate all ICD-10 codes in the SMR01 data to a maximum of 4 characters
# to ensure they match the level of detail in the lookup table.
data_smr01_truncated <- data_smr01_sample %>%
  mutate(
    main_condition = substr(main_condition, 1, 4),
    other_condition_1 = substr(other_condition_1, 1, 4),
    other_condition_2 = substr(other_condition_2, 1, 4),
    other_condition_3 = substr(other_condition_3, 1, 4),
    other_condition_4 = substr(other_condition_4, 1, 4),
    other_condition_5 = substr(other_condition_5, 1, 4)
  )

# Now, join the lookup table to each of the condition columns to add a description column for each.
# We rename the description column after each join to avoid column name clashes.
data_smr01_with_descriptions <- data_smr01_truncated %>%
  left_join(lookup_icd10, by = c("main_condition" = "code")) %>%
  rename(main_condition_desc = description) %>%
  
  left_join(lookup_icd10, by = c("other_condition_1" = "code")) %>%
  rename(other_condition_1_desc = description) %>%
  
  left_join(lookup_icd10, by = c("other_condition_2" = "code")) %>%
  rename(other_condition_2_desc = description) %>%
  
  left_join(lookup_icd10, by = c("other_condition_3" = "code")) %>%
  rename(other_condition_3_desc = description) %>%
  
  left_join(lookup_icd10, by = c("other_condition_4" = "code")) %>%
  rename(other_condition_4_desc = description) %>%
  
  left_join(lookup_icd10, by = c("other_condition_5" = "code")) %>%
  rename(other_condition_5_desc = description)





set.seed(123)  # optional, for reproducibility
data_smr01_with_descriptions2 <- data_smr01_with_descriptions %>%
  distinct(upi_number) %>%
  mutate(patient_id = sample(1:n())) %>%
  right_join(data_smr01_with_descriptions, by = "upi_number")


data_smr01_with_descriptions2 <- data_smr01_with_descriptions2
  select(patient_id, cis_marker, admission_date, discharge_date, main_condtion_desc, other_condition_1, other_condition_2, other_condition_1


# You can now view the final data with the added description columns
cat("Successfully added ICD-10 descriptions to the SMR01 data sample.\n")
print(head(data_smr01_with_descriptions))


# --- 10. SAVE THE FINAL DATASET ---

# Define the output file path
output_filepath <- "/PHI_conf/Dementia_Index/data/ad-hoc/smr01_dementia_sample_with_descriptions.xlsx"

# Save the final data frame to an Excel workbook
write_xlsx(data_smr01_with_descriptions, path = output_filepath)

cat("Successfully saved the final dataset to:", output_filepath, "\n")

