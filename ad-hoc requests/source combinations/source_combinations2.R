# --- 1. SETUP: Load Libraries and Define Parameters ---
library(tidyverse)
library(lubridate)
library(writexl)
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
  # NEW: Filter the deaths data to our end date
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

# NEW: Filter the index to only include records up to the end date
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

source_combination_counts <- upi_source_summary %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    percentage = (number_of_people / sum(number_of_people)) * 100
  )


# --- 5. BY YEAR ANALYSIS (Using Corrected Data) ---

upi_cohort_summary <- dementia_index_fixed %>%
  group_by(upi_number) %>%
  summarise(
    source_combination = paste(sort(unique(source)), collapse = ", "),
    first_record_year = year(min(diagnosis_date)),
    .groups = 'drop'
  )

cohort_combination_counts <- upi_cohort_summary %>%
  group_by(first_record_year) %>%
  count(source_combination, sort = TRUE, name = "number_of_people") %>%
  mutate(
    percentage = (number_of_people / sum(number_of_people)) * 100
  ) %>%
  ungroup() %>%
  arrange(desc(first_record_year), desc(number_of_people))


# --- 6. EXPORT RESULTS TO A SINGLE EXCEL FILE ---

write_xlsx(
  list(
    "Overall Counts" = source_combination_counts,
    "Counts By Year" = cohort_combination_counts
  ),
  path = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/source combinations/dementia_source_analysis_corrected_to_2024.xlsx"
)

cat("Successfully exported both summaries to a single Excel file.\n")