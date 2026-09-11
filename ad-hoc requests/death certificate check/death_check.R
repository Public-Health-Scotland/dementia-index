library(tidyverse)
library(lubridate)
library(janitor)
library(odbc)
library(ggplot2)
library(forcats)
library(openxlsx)

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]],
                            pwd = .rs.askForPassword("Password:"))

cohort_start_date <- as.Date("2014-01-01")

dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds") 
# deaths_extract <- readRDS("/PHI_conf/Dementia_Index/data/extracts/dementia_deaths.rds")

icd10_dementia <- c("F00", "F000", "F001", "F002", "F009",
                    "F01","F010", "F011", "F012",  "F013","F018","F019",
                    "F02","F020", "F021", "F022",  "F023", "F024","F028",
                    "F03", "F051","G318 D","F028 A", "F1073",  "F1173",  
                    "F1273",  "F1373",  "F1473",  "F1573",  "F1673",  "F1773", "F1873",  "F1973",
                    "G30")

deaths_temp_1 <- as_tibble(
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
  ungroup() %>%
  mutate(upi_number = case_when(is.na(upi_number) ~chi, T~upi_number)) %>%
  filter(!is.na(upi_number))

#### Check the position of dementia on the death certificate ####

deaths_temp_2 <- deaths_temp_1 %>%
  rowwise() %>%
  mutate(
    dementia_positions = {
      # Define our position labels: "U" for underlying, and 0-9 for the others
      position_labels <- c("U", 0:9)
      
      # Select all relevant columns for this row, in the correct order
      codes_in_row <- c_across(c(underlying_cause_of_death, starts_with("cause_of_death_code_")))
      
      # Check each code for a dementia prefix match
      matches <- sapply(codes_in_row, function(code) any(str_starts(code, icd10_dementia), na.rm = TRUE))
      
      # Find which positions returned TRUE (e.g., the 1st and 4th positions)
      match_indices <- which(matches)
      
      # Use the indices to get our labels (e.g., position_labels[c(1, 4)] -> "U", "3")
      final_positions <- position_labels[match_indices]
      
      # Paste the labels into a single string
      if (length(final_positions) > 0) paste(final_positions, collapse = ", ") else NA_character_
    }
  ) %>%
  ungroup()


position_summary <- deaths_temp_2 %>%
  # Filter out rows where no dementia code was found
  filter(!is.na(dementia_positions)) %>%
  # Count the occurrences of each unique value in dementia_positions
  count(dementia_positions, sort = TRUE) %>%
  # Add a percentage column for extra context
  mutate(percentage = n / sum(n) * 100)

# Print the summary table
print(position_summary)


# Take the top 20 positions for a cleaner chart
top_20_positions <- position_summary %>%
  head(20)

position_chart  <- ggplot(top_20_positions, aes(x = fct_reorder(dementia_positions, percentage), y = percentage)) +
  geom_col(fill = "#0072B2") +
  # Format the label to show one decimal place and a '%' sign
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")), 
    hjust = -0.2
  ) +
  coord_flip() +
  # Set the y-axis limit to make space for the labels
  scale_y_continuous(limits = c(0, max(top_20_positions$percentage) * 1.1)) +
  labs(
    title = "Analysis of dementia code positions on death certificates",
    subtitle = "20 most frequent combinations",
    x = "Position on Certificate",
    y = "Percentage (%) of Dementia Deaths"
  ) +
  theme_minimal()

ggsave("/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/death certificate check/dementia_position_chart.png", 
       plot = position_chart, 
       width = 10, 
       height = 8, 
       units = "in",
       dpi = 300)


#### Check whether dementia is mentioned in the death certificate of people on the index ####

# Step 1: Join your dementia cohort with the processed death records
# This keeps everyone in dementia_index and adds death info if it exists.
dementia_cohort_deaths <- dementia_index %>%
  select(upi_number) %>%
  group_by(upi_number) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(deaths_temp_2, by = "upi_number")

# Step 2: Summarize the results
dementia_on_cert_summary <- dementia_cohort_deaths %>%
  # Keep only the patients from the index who have actually died
  filter(!is.na(date_of_death)) %>%
  
  # Create a clear category based on whether dementia_positions has a value
  mutate(dementia_on_cert = case_when(
    !is.na(dementia_positions) ~ "Mentioned on Certificate",
    TRUE                         ~ "Not Mentioned on Certificate"
  )) %>%
  
  # Count how many people are in each group
  count(dementia_on_cert) %>%
  
  # Calculate the percentage for each group
  mutate(percentage = round(n / sum(n) * 100, 1))

# Print the final summary table. This is the main answer to your question.
print(dementia_on_cert_summary)


#### Save to Excel ####

# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "Position Summary Data")
writeData(wb, sheet = "Position Summary Data", x = position_summary)

addWorksheet(wb, "Death Certificate Summary")
writeData(wb, sheet = "Death Certificate Summary", x = dementia_on_cert_summary) # Assuming you created this from the previous step

addWorksheet(wb, "Position Summary Chart")
insertImage(wb, sheet = "Position Summary Chart", 
            file = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/death certificate check/dementia_position_chart.png", width = 10, height = 8, units = "in")


#Save the final Excel file
saveWorkbook(wb, file = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/death certificate check/Dementia_Mortality_Analysis_Report.xlsx", overwrite = TRUE)
