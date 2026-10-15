library(tidyverse)
library(lubridate)
library(janitor)
library(writexl)


single_row_upis <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds") %>%
  group_by(upi_number) %>%
  filter(n() == 1) %>%
  ungroup()

count <- single_row_upis %>%
  group_by(source) %>%
  count()

diagnosis_summary <- single_row_upis %>%
  count(source, diagnosis, diagnosis_description, sort = TRUE)

dementia_index_distinct_upis <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds") %>%
  summarise(distinct_upi_count = n_distinct(upi_number))

write_xlsx(
  list(
    "Single Row UPI Counts" = count,
    "Diagnosis Summary" = diagnosis_summary,
    "Distinct UPI Count (All Data)" = dementia_index_distinct_upis
  ),
  path = "/PHI_conf/Dementia_Index/code/Jack/dementia-index/ad-hoc requests/SMR01 and NRS Check/single row summary.xlsx"
)

