library(tidyverse)
library(lubridate)
library(janitor)

# Load the dementia index
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# Identify initial-diagnosis events
dementia_index <- dementia_index %>%
  group_by(upi_number) %>%
  mutate(event_number = row_number()) %>%
  mutate(initial_diagnosis_event = case_when(event_number == 1 ~ T,
                                             T ~ F)) %>%
  ungroup() %>%
  arrange(upi_number, event_number) %>% # Arrange the data for visual inspection
  select(upi_number, event_number, initial_diagnosis_event, everything()) %>%
  filter(initial_diagnosis_event == T)