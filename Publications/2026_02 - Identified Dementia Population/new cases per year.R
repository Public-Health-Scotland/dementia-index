library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(phsmethods) 

# Load the dementia index
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")

# Identify initial-diagnosis events (earliest diagnosis_date per person)
dementia_initial_diagnoses <- dementia_index %>%
  group_by(upi_number) %>%
  arrange(diagnosis_date, .by_group = TRUE) %>%
  mutate(
    event_number = row_number(),
    initial_diagnosis_event = case_when(
      event_number == 1 ~ TRUE,
      event_number > 1 ~ FALSE
    )
  ) %>%
  ungroup() %>%
  filter(initial_diagnosis_event) # keep first diagnosis per person

# Restrict to relevant financial years
annual_dementia_counts <- dementia_initial_diagnoses %>%
  filter(!is.na(diagnosis_date)) %>%
  mutate(
    financial_year = extract_fin_year(diagnosis_date)
  ) %>%
  filter(
    financial_year %in% (c("2020/21", "2021/22", "2022/23", "2023/24"))
  ) %>%
  count(financial_year, name = "n_people") %>%
  arrange(financial_year)

# Check the output
print(annual_dementia_counts)

# Save the new aggregated data
write_csv(
  annual_dementia_counts,
  "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/annual_count_of_new_dementia_cases.csv"
)

# Plotting with Financial Year on X-axis
p <- ggplot(annual_dementia_counts, aes(x = financial_year, y = n_people)) +
  geom_col(fill = "#2b6cb0") +
  labs(
    title = "Annual New Dementia Cases (Financial Year)",
    subtitle = "Financial Years 2020/21 to 2023/24",
    x = "Financial Year",
    y = "Count of People"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

print(p)

ggsave(
  filename = "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/annual_count_of_new_dementia_cases.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)