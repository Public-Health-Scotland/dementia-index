# gp prevalence data
# Index comparison to GP prevalence data
data_dir <- "/PHI_conf/Dementia_Index/outputs/HSCP dashboard/"

# data downloaded on 01/08/25 
gp_files <- c(paste0(data_dir, "DPDementia2.csv"), paste0(data_dir, "DPDementia.csv"))

gp_data <- bind_rows(lapply(gp_files, read_csv_arrow)) %>% 
  clean_names()

head(gp_data)
unique(gp_data$year)

# year var is last year of FY change to full FY string and 
# set population year to first year of FY and identify
# relevant age groups for comparisons
gp_data_2 <- gp_data %>% 
  mutate(pop_year = year-1,
         year = paste0(pop_year, "/", substr(year, 3, 4)),
         age_group = case_when(age < "60-64" ~ "18-59",
                               .default = age),
         age_65plus = case_when(age >= "65-69" & age != "All" ~ T,
                                .default = F)) %>% 
  transmute(year, hscp2019name = stringr::str_remove(location, ' HSCP'), 
            age, age_group, age_65plus,
            gender = case_when(sex == 'F' ~ 'Female',
                               sex == 'M' ~ 'Male',
                               .default = sex),
            count_dementia) %>% 
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  summarise(count_dementia = sum(count_dementia), 
            .by = c(year, hscp2019name, age, age_group, age_65plus, gender))

gp_data_65plus <- gp_data_2 %>%
  filter(age_65plus == T) %>% 
  transmute(year, hscp2019name, age_group = "65+", gender, count_dementia) %>% 
  summarise(count_gp_dementia = sum(count_dementia), .by = c(year, hscp2019name, age_group, gender))


gp_data_age_groups <- gp_data_2 %>%
  # filter(age_group != 'All') %>% 
  select(year, hscp2019name, age_group, gender, count_dementia) %>% 
  summarise(count_gp_dementia = sum(count_dementia), .by = c(year, hscp2019name, age_group, gender))
  
age_order2 <- c(age_order[1:6], "85plus")

# numbers to numbers comparison with index prevalence ####
## 65 plus ####
prev_comparison_65plus <- prevalence_all_65plus %>% 
  filter(year >= "2021/22") %>% 
  rename(count_index = individuals) %>%
  # add totals for all people
  group_by(year, hscp2019name, age_group) %>%
  nest() %>%
  mutate(data = map(data, ~ .x %>% adorn_totals("row", name = "All"))) %>%
  unnest(cols = c(data)) %>% ungroup() %>% 
  left_join(gp_data_65plus)# %>% 
  # mutate(index_difference = count_index-count_gp_dementia) %>% 
  # mutate(area_diff_total = sum(index_difference), .by = hscp2019name)

prev_comparison_65plus %>% 
  select(-c(count_gp_dementia)) %>% #, area_diff_total, index_difference)) %>% 
  filter(hscp2019name == 'Scotland') %>% 
  pivot_wider(names_from = gender, values_from = count_index) %>% 
  relocate(Female, .before = Male) %>% adorn_totals('col')


prev_comparison_65plus_source <- bind_rows(
  # index
  prev_comparison_65plus %>% 
    select(-c(age_group, count_gp_dementia)) %>%
    pivot_wider(names_from = gender, values_from = count_index) %>% 
    mutate(source = 'Dementia Index'),
  
  # GP dashboard
  prev_comparison_65plus %>% 
    select(-c(age_group, count_index)) %>%
    pivot_wider(names_from = gender, values_from = count_gp_dementia) %>% 
    mutate(source = 'GP Dashboard')
) %>% 
  select(year, hscp2019name, source, everything()) %>% 
  arrange(hscp2019name, year, source) %>% 
  relocate(Female, .before = Male) %>% 
  rename(Total = All)
  


shared_prev_comparison_65plus_table <- SharedData$new(
  prev_comparison_65plus_source %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 3"
)


shared_prev_comparison_65plus_chart <- SharedData$new(
  prev_comparison_65plus_source %>% 
    pivot_longer(Female:Total, names_to = 'gender', values_to = 'individuals') %>% 
    filter(gender != 'Total') %>% 
    mutate(group = paste(gender, source, sep = ' - ')) %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 3"
)

## Age groups ####
# Patient age is calculated in terms of how old a patient would have 
# been as at the end of each financial year presented in this publication
# same as this dashboard

prev_comparison_ages <- prevalence_all %>% 
  select(-lookup) %>% 
  filter(year >= min(gp_data_65plus$year)) %>%
  mutate(age_group = case_when(age_group >= "85-89" ~ "85plus",
                               .default = age_group),
         age_group = factor(age_group, levels = age_order2, ordered = T),
         gender = 'All') %>% 
  summarise(count_index = sum(individuals), .by = c(year, hscp2019name, gender, age_group)) %>% 
  left_join(gp_data_age_groups)

# Make data wide for both sources and bind together
prev_comparsion_ages_source <- bind_rows(
  # index
  prev_comparison_ages %>% 
    select(-c(gender, count_gp_dementia)) %>%
    pivot_wider(names_from = age_group, values_from = count_index) %>% 
    mutate(source = 'Dementia Index'),
  
  # GP dashboard
  prev_comparison_ages %>% 
    select(-c(gender, count_index)) %>%
    pivot_wider(names_from = age_group, values_from = count_gp_dementia) %>% 
    mutate(source = 'GP Dashboard')
  ) %>% 
  select(year, hscp2019name, source, everything()) %>% 
  arrange(hscp2019name, year, source) %>% 
  adorn_totals('col')


shared_prev_comparsion_ages_source <- SharedData$new(
  prev_comparsion_ages_source %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 3"
)
