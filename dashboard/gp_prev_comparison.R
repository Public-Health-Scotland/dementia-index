# gp prevalence data
# Index comparison to GP prevalence data
data_dir <- "/PHI_conf/Dementia_Index/outputs/HSCP dashboard/"

# data downloaded on 01/08/25 
gp_files <- c(paste0(data_dir, "DPDementia2.csv"), paste0(data_dir, "DPDementia.csv"))

gp_data <- bind_rows(lapply(gp_files, read_csv_arrow)) %>% 
  clean_names()

head(gp_data)
unique(gp_data$year)

list_sizes <- read_xlsx(paste0(data_dir, "table2_practice_listsizes_by_localauthority_age_2013_2023.xlsx"),
                        sheet = 'Data', skip = 2) %>% 
  clean_names()

list_size_25plus <- list_sizes %>%
  filter(practice_type == 'All') %>% 
  select(local_authority, year, x25_44:x85) %>%
  pivot_longer(cols = x25_44:x85, names_to = 'age_group', names_prefix = 'x', values_to = 'list_size') %>% 
  mutate(age_group = str_replace(age_group, '_', '-'),
         age_group = case_when(age_group == '85' ~ '85+',
                               .default = age_group),
         list_size = as.numeric(list_size),
         hscp2019name = case_when(local_authority %in% c("Clackmannanshire", "Stirling") ~ "Clackmannanshire and Stirling",
                                  local_authority == 'City of Edinburgh' ~ 'Edinburgh',
                                  local_authority == 'Na h-Eileanan Siar' ~ 'Western Isles',
                                  .default = local_authority),
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  arrange(hscp2019name, year, age_group) %>% 
  summarise(list_size = sum(list_size),
            .by = c(year, hscp2019name, age_group))

list_size_65plus <- list_sizes %>% 
  filter(practice_type == 'All') %>% 
  select(local_authority, year, x65_74:x85) %>%
  mutate(x65_74 = as.numeric(x65_74), x75_84 = as.numeric(x75_84), 
         x85 = as.numeric(x85),
         x65plus = rowSums(across(where(is.numeric))),
         year = paste0(year, "/", substr(year + 1, 3, 4)),
         hscp2019name = case_when(local_authority %in% c("Clackmannanshire", "Stirling") ~ "Clackmannanshire and Stirling",
                                  local_authority == 'City of Edinburgh' ~ 'Edinburgh',
                                  local_authority == 'Na h-Eileanan Siar' ~ 'Western Isles',
                                  .default = local_authority),
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  summarise(x65_74 = sum(x65_74),
            x75_84 = sum(x75_84), 
            x85 = sum(x85),
            x65plus = sum(x65plus),
            .by = c(year, hscp2019name))

list_size_65_onwards <- list_size_65plus %>% 
  select(-x65plus) %>% 
  pivot_longer(cols = x65_74:x85, names_to = 'age_group', names_prefix = 'x', values_to = 'list_size') %>% 
  mutate(age_group = str_replace(age_group, '_', '-'),
         age_group = case_when(age_group == '85' ~ '85+',
                               .default = age_group))

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
  filter(!grepl('NHS', hscp2019name, ignore.case = T)) %>%
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>%
  summarise(count_dementia = sum(count_dementia),
            .by = c(year, hscp2019name, age, age_group, age_65plus, gender))

# gp_data_2 <- gp_data %>% 
#   filter(age > "20-24",
#          age != 'All') %>% 
#   mutate(pop_year = year-1,
#          year = paste0(pop_year, "/", substr(year, 3, 4)),
#          # age_group = case_when(age < "60-64" ~ "18-59",
#          #                       .default = age),
#          age_group = case_when(age %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
#                                age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
#                                age %in% c("65-69", "70-74") ~ "65-74",
#                                age %in% c("75-79", "80-84") ~ "75-84",
#                                age == "85plus" ~ "85+",
#                                .default = age),
#          age_group = factor(age_group, levels = age_order_gp, ordered = T),
#          age_65plus = case_when(age >= "65-69" & age != "All" ~ T,
#                                 .default = F)) %>% 
#   transmute(year, hscp2019name = stringr::str_remove(location, ' HSCP'), 
#             age, age_group, age_65plus,
#             gender = case_when(sex == 'F' ~ 'Female',
#                                sex == 'M' ~ 'Male',
#                                .default = sex),
#             count_dementia) %>% 
#   filter(!grepl('NHS', hscp2019name, ignore.case = T)) %>% 
#   mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
#   summarise(count_dementia = sum(count_dementia), 
#             .by = c(year, hscp2019name, age, age_group, age_65plus, gender))

gp_data_65plus <- gp_data_2 %>%
  filter(age_65plus == T) %>% 
  transmute(year, hscp2019name, age_group = "65+", gender, count_dementia) %>% 
  summarise(count_gp_dementia = sum(count_dementia), .by = c(year, hscp2019name, age_group, gender))

gp_data_age_groups <- gp_data_2 %>%
  filter(age_group != 'All') %>%
  select(year, hscp2019name, age_group, gender, count_dementia) %>% 
  summarise(count_gp_dementia = sum(count_dementia), .by = c(year, hscp2019name, age_group, gender))
  
gp_data_age_groups_all <- gp_data %>% 
  filter(sex == 'All',
         # age >= "60-64",
         age != 'All',
         year != max(year)) %>% 
  mutate(pop_year = year-1,
         year = paste0(pop_year, "/", substr(year, 3, 4)),
         age = case_when(age == "85plus" ~ "85+",
                         age < "60-64" ~ "18-59",
                         .default = age)) %>% 
  transmute(year, hscp2019name = stringr::str_remove(location, ' HSCP'), 
            age_group = age, count_dementia, rate_dementia) %>% 
  filter(!grepl('NHS', hscp2019name, ignore.case = T)) %>% 
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T),
         age_group = factor(age_group, levels = age_order2, ordered = T)) %>%
  arrange(hscp2019name, year, age_group)

# gp_data_age_groups_all <- gp_data_age_groups %>%
#   filter(gender == 'All') %>% select(-gender) %>%
#   summarise(count_gp_dementia = sum(count_gp_dementia),
#             .by = c(year, hscp2019name, age_group)) %>% 
#   mutate(cal_year = as.numeric(substr(year, 1, 4))) %>% 
#   left_join(list_size_25plus, by = c('cal_year' = 'year', 'hscp2019name', 'age_group')) %>% 
#   filter(year != max(year)) %>% 
#   mutate(rate = (count_gp_dementia/list_size)*100000,
#          age_group = factor(age_group, levels = age_order_gp, ordered = T)) %>%
#  arrange(hscp2019name, year, age_group)



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

### 65+ rate ####
index_65plus_rate_comp <- prevalence_all_65plus_rates %>% 
  summarise(count_index = sum(individuals),
            pop = sum(pop),
            .by = c(year, hscp2019name, age_group)) %>% 
  mutate(rate = (count_index/pop)*100000,
         source = 'Dementia Index')

gp_65plus_rate_comp <- gp_data_65plus %>%
  filter(gender == 'All',
         year < max_(year),
         !is.na(hscp2019name)) %>% 
  left_join(list_size_65plus %>% select(year, hscp2019name, x65plus)) %>% 
  fill(x65plus) %>% 
  mutate(rate = (count_gp_dementia/x65plus)*100000,
         source = 'GP Dashboard',
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  select(-gender)

gp_65plus_comp_rate <- index_65plus_rate_comp %>% 
  filter(year >= "2021/2022") %>% 
  # left_join(gp_65plus_rate_comp) %>% 
  bind_rows(gp_65plus_rate_comp) %>% 
  arrange(hscp2019name, year) %>% 
  select(year, area = hscp2019name, source, rate)

shared_gp_65plus_comp_rate <- SharedData$new(
  gp_65plus_comp_rate,
  key = ~area, group = "Group 3")

## Age groups ####
# Patient age is calculated in terms of how old a patient would have 
# been as at the end of each financial year presented in this publication
# same as this dashboard

# prev_comparison_ages <- prevalence_all %>% 
#   select(-lookup) %>% 
#   filter(year >= min(gp_data_65plus$year)) %>%
#   mutate(age_group = case_when(age_group >= "85-89" ~ "85plus",
#                                .default = age_group),
#          age_group = factor(age_group, levels = age_order2, ordered = T),
#          gender = 'All') %>% 
#   summarise(count_index = sum(individuals), .by = c(year, hscp2019name, gender, age_group)) %>% 
#   left_join(gp_data_age_groups)

prev_gp_ages <- bind_rows(lapply(dates, prev_pl_df_gp, df = index_first))

prev_gp_ages_ca <- prev_gp_ages %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name, age_group)) %>%
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  arrange(hscp2019name, year, age_group) %>% 
  relocate(hscp2019name, .before = age_group)
  

# prev_comparison_ages_numbers <- prev_gp_ages_ca %>%
#   mutate(hscp2019name = factor('Scotland', levels = area_order, ordered = T)) %>% 
#   summarise(individuals = sum(individuals),
#             .by = c(year, hscp2019name, age_group)) %>%
#   bind_rows(prev_gp_ages_ca) %>% 
#   arrange(hscp2019name, year, age_group)
# 
# prev_comparison_ages_rates <- prev_comparison_ages_numbers %>% 
#   mutate(cal_year = as.numeric(substr(year, 1, 4))) %>% 
#   left_join(pops_gp_age_list_groups, by = c('cal_year' = 'year', 'hscp2019name', 'age_group')) %>% 
#   mutate(rate = (individuals/pop)*100000) %>% 
#   select(-c(cal_year, individuals, pop)) 

prev_gp_ages <- prevalence_all_rates %>%
  mutate(age_group = case_when(age_group >= "85-89" ~ "85+",
                               .default = age_group),
         age_group = factor(age_group, levels = age_order2, ordered = T)) %>% 
  summarise(individuals = sum(individuals),
            pop = sum(pop),
            .by = c(year, hscp2019name, age_group)) %>% 
  mutate(rate = (individuals/pop)*100)

prev_comparison_ages_numbers <- prev_gp_ages %>%
  # filter(age_group != min(age_group)) %>% 
  select(-c(pop, rate))

prev_comparison_ages_rates <- prev_gp_ages %>% 
  filter(age_group != min(age_group)) %>% 
  select(-c(individuals, pop))

prev_comparison_ages_numbers_gp <- gp_data_age_groups_all %>%
  summarise(count_dementia = sum(count_dementia),
            .by = c(year, hscp2019name, age_group))
  
prev_comparison_ages_rates_gp <- gp_data_age_groups_all %>% 
  filter(age_group != min(age_group)) %>% 
  rename(rate = rate_dementia) %>% 
  select(-c(count_dementia))

# Make data wide for both sources and bind together
prev_comparsion_ages_source_numbers <- bind_rows(
  # index
  prev_comparison_ages_numbers %>% 
    pivot_wider(names_from = age_group, values_from = individuals, values_fill = 0) %>% 
    mutate(source = 'Dementia Index') %>% 
    adorn_totals('col'),
  
  # GP dashboard
  prev_comparison_ages_numbers_gp %>% 
    pivot_wider(names_from = age_group, values_from = count_dementia, values_fill = 0) %>% 
    mutate(source = 'GP Dashboard') %>% 
    left_join(gp_data_2 %>% filter(age == 'All', gender == 'All') %>% select(year, hscp2019name, Total = count_dementia))
  
  # prev_comparison_ages %>% 
  #   select(-c(gender, count_index)) %>%
  #   pivot_wider(names_from = age_group, values_from = count_gp_dementia) %>% 
  #   mutate(source = 'GP Dashboard')
  ) %>% 
  filter(year >= "2021/22") %>% 
  select(year, hscp2019name, source, everything()) %>% 
  arrange(hscp2019name, year, source)

prev_comparsion_ages_source_rates <- bind_rows(
  # index
  prev_comparison_ages_rates %>% 
    pivot_wider(names_from = age_group, values_from = rate, values_fill = 0) %>% 
    mutate(source = 'Dementia Index'),
  
  # GP dashboard
  prev_comparison_ages_rates_gp %>% 
    pivot_wider(names_from = age_group, values_from = rate, values_fill = 0) %>% 
    mutate(source = 'GP Dashboard')
) %>% 
  filter(year >= "2021/22") %>% 
  select(year, hscp2019name, source, everything()) %>% 
  arrange(hscp2019name, year, source)


shared_prev_comparsion_ages_source_numbers <- SharedData$new(
  prev_comparsion_ages_source_numbers %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 3"
)

shared_prev_comparsion_ages_source_rates <- SharedData$new(
  prev_comparsion_ages_source_rates %>% 
    rename(area = hscp2019name), key = ~area, group = "Group 3"
)

### Rates ####
index_rates_comp <- prevalence_all_rates %>% 
  filter(year >= "2021/22",
         age_group > "60-64") %>% 
  mutate(age_group = case_when(age_group %in% c('65-69', '70-74') ~ '65-74',
                               age_group %in% c('75-79', '80-84') ~ '75-84',
                               .default = '85+')) %>% 
  summarise(count_index = sum(individuals),
            pop = sum(pop),
            .by = c(year, hscp2019name, age_group)) %>% 
  mutate(rate = (count_index/pop)*100000,
         source = 'Dementia_index') %>% 
  select(-c(count_index, pop))

gp_rates_comp <- gp_data_age_groups %>%
  filter(!age_group %in% c("All", "18-59", "60-64"),
         gender == "All") %>% 
  mutate(age_group = case_when(age_group %in% c('65-69', '70-74') ~ '65-74',
                               age_group %in% c('75-79', '80-84') ~ '75-84',
                               .default = '85+')) %>% 
  summarise(count_gp_dementia = sum(count_gp_dementia),
            .by = c(year, hscp2019name, age_group)) %>% 
  left_join(list_size_65_onwards) %>% 
  filter(!is.na(list_size)) %>% 
  mutate(rate = (count_gp_dementia/list_size)*100000,
         source = "GP Dashboard") %>% 
  select(-c(count_gp_dementia, list_size))

gp_comp_rates <- index_rates_comp %>% 
  bind_rows(gp_rates_comp) %>% 
  pivot_wider(names_from = age_group, values_from = rate) %>% 
  rename(area = hscp2019name) %>% 
  arrange(area, year)

shared_gp_comp_rates <- SharedData$new(
  gp_comp_rates, key = ~area, group = "Group 3"
)
