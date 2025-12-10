library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(purrr)
library(lubridate)
library(phsmethods)
library(phsstyles)
# remotes::install_github("Public-Health-Scotland/phslookups") 
library(phslookups)
library(ggplot2)
library(scales)
library(plotly)

# Dementia Index - Publication Charts and Tables
# Years 2020/21-2023/24

# import data ####
code_folder <- paste0(here::here(), "/Publications/2026_02 - Identified Dementia Population/")
outputs_folder <- "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/"

# import functions
source(paste0(code_folder, "/functions.R"))

# Index data
index <- readRDS(paste0('/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds')) %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name)) %>%
  # remove those with no Scottish residence data
  filter(!is.na(ca2019name))

# end dates for required years for age calculations
year_end_date <- dmy(31032021)
year_end_dates <- seq.Date(year_end_date, year_end_date+years(3), by = 'year')

# set age groups and areas as factors
age_order <- c("18-59", create_age_groups(seq(60, 90, by=5), 60, 90, by = 5, as_factor = F))
area_order <- c("Scotland", sort(unique(index$hbres)), sort(unique(index$hscp2019name)))

# population data ####
source(paste0(code_folder, 'populations.R'))

# max epop for all ages - (100000 per sex group) see calculate_easr function/pops_easr for breakdowns
# pops_easr
max_epop <- 200000
# max epop for 18+ - (80700 per sex group)
max_epop_18plus <- 161400

# Recorded Prevalence ####
# get prevalence data for each patient per year
# use easr for rates
prevalence <- bind_rows(lapply(year_end_dates, prev_pl_df_easr, df = index))

## Scotland ####
# Total, Age, Sex & Deprivation 

### Total #### 
scotland_total <- scot_pops_easr_18plus %>%
  # use population figures for years 2020:2023
  filter(between(cal_year, 2020, 2023)) %>% 
  left_join(prevalence %>%
              mutate(area = factor('Scotland', levels = area_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex))) %>%
  relocate(year, .before = area) %>%
  mutate(area_type = 'National', .after = area) %>%
  # set numerator (individuals) and denominator (pop) for use in calculate_easr function
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = sex) %>% 
  select(-c(cal_year, individuals, pop)) %>% 
  # fill in any year that is NA (numerator = 0) in the data
  fill(year, .direction = 'up')

scotland_total_chart <- scotland_total %>%
  # join for use in calculate_easr function
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_total_table <- scotland_total_chart %>%
  select(Year = year, Area = area, Individuals = numerator, `Rate` = rate)

# rates
scotland_table_rate <- scotland_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

# counts
scotland_table_count <- scotland_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)

### Age Distribution ####
scotland_age_totals <- scot_age_pops_easr_18plus %>%
  filter(between(cal_year, 2020, 2023)) %>% 
  left_join(prevalence %>% 
              mutate(area = factor('Scotland', levels = area_order, ordered = T),
                     # as we are calculating 18-59 age group need to add these all together into one group
                     age_group = case_when(age_group < "60-64" ~ "18-59",
                                           .default = as.character(age_group)),
                     age_group = factor(age_group, levels = age_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex))) %>% 
  relocate(year, .before = area) %>%
  mutate(area_type = 'National', .after = area) %>%
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = sex) %>% 
  select(-c(cal_year, individuals, pop)) %>% 
  fill(year, .direction = 'up')

scotland_age_chart <- scotland_age_totals %>%
  # Create a group for the under 59s (check if this is valid practice)
  # function below will be added so 1 == 55200 sized population (addition on groups 4-12 using age groups 18 and 19 in group 4, 1100 pop each)
  left_join(easr_pops %>% 
              mutate(age_group = case_when(age_group < "60-64" ~ "18-59",
                                           .default = age_group),
                     age_group = factor(age_group, levels = age_order, ordered = T),
                     epop = case_when(age_group == '18-59' ~ 1,
                                      .default = epop)) %>% 
              distinct()) %>% 
  # function to handle the age groups
  calculate_easr_age(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_age_table <- scotland_age_chart %>%
  select(Year = year, Area = area, "Age Group" = age_group, Individuals = numerator, Rate = rate)

scotland_age_table_rate <- scotland_age_table %>%
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

scotland_age_table_count <- scotland_age_table %>%
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)

### Sex ####
# This is already broken down by sex in the scotland_total df so can use that
# and calculate_easr_sex function will group by sex
scotland_sex_total_chart <- scotland_total %>%
  left_join(easr_pops) %>% 
  # use half the total epop as we are splitting between sex
  calculate_easr_sex(epop_total = max_epop_18plus/2, area_type = first(area_type), epop_age = '18+') %>% 
  mutate(sex = case_when(sex == 1 ~ 'Male',
                         .default = 'Female'))

scotland_sex_table <- scotland_sex_total_chart %>% 
  select(Year = year, Area = area, Sex = sex, Individuals = numerator, Rate = rate) 

scotland_sex_table_rate <- scotland_sex_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate) %>% 
  arrange(Sex)

scotland_sex_table_count <- scotland_sex_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals) %>% 
  arrange(Sex)

### Deprivation ####
scotland_dep_total <- simd_pops_easr_18plus %>%
  filter(between(cal_year, 2020, 2023)) %>% 
  left_join(prevalence %>%
              mutate(area = factor('Scotland', levels = area_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex, simd2020v2_sc_quintile))) %>%
  relocate(year, .before = area) %>%
  mutate(area_type = 'National', .after = area) %>%
  mutate(quintile = case_match(simd2020v2_sc_quintile,
                               1 ~ '1 (Most Deprived)',
                               5 ~ '5 (Least Deprived)',
                               .default = as.character(simd2020v2_sc_quintile)),
         .before = simd2020v2_sc_quintile) %>% 
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = quintile) %>% 
  select(-c(cal_year, simd2020v2_sc_quintile, individuals, pop)) %>% 
  fill(year, .direction = 'up')

scotland_dep_total_chart <- scotland_dep_total %>%
  left_join(easr_pops) %>%
  calculate_easr(epop_total = max_epop_18plus, area_type = first(area_type), epop_age = '18+')

# table data
scotland_dep_table <- scotland_dep_total_chart %>% 
  select(Year = year, Area = area, Quintile = quintile, Individuals = numerator, Rate = rate)

scotland_dep_table_rate <- scotland_dep_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

scotland_dep_table_count <- scotland_dep_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)
  

## Health Board - Total ####
hb_total <- hb_pops_easr_18plus %>%
  filter(between(cal_year, 2020, 2023)) %>% 
  left_join(prevalence %>%
              mutate(area = factor(hbres, levels = area_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex))) %>%
  relocate(year, .before = area) %>%
  mutate(area_type = 'HB', .after = area) %>%
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = sex) %>% 
  select(-c(cal_year, individuals, pop)) %>% 
  fill(year, .direction = 'up')

hb_total_chart <- hb_total %>%
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, epop_age = '18+', area_type = first(area_type)) %>% 
  # add scotland total rates for comparisons
  bind_rows(scotland_total_chart)

# table data
hb_total_table <- hb_total_chart %>%
  arrange(area_type, area) %>% 
  select(Year = year, Area = area, Individuals = numerator, Rate = rate)

hb_table_rate <- hb_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hb_table_count <- hb_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)


## HSCP - Total ####
hscp_total <- hscp_pops_easr_18plus %>%
  filter(between(cal_year, 2020, 2023)) %>% 
  left_join(prevalence %>%
              mutate(area = factor(hscp2019name, levels = area_order, ordered = T)) %>%
              summarise(individuals = n(), .by = c(year, cal_year, area, age_group, sex))) %>%
  relocate(year, .before = area) %>%
  mutate(area_type = 'HSCP', .after = area) %>%
  mutate(numerator = case_when(is.na(individuals) ~ 0,
                               .default = individuals),
         denominator = pop, .after = sex) %>% 
  select(-c(cal_year, individuals, pop)) %>% 
  fill(year, .direction = 'up')

hscp_total_chart <- hscp_total %>%
  left_join(easr_pops) %>% 
  calculate_easr(epop_total = max_epop_18plus, epop_age = '18+', area_type = first(area_type)) %>% 
  # add scotland total rates for comparisons 
  bind_rows(scotland_total_chart)

# table data
hscp_total_table <- hscp_total_chart %>%
  arrange(area_type, area) %>% 
  select(Year = year, Area = area, Individuals = numerator, Rate = rate)

hscp_table_rate <- hscp_total_table %>% 
  select(-Individuals) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hscp_table_count <- hscp_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Individuals)


# save out table data
table_data <- list(
  "Scotland total - Rate" = scotland_table_rate,
  "Sex split - Rate" = scotland_sex_table_rate,
  "Age split - Rate" = scotland_age_table_rate,
  "Deprivation - Rate" = scotland_dep_table_rate,
  "Health Boards - Rate" = hb_table_rate,
  "HSCP - Rate" = hscp_table_rate,
  "Scotland total - Count" = scotland_table_count,
  "Sex split - Count" = scotland_sex_table_count,
  "Age split - Count" = scotland_age_table_count,
  "Deprivation - Count" = scotland_dep_table_count,
  "Health Boards - Count" = hb_table_count,
  "HSCP - Count" = hscp_table_count
)

writexl::write_xlsx(table_data, paste0(outputs_folder, 'chart_table_data_18plus_rates.xlsx'))

# Charts (Not used anymore - now done in excel) ####
chart_colours <- unname(phs_colour_values)
chart_annotation_colour <- 'white'

## 18+ total rate ####
ggplot(scotland_total_chart_18plus) +
  geom_line(aes(x = year, y = rate, group = area, colour = area),
            stat = 'identity', linewidth = 1, show.legend = F) +
  ggtitle("") +
  # ggtitle("People identified as living with dementia in Scotland",
  #         subtitle = "EASR rate per 100,000") +
  labs(x = 'Financial Year', y = 'Rate per 100,000') +
  scale_colour_manual(values = chart_colours[1])  +
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1500)) +
  
  theme_dash()

## Sex chart
# chart_scotland_sex <- 
ggplot(scotland_sex_total_chart %>% rename(Sex = sex)) +
  geom_line(aes(x = year, y = rate, group = Sex, colour = Sex),
            stat = 'identity', linewidth = 1) +
  # ggtitle("People living with dementia at end of year by sex",
  #         subtitle = 'Rate per 100,000 population') +
  ggtitle('') +
  labs(x = 'Financial Year', y = 'Rate per 100,000') +
  scale_colour_manual(values = c(chart_colours[1:2]))  +
  
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1500)) +
  theme_dash() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 16))

## Age groups - latest year ####
# chart_scotland_age <- 
ggplot(scotland_age_chart %>% filter(year == max(year))) +
  geom_bar(mapping = aes(x = rate, y = age_group, colour = area, fill = area), stat = "identity") +
  labs(x = 'Age Group', y = 'Rate per 100,000') +
  ggtitle("") +
  # ggtitle("People identified as living with dementia in Scotland by age group (2024)",
  #         subtitle = 'Rate per 100,000 population') +
  # scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ",")) +
  geom_text(aes(x = rate, y = age_group, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, fontface = 'bold', nudge_x = 300) +
  scale_colour_manual(values = c(chart_colours[1], 'black')) +
  scale_fill_manual(values = c(chart_colours[1], 'black')) +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 12000)) +
  
  theme_bar()

## Deprivation Charts ####
ggplot(scotland_dep_total_chart %>% filter(year == max(year)),
       aes(y = reorder(quintile, rate))) +
  geom_bar(mapping = aes(x = rate, colour = quintile, fill = quintile), stat = "identity") +
  geom_text(aes(x = rate, y = quintile, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, hjust = 0.5, nudge_x = -200, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  # ggtitle("People identified as living with dementia by HSCP (2024)",
  #         subtitle = 'Rate per 100,000 population (18 plus)') +
  ggtitle("") +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1600)) +
  scale_colour_manual(values = c(chart_colours[1:5], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1:5], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +
  
  theme_bar()

ggplot(scotland_dep_total_chart %>% filter(year == max(year)) %>% 
         mutate(quintile = case_when(grepl("1 ", quintile) ~ "(Most Deprived) 1",
                                     grepl("5 ", quintile) ~ "(Least Deprived) 5",
                                     .default = quintile)),
       aes(y = reorder(quintile, rate))) +
  geom_bar(mapping = aes(x = rate, colour = area_type, fill = area_type), stat = "identity") +
  geom_text(aes(x = rate, y = quintile, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, hjust = 0.5, nudge_x = -200, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  # ggtitle("People identified as living with dementia by HSCP (2024)",
  #         subtitle = 'Rate per 100,000 population (18 plus)') +
  ggtitle("") +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1600)) +
  scale_colour_manual(values = c(chart_colours[1], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +
  
  theme_bar()

ggplot(scotland_dep_total_chart) +
  geom_line(aes(x = year, y = rate, group = quintile, colour = quintile),
            stat = 'identity', linewidth = 1.8, show.legend = F) +
  
  # ggtitle("People identified as living with dementia in Scotland",
  #         subtitle = "EASR rate per 100,000") +
  labs(x = 'Financial Year', y = 'Rate per 100,000') +
  scale_colour_manual(values = chart_colours[1:5])  +
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1600)) +
  
  theme_dash()

## HB Chart ####
ggplot(hb_total_chart %>% filter(year == max(year)),
       aes(y = reorder(area, rate))) +
  geom_bar(mapping = aes(x = rate, colour = area_type, fill = area_type), stat = "identity") +
  geom_text(aes(x = rate, y = area, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, hjust = 0.5, nudge_x = -100, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  # ggtitle("People identified as living with dementia by HSCP (2024)",
  #         subtitle = 'Rate per 100,000 population (18 plus)') +
  ggtitle("") +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1500)) +
  scale_colour_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +
  
  theme_bar()

## HSCP Chart ####
ggplot(hscp_total_chart %>% filter(year == max(year)),
       aes(y = reorder(area, rate))) +
  geom_bar(mapping = aes(x = rate, colour = area_type, fill = area_type), stat = "identity",
           width = 0.8) +
  geom_text(aes(x = rate, y = area, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, hjust = 0.5, nudge_x = -100, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  # ggtitle("People identified as living with dementia by HSCP (2024)",
  #         subtitle = 'Rate per 100,000 population (18 plus)') +
  ggtitle("") +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1750)) +
  scale_colour_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +
  
  theme_bar() +
  theme(axis.ticks.x = element_blank())