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
# Years 2020-2024

# import data ####
code_folder <- paste0(here::here(), "/Publications/2026_02 - Identified Dementia Population/")

# import functions
source(paste0(code_folder, "/functions.R"))

# Index data
index <- readRDS(paste0('/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds')) %>% 
  mutate(hscp2019name = case_when(
    ca2019name == "City of Edinburgh" ~ "Edinburgh",
    ca2019name == 'Na h-Eileanan Siar' ~ 'Western Isles',
    ca2019name %in% c("Stirling", "Clackmannanshire") ~ "Clackmannanshire and Stirling",
    .default = ca2019name))

year_end_date <- dmy(31032021)
year_end_dates <- seq.Date(year_end_date, year_end_date+years(3), by = 'year')

age_order <- c("18-59", create_age_groups(seq(60, 90, by=5), 60, 90, by = 5, as_factor = F))
area_order <- c("Scotland", sort(unique(index$hbres)), sort(unique(index$hscp2019name)))

# population data ####
source(paste0(code_folder, 'populations.R'))

# Recorded Prevalence ####
# get prevalence data for each patient per year
prevalence_crude <- bind_rows(lapply(year_end_dates, prev_pl_df, df = index))

## Scotland ####
# Total, Age, Sex & Deprivation 

### Total #### 
#### Chart Data ####
scotland_total_chart_crude <- prevalence_crude %>%
  mutate(area = 'Scotland', 
         area_type = 'National') %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, area_type)) %>% 
  left_join(scot_pops) %>% 
  mutate(rate = (individuals/pop)*100000)

scotland_total_chart_crude <- prevalence_crude %>%
  mutate(area = 'Scotland', 
         area_type = 'National') %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, area_type)) %>% 
  left_join(scot_pops_all) %>% 
  mutate(rate = (individuals/pop)*100000)

scotland_total_chart_65_crude <- prevalence_crude %>%
  filter(age_group >= "65-69") %>% 
  mutate(area = 'Scotland',
         area_type = 'National',
         age_group = "65+") %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, area_type, age_group)) %>% 
  left_join(scot_pops65) %>% 
  mutate(rate = (individuals/pop)*100000)

chart_colours <- unname(phs_colour_values)
chart_annotation_colour <- 'white'

# All Age rate chart
ggplot(scotland_total_chart) +
  geom_line(aes(x = year, y = rate, group = area, colour = area),
           stat = 'identity', linewidth = 1) +
  
  ggtitle("People identified as living with dementia in Scotland",
          subtitle = "18 and over") +
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = chart_colours[1])  +
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 1500)) +
  
  theme_dash()

# 65+ rate chart
ggplot(scotland_total_chart_65) +
  geom_line(aes(x = year, y = rate, group = area, colour = area),
            stat = 'identity', linewidth = 1) +
  
  ggtitle("People identified as living with dementia in Scotland",
          subtitle = "Aged 65 and over") +
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = chart_colours[1])  +
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 6000)) +
  
  theme_dash()

# table data
# all ages
scotland_total_table <- scotland_total_chart %>%
  select(Year = year, Area = area, Count = individuals, Rate = rate)

scotland_rate_table <- scotland_total_table %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

scotland_count_table <- scotland_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)

# 65 plus
scotland_total_table65 <- scotland_total_chart_65 %>%
  select(Year = year, Area = area, Count = individuals, Rate = rate)

scotland_rate_table <- scotland_total_table %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

scotland_count_table <- scotland_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)

### Age Distribution ####
scotland_age_chart <- prevalence %>%
  mutate(area = 'Scotland') %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, age_group)) %>% 
  left_join(scot_age_pops, by = c("cal_year" = "year", "area", "age_group")) %>% 
  mutate(rate = (individuals/pop)*100000)

scotland_age_table <- scotland_age_chart %>%
  select(year = year, Area = area, "Age Group" = age_group, Count = individuals, Rate = rate)

# Trend - line plot
# ggplot(scotland_age_chart) +
#   geom_line(aes(x = year, y = rate, group = age_group, colour = age_group),
#             stat = 'identity', linewidth = 1) +
#   ggtitle("Count of people living with dementia at end of year by age group") + 
#   labs(x = 'Year', y = 'Individuals') +
#   scale_colour_manual(values = c(chart_colours[1:8]))  +
#   
#   scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#                      limits = c(0, 30000)) +
#   theme_dash()

# Latest year - bar plot
chart_scotland_age <- ggplot(scotland_age_chart %>% filter(year == max(year))) +
  geom_bar(mapping = aes(x = age_group, y = rate, colour = area, fill = area), stat = "identity") +
  labs(x = 'Age Group', y = 'Rate per 100,000') +
  ggtitle("People identified as living with dementia in Scotland by age group (2024)",
          subtitle = 'Rate per 100,000 population') +
  # scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ",")) +
  geom_text(aes(x = age_group, y = rate, label = prettyNum(round_half_up(rate, digits = 0), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 5, fontface = 'bold', vjust = -0.5) +
  scale_colour_manual(values = c(chart_colours[1], 'black')) +
  scale_fill_manual(values = c(chart_colours[1], 'black')) +
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 30000)) +
  
  theme_bar()

# ggplot(scotland_age_chart %>% filter(year == max(year)) %>% mutate(chart_sort = row_number()), 
#        aes(y = reorder(age_group, chart_sort))) +
#   geom_bar(mapping = aes(x = individuals, colour = area, fill = area), stat = "identity") +
#   labs(x = 'Individuals', y = 'Age Group') +
#   ggtitle("Count of people living with dementia at end of year by age group (2024)") +
#   scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#                      limits = c(0, 15000)) +
#   scale_colour_manual(values = chart_colours[1]) +
#   scale_fill_manual(values = chart_colours[1]) +
#   # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#   #                    limits = c(0, 15000)) +
#   
#   theme_bar()

### Sex ####
#### Chart Data ####
# 18 plus
scotland_sex_chart <- prevalence %>%
  mutate(area = 'Scotland') %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, sex)) %>% 
  left_join(scot_sex_pops, by = c("cal_year" = "year", "area", "sex")) %>% 
  mutate(rate = (individuals/pop)*100000)

scotland_sex_table <- scotland_sex_chart %>% 
  select(year, area, sex, individuals, rate)

# Count
# ggplot(scotland_sex_chart) +
#   geom_line(aes(x = year, y = individuals, group = sex, colour = sex),
#             stat = 'identity', linewidth = 1) +
#   ggtitle("Count of people living with dementia at end of year by sex") + #,
#   #         subtitle = paste0("Crude Rate per ", format(rate_multiplyer, big.mark = ","), ' population ', min(dd_age_dfs$`18-74`$fy), ' - ', max(dd_age_dfs$`18-74`$fy))) +
#   # labs(x = '', y = paste0('Rate per ', format(rate_multiplyer, big.mark = ","), ' population')) +
#   scale_colour_manual(values = c(chart_colours[1:2]))  +
#   
#   # use if phsstyles not installed
#   # scale_colour_manual(values = c('red', 'green', 'yellow', 'purple', 'blue', 'brown')) +
#   
#   scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#                      limits = c(0, 40000)) +
#   theme_dash()

# Rate
chart_scotland_sex <- ggplot(scotland_sex_chart) +
  geom_line(aes(x = year, y = rate, group = sex, colour = sex),
            stat = 'identity', linewidth = 1) +
  ggtitle("People living with dementia at end of year by sex",
          subtitle = 'Rate per 100,000 population') +
  labs(x = 'Year', y = 'Rate per 100,000') +
  scale_colour_manual(values = c(chart_colours[1:2]))  +
  
  scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 8000)) +
  theme_dash()

# 65 plus
scotland_sex_chart_65 <- prevalence %>%
  filter(age_group >= "65-69") %>% 
  mutate(area = 'Scotland') %>% 
  summarise(individuals = n(), .by = c(year, cal_year, area, sex)) %>% 
  left_join(scot_sex_pops_65, by = c("cal_year" = "year", "area", "sex")) %>% 
  mutate(rate = (individuals/pop)*100000)

scotland_sex_table_65 <- scotland_sex_chart_65 %>% 
  select(year, area, sex, individuals, rate)

## Health Board - Total ####
# 18 plus
hb_total_chart <- prevalence %>%
  filter(!is.na(hbres)) %>%
  rename(area = hbres) %>%
  summarise(area_type = 'Health Board',
            individuals = n(), .by = c(year, cal_year, area)) %>%
  arrange(area, year) %>%
  left_join(hb_pops65, by = c("cal_year" = "year", "area")) %>%
  mutate(rate = (individuals/pop)*100000) %>%
  bind_rows(scotland_total_chart_65)

hb_total_table <- hb_total_chart %>% 
  select(Year = year, Area = area, Count = individuals, Rate = rate)

hb_rate_table <- hb_total_table %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hb_count_table <- hb_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)

# ggplot(hb_total_chart) +
#   geom_line(aes(x = year, y = individuals, group = area, colour = area),
#             stat = 'identity', linewidth = 1) +
#   
#   ggtitle("People living with dementia at end of year",
#           subtitle = "Crude Rate per 100,000 population ") +
#   # labs(x = '', y = paste0('Rate per ', format(rate_multiplyer, big.mark = ","), ' population')) +
#   scale_colour_manual(values = chart_colours[1])  +
#   
#   scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#                      limits = c(0, 2000)) +
#   
#   theme_dash()

# Add annotations with values for bars
ggplot(hb_total_chart %>% filter(year == max(year)),
       aes(y = reorder(area, rate))) +
  geom_bar(mapping = aes(x = rate, colour = area_type, fill = area_type), stat = "identity") +
  geom_text(aes(x = rate, y = area, label = prettyNum(round_half_up(rate, digits = 1), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 3, hjust = 0.5, nudge_x = -200, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  ggtitle("People identified as living with dementia by Health Board (2024)",
          subtitle = 'Rate per 100,000 population') +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 6000)) +
  scale_colour_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +

  theme_bar()

# 65 plus
hb_total_chart_65 <- prevalence %>%
  filter(!is.na(hbres),
         age_group >= "65-69") %>%
  rename(area = hbres) %>%
  summarise(area_type = 'Health Board',
            individuals = n(), .by = c(year, cal_year, area)) %>%
  arrange(area, year) %>%
  left_join(hb_pops65, by = c("cal_year" = "year", "area")) %>%
  mutate(rate = (individuals/pop)*100000) %>%
  bind_rows(scotland_total_chart_65)

hb_total_table_65 <- hb_total_chart_65 %>% 
  select(Year = year, Area = area, Count = individuals, Rate = rate)

hb_rate_table_65 <- hb_total_table_65 %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hb_count_table_65 <- hb_total_table_65 %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)


## HSCP - Total ####
# hscp_total_chart <- prevalence %>% 
#   filter(!is.na(hscp2019name)) %>% 
#   rename(area = hscp2019name) %>% 
#   summarise(area_type = 'HSCP',
#             individuals = n(), .by = c(year, area)) %>% 
#   arrange(area, year) %>% 
#   left_join(hscp_pops65) %>% 
#   mutate(rate = (individuals/pop)*100000) %>% 
#   bind_rows(scotland_total_chart65)

# 18 plus 
hscp_total_chart <- prevalence %>% 
  filter(!is.na(hscp2019name)) %>% 
  rename(area = hscp2019name) %>% 
  summarise(area_type = 'HSCP',
            individuals = n(), .by = c(year, cal_year, area)) %>% 
  arrange(area, year) %>% 
  left_join(hscp_pops, by = c("cal_year" = "year", "area")) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  bind_rows(scotland_total_chart)

hscp_total_table <- hscp_total_chart %>%
  select(Year = year, Area = area, Count = individuals, Rate = rate)

hscp_rate_table <- hscp_total_table %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hscp_count_table <- hscp_total_table %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)

### Count #### 
# ggplot(hscp_total_chart %>% filter(year == max(year),
#                                    area != 'Scotland'),
#        aes(y = reorder(area, individuals))) +
#   geom_bar(mapping = aes(x = individuals, colour = area_type, fill = area_type), stat = "identity") +
#   labs(x = 'Individuals', y = 'HSCP') +
#   ggtitle("Count of people living with dementia at end of year by age group (2024)") +
#   scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#                      limits = c(0, 6000)) +
#   scale_colour_manual(values = chart_colours[1]) +
#   scale_fill_manual(values = chart_colours[1]) +
#   # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
#   #                    limits = c(0, 15000)) +
#   
#   theme_bar()

### Rate ####
ggplot(hscp_total_chart %>% filter(year == max(year)),
       aes(y = reorder(area, rate))) +
  geom_bar(mapping = aes(x = rate, colour = area_type, fill = area_type), stat = "identity") +
  geom_text(aes(x = rate, y = area, label = prettyNum(round_half_up(rate, digits = 1), big.mark = ",", big.interval = 3), colour = 'text_colour'), 
            size = 3, hjust = 0.5, nudge_x = -475, fontface = 'bold') +
  labs(x = 'Rate per 100,000', y = NULL) +
  ggtitle("People identified as living with dementia by HSCP (2024)",
          subtitle = 'Rate per 100,000 population (18 plus)') +
  scale_x_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
                     limits = c(0, 2000)) +
  scale_colour_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  scale_fill_manual(values = c(chart_colours[1:2], chart_annotation_colour)) +
  # scale_y_continuous(expand = c(0,0), labels = comma_format(big.mark = ","),
  #                    limits = c(0, 15000)) +
  
  theme_bar()

# 65 plus
hscp_total_chart_65 <- prevalence %>% 
  filter(!is.na(hscp2019name),
         age_group >= "65-69") %>% 
  rename(area = hscp2019name) %>% 
  summarise(area_type = 'HSCP',
            individuals = n(), .by = c(year, cal_year, area)) %>% 
  arrange(area, year) %>% 
  left_join(hscp_pops65, by = c("cal_year" = "year", "area")) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  bind_rows(scotland_total_chart_65)

hscp_total_table_65 <- hscp_total_chart_65 %>%
  select(Year = year, Area = area, Count = individuals, Rate = rate)

hscp_rate_table_65 <- hscp_total_table_65 %>% 
  select(-Count) %>% 
  pivot_wider(names_from = Year, values_from = Rate)

hscp_count_table_65 <- hscp_total_table_65 %>% 
  select(-Rate) %>% 
  pivot_wider(names_from = Year, values_from = Count)