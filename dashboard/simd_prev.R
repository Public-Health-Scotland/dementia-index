# # Create prevalence figures
# # Urban Rural Classification ####
# urc_2020_path <- '/conf/linkage/output/lookups/Unicode/Geography/Urban Rural Classification/postcode_urban_rural_2020.rds'
# urc_2022_path <- '/conf/linkage/output/lookups/Unicode/Geography/Urban Rural Classification/Postcode_urban_rural_2022.rds'
# urc_files <- c(urc_2020_path, urc_2022_path)
# 
# # 4 different breakdowns of rurality (2, 3, 6 & 8), will use the most descriptive - 8
# urc <- bind_rows(lapply(urc_files, readRDS)) %>% 
#   clean_names() %>% 
#   select(pc7, starts_with('ur6')) # %>% 
#   # this seems like it removes a lot of postcodes
# 
# colnames(urc)
# 
# # need to get all versions into 1 row for each postcode
# # so create a variable to hold ur version and its description
# # then widen so all versions on one row rather than 1 row per version
# urc_20_22 <- urc %>%
#   mutate(ur6_2022_name = case_when(is.na(ur6_2022_name) ~ NA,
#                                    .default = paste0(ur6_2022, " ", ur6_2022_name))) %>% 
#   select(-c(ur6_2020, ur6_2022)) %>% 
#   arrange(pc7) %>% 
#   pivot_longer(starts_with('ur6'), names_to = 'ur_version', values_to = 'ur_name') %>% 
#   filter(!is.na(ur_name)) %>% 
#   pivot_wider(names_from = ur_version, values_from = ur_name, values_fill = NA) %>% 
#   rename(postcode = pc7)



# Calculate prevalence
# instead of summarising the numbers, create a patient level df of 
# prevalence, will allow for easier breakdowns of urc and simd etc

# patient level prevalence data
prev_ca_source_first_pl <- bind_rows(lapply(dates, prev_pl_df, df = index_first)) %>% 
  rename(ur6_name = ur6_2022_name, ur8_name = ur8_2022_name)

# Between 175-275 missing uri each year, likewise with simd
prev_ca_source_first_pl %>% 
  filter(!is.na(postcode),
         is.na(ur6_name)) %>% 
  tabyl(year)

prev_ca_source_first_pl %>% 
  filter(!is.na(postcode),
         is.na(simd2020v2_sc_quintile)) %>% 
  tabyl(year)

## SIMD numbers w/o source ####
prevalence_ca_simd <- prev_ca_source_first_pl %>%
  filter(!is.na(simd2020v2_sc_quintile)) %>% 
  select(-source) %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, age_group, simd2020v2_sc_quintile)

prevalence_sc_simd <- prevalence_ca_simd %>%
  mutate(hscp2019name = 'Scotland') %>%
  summarise(individuals = sum(individuals),
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, age_group, simd2020v2_sc_quintile)

prevalence_all_simd <- prevalence_sc_simd %>%
  bind_rows(prevalence_ca_simd %>% filter(!is.na(hscp2019name))) %>%
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T))

prevalence_all_65plus_simd <- prevalence_all_simd %>%
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+") %>% 
  summarise(individuals = sum(individuals), 
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, simd2020v2_sc_quintile)

# All age groups but make it selectable by age group
# need to create a look up for the key
prevalence_all_simd %>% 
  summarise(individuals = sum(individuals),
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, age_group, simd2020v2_sc_quintile)


# can use rates for SIMD
dz_pop <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2022.rds")

# all ages
simd_pops <- dz_pop %>%
  filter(year >= 2017) %>% 
  select(year, hscp2019name, simd2020v2_sc_quintile, total_pop, age0:age90plus) %>% 
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange(desc(age)) %>%
  mutate(age_group = create_age_groups(age, from = 0, to = 90, by = 5, as_factor = TRUE),
         age_group = case_when(age_group < "60-64" ~ "0-59",
                               .default = as.character(age_group)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), 
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, age_group, simd2020v2_sc_quintile)

# 18+
simd_pops <- dz_pop %>%
  filter(year >= 2017) %>% 
  select(year, hscp2019name, simd2020v2_sc_quintile, total_pop, age18:age90plus) %>% 
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange(desc(age)) %>%
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), 
            .by = c(year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, year, age_group, simd2020v2_sc_quintile)

simd_pops_all <- simd_pops %>% 
  group_by(year, age_group, simd2020v2_sc_quintile) %>%
  nest() %>%
  mutate(data = map(data, ~ .x %>% adorn_totals("row", name = "Scotland"))) %>%
  unnest(cols = c(data)) %>% ungroup() %>% 
  relocate(hscp2019name, .before = age_group)

prevalence_all_simd_rate <- prevalence_all_simd %>% 
  rename(fin_year = year) %>% 
  mutate(year = as.numeric(substr(fin_year, 1, 4)),
         year = case_when(year > max(dz_pop$year) ~ max(dz_pop$year),
                          .default = year)) %>%
  left_join(simd_pops_all) %>% 
  mutate(rate_100000 = (individuals/pop)*100000,
         hscp2019name = factor(hscp2019name, levels = area_order, ordered = T)) %>% 
  select(-year) %>% rename(year = fin_year, area = hscp2019name)

prevalence_all_65plus_simd_rate <- prevalence_all_simd_rate %>% 
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+") %>% 
  summarise(individuals = sum(individuals), 
            pop = sum(pop),
            .by = c(year, area, age_group, simd2020v2_sc_quintile)) %>% 
  mutate(rate_100000 = (individuals/pop)*100000) %>% 
  arrange(area, year, simd2020v2_sc_quintile)


### SIMD Numbers table 65+ ####
simd_totals_65plus <- prevalence_all_65plus_simd_rate %>% 
  select(-c(age_group, pop, rate_100000)) %>% 
  pivot_wider(names_from = simd2020v2_sc_quintile,
              values_from = individuals, values_fill = 0) %>% 
  rename(`1 (Most Deprived)` = `1`, `5 (Least Deprived)` = `5`)

simd_65_cols <- colnames(simd_totals_65plus)[3:7]

### SIMD rate table 65+ ####
simd_rates_65plus <- prevalence_all_65plus_simd_rate %>% 
  select(-c(age_group, individuals, pop)) %>% 
  pivot_wider(names_from = simd2020v2_sc_quintile,
              values_from = rate_100000, values_fill = 0) %>% 
  rename(`1 (Most Deprived)` = `1`, `5 (Least Deprived)` = `5`)

# need these to have data for each simd quintile
# shared_simd_total_65plus <- SharedData$new(simd_totals_65plus, group = 'SIMD')

shared_simd_total_65plus <- SharedData$new(simd_totals_65plus %>% 
                                             mutate(key = paste0(year,area)), 
                                           key = ~key, group = "Group 4")

shared_simd_total_65plus_ch <- SharedData$new(
  simd_totals_65plus %>% 
    pivot_longer(cols = all_of(simd_65_cols), names_to = 'simd2020v2_sc_quintile', values_to = 'individuals') %>% 
    mutate(key = paste0(year,area)), 
  key = ~key, group = "Group 4"
)

shared_simd_rates_65plus <- SharedData$new(simd_rates_65plus %>% 
                                             mutate(key = paste0(year,area)), 
                                           key = ~key, group = "Group 4")

shared_simd_rates_65plus_ch <- SharedData$new(
  simd_rates_65plus %>% 
    pivot_longer(cols = all_of(simd_65_cols), names_to = 'simd2020v2_sc_quintile', values_to = 'rate') %>% 
    mutate(key = paste0(year,area)), 
  key = ~key, group = "Group 4"
)

### SIMD Numbers table All age ####
simd_totals <- prevalence_all_simd_rate %>% 
  select(-c(pop, rate_100000)) %>% 
  pivot_wider(names_from = age_group,
              values_from = individuals, values_fill = 0) %>% 
  mutate(simd2020v2_sc_quintile = case_when(simd2020v2_sc_quintile == 1 ~ "1 (Most Deprived)",
                                            simd2020v2_sc_quintile == 5 ~ "5 (Most Deprived)",
                                            .default = as.character(simd2020v2_sc_quintile)))

simd_totals_ch <- simd_totals %>%
  pivot_longer(cols = all_of(age_order), names_to = 'age_group', values_to = 'individuals')


### SIMD rate table All age ####
simd_rates <- prevalence_all_simd_rate %>% 
  select(-c(individuals, pop)) %>% 
  pivot_wider(names_from = age_group,
              values_from = rate_100000, values_fill = 0) %>% 
  mutate(simd2020v2_sc_quintile = case_when(simd2020v2_sc_quintile == 1 ~ "1 (Most Deprived)",
                                            simd2020v2_sc_quintile == 5 ~ "5 (Most Deprived)",
                                            .default = as.character(simd2020v2_sc_quintile)))

simd_rates_ch <- simd_rates %>% 
  pivot_longer(cols = all_of(age_order), names_to = 'age_group', values_to = 'rate')

shared_simd_total <- SharedData$new(simd_totals %>% 
                                      mutate(key = paste0(year,area)), 
                                    key = ~key, group = "Group 5")
shared_simd_total_ch <- SharedData$new(simd_totals_ch %>% 
                                         mutate(key = paste0(year,area)), 
                                       key = ~key, group = "Group 5")
shared_simd_rates <- SharedData$new(simd_rates %>% 
                                      mutate(key = paste0(year,area)), 
                                    key = ~key, group = "Group 5")
shared_simd_rates_ch <- SharedData$new(simd_rates_ch %>% 
                                         mutate(key = paste0(year,area)), 
                                       key = ~key, group = "Group 5")

## URC ####
urc_file <- readRDS("/conf/linkage/output/lookups/Unicode/Geography/Urban Rural Classification/DataZone2011_urban_rural_2020v2.rds")

dz_urc_pops_age <- dz_pop %>%
  filter(year >= 2017) %>% 
  select(year, datazone2011, hscp2019name, total_pop, age18:age90plus) %>% 
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange(desc(age)) %>%
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), 
            .by = c(year, datazone2011, hscp2019name, age_group)) %>% 
  arrange(hscp2019name, datazone2011, year, age_group) %>% 
  left_join(urc_file)

dz_urc_pops_age_sc <- dz_urc_pops_age %>% 
  select(-c(UR2_2020:UR3_2020_name, UR6_2020, UR8_2020)) %>% 
  mutate(hscp2019name = "Scotland") %>% 
  summarise(pop = sum(pop),
            .by = c(year, hscp2019name, age_group, UR6_2020_name, UR8_2020_name))

urc_pops <- dz_urc_pops_age_sc %>% 
  bind_rows(dz_urc_pops_age %>%
              select(-c(UR2_2020:UR3_2020_name, UR6_2020, UR8_2020)) %>%
              summarise(pop = sum(pop),
                        .by = c(year, hscp2019name, age_group, UR6_2020_name, UR8_2020_name))) %>% 
  rename(cal_year = year, area = hscp2019name, ur6_name = UR6_2020_name, ur8_name = UR8_2020_name) 

urc_pops <- urc_pops %>% 
  bind_rows(
    urc_pops %>% 
      filter(age_group >= "65-69") %>% 
      mutate(age_group = "65+") %>% 
      summarise(pop = sum(pop),
                .by = c(cal_year, area, age_group, ur6_name, ur8_name))) %>% 
  mutate(area = factor(area, levels = area_order, ordered = T))

ur6_pops <- urc_pops %>%
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, ur6_name))

ur8_pops <- urc_pops %>%
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, ur8_name))

# for using population now need to split into separate ur6 and 8 dfs
prevalence_ca_urc <- prev_ca_source_first_pl %>%
  filter(!is.na(ur6_name)) %>% 
  select(-source) %>% 
  summarise(individuals = n(),
            .by = c(year, hscp2019name, age_group, ur6_name, ur8_name)) %>% 
  arrange(hscp2019name, year, age_group, ur6_name)

prevalence_sc_urc <- prevalence_ca_urc %>%
  mutate(hscp2019name = 'Scotland') %>%
  summarise(individuals = sum(individuals),
            .by = c(year, hscp2019name, age_group, ur6_name, ur8_name)) %>% 
  arrange(hscp2019name, year, age_group, ur6_name)

prevalence_all_urc <- prevalence_sc_urc %>%
  bind_rows(prevalence_ca_urc %>% filter(!is.na(hscp2019name))) %>%
  mutate(hscp2019name = factor(hscp2019name, levels = area_order, ordered = T),
         cal_year = as.numeric(substr(year, 1, 4)),
         cal_year = case_when(cal_year > max(dz_pop$year) ~ max(dz_pop$year),
                              .default = cal_year)) %>% 
  rename(area = hscp2019name) %>% 
  left_join(urc_pops)

### 65+ urc data ####

#### UR6 ####
##### Numbers ####
urc6_65plus <- prevalence_all_urc %>%
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+") %>%
  summarise(individuals = sum(individuals),
            .by = c(year, area, age_group, ur6_name)) %>% 
  mutate(perc = individuals/sum(individuals),
         .by = c(year, area)) %>% 
  mutate(key = paste0(year,area))

urc6_65plus_table <- urc6_65plus %>%
  select(year, area, ur6_name, individuals) %>% 
  pivot_wider(names_from = ur6_name, 
              values_from = individuals, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

urc6_cols <- colnames(urc6_65plus_table)[3:8]
  
shared_urc6_65plus_ch <- SharedData$new(urc6_65plus, key = ~key, group = 'urc65')
shared_urc6_65plus_table <- SharedData$new(urc6_65plus_table, key = ~key, group = 'urc65')

##### Rate ####
urc6_65plus_rate <- urc6_65plus %>%
  mutate(cal_year = as.numeric(substr(year, 1, 4)),
         cal_year = case_when(cal_year > max(dz_pop$year) ~ max(dz_pop$year),
                              .default = cal_year)) %>%
  left_join(ur6_pops) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  select(-c(perc, cal_year)) %>% 
  relocate(key, .after = rate)

urc6_65plus_rate_table <- urc6_65plus_rate %>%
  select(year, area, ur6_name, rate) %>% 
  pivot_wider(names_from = ur6_name,
              values_from = rate, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

shared_urc6_65plus_rate_ch <- SharedData$new(urc6_65plus_rate, key = ~key, group = 'urc65')
shared_urc6_65plus_rate_table <- SharedData$new(urc6_65plus_rate_table, key = ~key, group = 'urc65')

#### UR8 ####
##### Numbers ####
urc8_65plus <- prevalence_all_urc %>%
  filter(age_group >= "65-69") %>%
  mutate(age_group = "65+") %>%
  summarise(individuals = sum(individuals),
            .by = c(year, area, age_group, ur8_name)) %>% 
  mutate(perc = individuals/sum(individuals),
         .by = c(year, area)) %>% 
  mutate(key = paste0(year,area)) %>% 
  arrange(year, area, age_group, ur8_name)

urc8_65plus_table <- urc8_65plus %>%
  select(year, area, ur8_name, individuals) %>% 
  pivot_wider(names_from = ur8_name, 
              values_from = individuals, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

urc8_cols <- colnames(urc8_65plus_table)[3:10]

shared_urc8_65plus_ch <- SharedData$new(urc8_65plus, key = ~key, group = 'urc65')
shared_urc8_65plus_table <- SharedData$new(urc8_65plus_table, key = ~key, group = 'urc65')

##### Rate ####
urc8_65plus_rate <- urc8_65plus %>%
  mutate(cal_year = as.numeric(substr(year, 1, 4)),
         cal_year = case_when(cal_year > max(dz_pop$year) ~ max(dz_pop$year),
                              .default = cal_year)) %>%
  left_join(ur8_pops) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  select(-c(perc, cal_year)) %>% 
  relocate(key, .after = rate)

urc8_65plus_rate_table <- urc8_65plus_rate %>%
  select(year, area, ur8_name, rate) %>% 
  pivot_wider(names_from = ur8_name,
              values_from = rate, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

shared_urc8_65plus_rate_ch <- SharedData$new(urc8_65plus_rate, key = ~key, group = 'urc65')
shared_urc8_65plus_rate_table <- SharedData$new(urc8_65plus_rate_table, key = ~key, group = 'urc65')

##### Percentage ####
# Just need the table here
urc8_65plus_perc_table <- urc8_65plus %>%
  select(year, area, ur8_name, perc) %>% 
  pivot_wider(names_from = ur8_name, 
              values_from = perc, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

shared_urc8_65plus_perc_table <- SharedData$new(urc8_65plus_perc_table, key = ~key, group = 'urc65')

### All age urc data ####

#### UR6 ####
##### Numbers ####
urc6_all <- prevalence_all_urc %>%
  summarise(individuals = sum(individuals),
            .by = c(year, area, age_group, ur6_name))

urc6_all_table <- urc6_all %>% 
  select(year, area, age_group, ur6_name, individuals) %>% 
  pivot_wider(names_from = age_group, 
              values_from = individuals, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

urc6_all <- urc6_all_table %>%
  pivot_longer(cols = all_of(age_order), names_to = 'age_group', values_to = 'individuals') %>% 
  mutate(perc = individuals/sum(individuals),
         .by = c(year, area, ur6_name)) %>% 
  mutate(key = paste0(year,area)) 

shared_urc6_all_ch <- SharedData$new(urc6_all, key = ~key, group = 'urc_all')
shared_urc6_all_table <- SharedData$new(urc6_all_table, key = ~key, group = 'urc_all')

##### Rate ####
urc6_all_rate <- urc6_all %>%
  mutate(cal_year = as.numeric(substr(year, 1, 4)),
         cal_year = case_when(cal_year > max(dz_pop$year) ~ max(dz_pop$year),
                              .default = cal_year)) %>%
  left_join(ur6_pops) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  select(-c(perc, cal_year)) %>% 
  relocate(key, .after = rate)

urc6_all_rate_table <- urc6_all_rate %>%
  select(year, area, age_group, ur6_name, rate) %>% 
  pivot_wider(names_from = age_group,
              values_from = rate, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

shared_urc6_all_rate_ch <- SharedData$new(urc6_all_rate, key = ~key, group = 'urc_all')
shared_urc6_all_rate_table <- SharedData$new(urc6_all_rate_table, key = ~key, group = 'urc_all')

#### UR8 ####
##### Numbers ####
urc8_all <- prevalence_all_urc %>%
  summarise(individuals = sum(individuals),
            .by = c(year, area, age_group, ur8_name))

urc8_all_table <- urc8_all %>% 
  select(year, area, age_group, ur8_name, individuals) %>% 
  pivot_wider(names_from = age_group, 
              values_from = individuals, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

urc8_all <- urc8_all_table %>%
  pivot_longer(cols = all_of(age_order), names_to = 'age_group', values_to = 'individuals') %>% 
  mutate(perc = individuals/sum(individuals),
         .by = c(year, area, ur8_name)) %>% 
  mutate(key = paste0(year,area)) 

shared_urc8_all_ch <- SharedData$new(urc8_all, key = ~key, group = 'urc_all')
shared_urc8_all_table <- SharedData$new(urc8_all_table, key = ~key, group = 'urc_all')

##### Rate ####
urc8_all_rate <- urc8_all %>%
  mutate(cal_year = as.numeric(substr(year, 1, 4)),
         cal_year = case_when(cal_year > max(dz_pop$year) ~ max(dz_pop$year),
                              .default = cal_year)) %>%
  left_join(ur8_pops) %>% 
  mutate(rate = (individuals/pop)*100000) %>% 
  select(-c(perc, cal_year)) %>% 
  relocate(key, .after = rate)

urc8_all_rate_table <- urc8_all_rate %>%
  select(year, area, age_group, ur8_name, rate) %>% 
  pivot_wider(names_from = age_group,
              values_from = rate, values_fill = 0) %>% 
  mutate(key = paste0(year, area))

shared_urc8_all_rate_ch <- SharedData$new(urc8_all_rate, key = ~key, group = 'urc_all')
shared_urc8_all_rate_table <- SharedData$new(urc8_all_rate_table, key = ~key, group = 'urc_all')
