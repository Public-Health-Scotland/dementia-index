# population data (2019 onwards) ####
pops <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HSCP2019_pop_est_1981_2024.rds") %>% 
  filter(year >= 2019) %>% 
  rename(cal_year = year)

## HSCP and HB populations ####
hb_lookup <- phslookups::get_spd(col_select = c('hscp2019name', 'hb2019name')) %>% 
  distinct()

hscp_pops <- pops %>%
  filter(age >= 18) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name)) %>%
  mutate(area = factor(hscp2019name, levels = area_order, ordered = T),
         .keep = 'unused', .before = pop)

hscp_pops65 <- pops %>% 
  filter(age >= 65) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name)) %>% 
  mutate(area = factor(hscp2019name, levels = area_order, ordered = T),
         .keep = 'unused', .before = pop)

hb_pops <- hscp_pops %>%
  left_join(hb_lookup, by = c("area" = "hscp2019name")) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hb2019name)) %>%
  mutate(area = factor(hb2019name, levels = area_order, ordered = T),
         .keep = 'unused', .before = pop)

hb_pops65 <- hscp_pops65 %>% 
  left_join(hb_lookup, by = c("area" = "hscp2019name")) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hb2019name)) %>%
  mutate(area = factor(hb2019name, levels = area_order, ordered = T),
         .keep = 'unused', .before = pop)

## Scotland populations ####
scot_pops <- hb_pops %>%
  mutate(area = factor('Scotland', levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop), .by = c(cal_year, area))

scot_pops_all <- pops %>%
  # filter(age >= 18) %>% 
  mutate(area = factor("Scotland", levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area))

scot_pops65 <- hb_pops65 %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop), .by = c(cal_year, area))

scot_dem_pops <- pops %>%
  filter(age >= 18) %>% 
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T),
         area = factor('Scotland', levels = area_order, ordered = T))

scot_age_pops <- scot_dem_pops %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group))

scot_sex_pops <- scot_dem_pops %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, sex_name)) %>% 
  mutate(sex = case_when(sex_name == 'F' ~ 'Female',
                         .default = 'Male'),
         .keep = 'unused', .before = pop)

scot_sex_pops_65 <- scot_dem_pops %>% 
  filter(age_group >= "65-69") %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, sex_name)) %>% 
  mutate(sex = case_when(sex_name == 'F' ~ 'Female',
                         .default = 'Male'),
         .keep = 'unused', .before = pop)

## EASR Populations ####
pops_easr <- haven::read_sav("/conf/linkage/output/lookups/Unicode/Populations/Standard/ESP2013_by_sex.sav")
easr_pops <- tibble(age_group = create_age_groups(seq(0, 91, by = 5), as_factor = TRUE)) %>% 
  mutate(epop = row_number())

hscp_pops_easr <- pops %>% 
  mutate(age_group = create_age_groups(age, as_factor = TRUE)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name, age_group, sex)) %>% 
  mutate(area = factor(hscp2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

hscp_pops_easr_18plus <- pops %>% 
  filter(age >= 18) %>% 
  mutate(age_group = create_age_groups(age, as_factor = TRUE)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name, age_group, sex)) %>% 
  mutate(area = factor(hscp2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

hb_pops_easr <- hscp_pops_easr %>% 
  left_join(hb_lookup, by = c("area" = 'hscp2019name')) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hb2019name, age_group, sex)) %>% 
  mutate(area = factor(hb2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

hb_pops_easr_18plus <- hscp_pops_easr_18plus %>% 
  left_join(hb_lookup, by = c("area" = 'hscp2019name')) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hb2019name, age_group, sex)) %>% 
  mutate(area = factor(hb2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

scot_pops_easr <- hb_pops_easr %>% 
  mutate(area = factor("Scotland", levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

scot_pops_easr_18plus <- hb_pops_easr_18plus %>% 
  mutate(area = factor("Scotland", levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

scot_age_pops_easr <- pops %>%
  mutate(age_group = case_when(age < 60 ~ "Under 59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order2, ordered = T)) %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

scot_age_pops_easr_18plus <- pops %>%
  filter(age >= 18) %>% 
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

# scot_age_pops_easr <- scot_dem_pops %>% 
#   summarise(pop = sum(pop),
#             .by = c(cal_year, area, age_group, sex))

## Scotland SIMD pops ####
dz_pop <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2022.rds") %>% 
  rename(cal_year = year)

# 18+
simd_pops <- dz_pop %>%
  filter(cal_year >= 2017) %>% 
  select(cal_year, hscp2019name, simd2020v2_sc_quintile, total_pop, age18:age90plus) %>% 
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange(desc(age)) %>%
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  summarise(pop = sum(pop), 
            .by = c(cal_year, hscp2019name, age_group, simd2020v2_sc_quintile)) %>% 
  arrange(hscp2019name, cal_year, age_group, simd2020v2_sc_quintile)

simd_pops_all <- simd_pops %>% 
  group_by(cal_year, age_group, simd2020v2_sc_quintile) %>%
  nest() %>%
  mutate(data = map(data, ~ .x %>% adorn_totals("row", name = "Scotland"))) %>%
  unnest(cols = c(data)) %>% ungroup() %>% 
  relocate(hscp2019name, .before = age_group)

# 65+
simd_pops_all_65 <- simd_pops_all %>%
  filter(age_group >= "65-69") %>% 
  mutate(age_group = "65+") %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name, age_group, simd2020v2_sc_quintile))

# all ages easr
simd_pops_easr <- dz_pop %>%
  filter(cal_year > 2019) %>%
  select(cal_year, hscp2019name, simd2020v2_sc_quintile, age0:age90plus, sex) %>%
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange((age)) %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T),
         age_group = create_age_groups(age, as_factor = T),
         sex = case_match(sex,
                          'M' ~ 1,
                          .default = 2)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex, simd2020v2_sc_quintile)) %>% 
  arrange(area, cal_year, age_group, simd2020v2_sc_quintile, sex)

# use 2022 populations for missing 2023 data
simd_pops_easr %<>% 
  bind_rows(simd_pops_easr %>% 
              filter(cal_year == 2022) %>% 
              mutate(cal_year = 2023)) %>% distinct()

simd_pops_easr_18plus <- dz_pop %>%
  filter(cal_year > 2019) %>%
  select(cal_year, hscp2019name, simd2020v2_sc_quintile, age18:age90plus, sex) %>%
  pivot_longer(cols = starts_with('age'), names_to = "age",
               # names_pattern = "age(\\d+)",
               names_transform = list(age = ~ as.numeric(gsub("age|plus", "", .x))),
               values_to = 'pop') %>% arrange((age)) %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T),
         age_group = create_age_groups(age, as_factor = T),
         sex = case_match(sex,
                          'M' ~ 1,
                          .default = 2)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex, simd2020v2_sc_quintile)) %>% 
  arrange(area, cal_year, age_group, simd2020v2_sc_quintile, sex)

simd_pops_easr_18plus %<>% 
  bind_rows(simd_pops_easr_18plus %>% 
              filter(cal_year == 2022) %>% 
              mutate(cal_year = 2023)) %>% distinct()
