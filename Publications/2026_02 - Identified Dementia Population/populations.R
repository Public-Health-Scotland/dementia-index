# population data (2019 onwards) ####
pops <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HSCP2019_pop_est_1981_2024.rds") %>% 
  filter(year >= 2019) %>% 
  rename(cal_year = year)

# hb_lookup
hb_lookup <- phslookups::get_spd(col_select = c('hscp2019name', 'hb2019name')) %>% 
  distinct()

## EASR Populations ####
pops_easr <- haven::read_sav("/conf/linkage/output/lookups/Unicode/Populations/Standard/ESP2013_by_sex.sav")

# for use in calculate_easr functions
easr_pops <- tibble(age_group = create_age_groups(seq(0, 91, by = 5), as_factor = TRUE)) %>% 
  mutate(epop = row_number())

### HSCP ####
hscp_pops_easr_18plus <- pops %>% 
  filter(age >= 18) %>% 
  mutate(age_group = create_age_groups(age, as_factor = TRUE)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hscp2019name, age_group, sex)) %>% 
  mutate(area = factor(hscp2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

### Health Boards ####
hb_pops_easr_18plus <- hscp_pops_easr_18plus %>% 
  left_join(hb_lookup, by = c("area" = 'hscp2019name')) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, hb2019name, age_group, sex)) %>% 
  mutate(area = factor(hb2019name, levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year)

### Scotland Total ####
scot_pops_easr_18plus <- hb_pops_easr_18plus %>% 
  mutate(area = factor("Scotland", levels = area_order, ordered = T)) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

### Scotland Age Groups ####
scot_age_pops_easr_18plus <- pops %>%
  filter(age >= 18) %>% 
  mutate(age_group = case_when(age < 60 ~ "18-59",
                               .default = create_age_groups(age, from = 60, to = 90, by = 5, as_factor = TRUE)),
         age_group = factor(age_group, levels = age_order, ordered = T)) %>% 
  mutate(area = factor('Scotland', levels = area_order, ordered = T),
         .keep = 'unused', .after = cal_year) %>% 
  summarise(pop = sum(pop),
            .by = c(cal_year, area, age_group, sex))

### Scotland SIMD pops ####
dz_pop <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2022.rds") %>% 
  rename(cal_year = year)

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

# use 2022 populations for missing 2023 data
simd_pops_easr_18plus %<>% 
  bind_rows(simd_pops_easr_18plus %>% 
              filter(cal_year == 2022) %>% 
              mutate(cal_year = 2023)) %>% 
  bind_rows(simd_pops_easr_18plus %>% 
              filter(cal_year == 2022) %>% 
              mutate(cal_year = 2024)) %>% 
  distinct()
