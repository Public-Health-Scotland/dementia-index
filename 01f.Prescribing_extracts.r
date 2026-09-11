#################################################
#01f.Prescribing information.r#####################
##################################################

#this file imports extracts from PIS 

###load in PIS and clean date formats - now one file in parquet format
# PIS data 2017/18 - 2024/25
extract_201718_202324 <- read_parquet("/PHI_conf/Dementia_Index/data/extracts/PIS/PIS_dementia_index_data_extract_201718-202425_IR2025-00954.parquet")

pis <- extract_201718_202324 %>% 
  mutate(pat_dob_clean = ymd(substr(`Pat Date of Birth [C]`, 1, 10)),
         presc_date_clean = ymd(substr(`Presc Date`, 1, 10)),
         pat_gender_code = case_match(`Pat Gender Description`,
                                      "Female" ~ 2, "Male" ~ 1,
                                      .default = NA)) %>%
  clean_names() %>%
  select(-presc_date, -pat_date_of_birth_c)

#table(pis$disp_financial_year_name)
##chi - UPI lookup####
clear_temp_tables(SMRAConnection)
upis <- SMRAConnection %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(pis %>% select(pat_upi_c) %>% unique() %>% rename(CHI_NUMBER = pat_upi_c), copy = TRUE) %>%
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER,UPI_NUMBER, DATE_OF_BIRTH) %>%
  distinct() %>% 
  collect()

pis <- pis  %>% left_join(upis, by = c("pat_upi_c" = "CHI_NUMBER"))

#table(is.na(pis$UPI_NUMBER), phsmethods::chi_check(pis$pat_upi_c))
#table(pis$UPI_NUMBER==pis$pat_upi_c, useNA="always")

##UPI and DOB from CHI table where avlaible
pis <- pis %>% 
  mutate(pat_upi_c = case_when(!is.na(UPI_NUMBER) ~ UPI_NUMBER, T~pat_upi_c)) %>% 
  mutate(pat_dob_clean = case_when(!is.na(DATE_OF_BIRTH) ~ as.Date(DATE_OF_BIRTH), T~pat_dob_clean))

##First valid record per person.  
pis <- pis  %>%
  #filter dates before Apr 2017
  filter(presc_date_clean >= as.Date("2017-04-01")) %>%
  ##remove missing and invalid chi
  mutate(valid_chi = phsmethods::chi_check(pat_upi_c)) %>%
  filter(valid_chi=="Valid CHI") %>%
  select(-valid_chi) %>%
  arrange(pat_upi_c, presc_date_clean) %>%
  ##group on upi and pick demographics
  group_by(pat_upi_c) %>% 
  summarise(presc_date = min_(presc_date_clean), 
            postcode = first_(pat_postcode_c),
            sex = first_(pat_gender_code),
            dob = first_(pat_dob_clean),
            ethnic_group = "09", # not available in PIS extract, data in previous PIS extracts is mainly 09, otherwise NA 
            CHI_dob = first_(DATE_OF_BIRTH))

pis <- pis %>%
  rename_with(.cols = everything(), function(x){paste0("pis_", x)}) %>% mutate(source="PIS")

saveRDS(pis , "/PHI_conf/Dementia_Index/data/cleaned_extracts/pis_clean.rds")
