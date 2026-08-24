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
## TODO: CHI -> UPI resolution removed - UPIP access withdrawn.
## The UPIP.L_UPI_DATA lookup previously replaced pat_upi_c with the master
## UPI and overwrote pat_dob_clean with the CHI-database DATE_OF_BIRTH. Until
## a replacement lookup exists the PIS-supplied pat_upi_c and pat_dob_clean
## are used as-is.

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
            ethnic_group = "09" # not available in PIS extract, data in previous PIS extracts is mainly 09, otherwise NA 
            )

pis <- pis %>%
  rename_with(.cols = everything(), function(x){paste0("pis_", x)}) %>% mutate(source="PIS")

saveRDS(pis , "/PHI_conf/Dementia_Index/data/cleaned_extracts/pis_clean.rds")
