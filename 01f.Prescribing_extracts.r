#################################################
#01f.Prescribing information.r#####################
##################################################

#this file imports extracts from PIS and combines with extracts from HEPMA and homecare. 

###load in PIS
###just a couple for now to get code to run
extract_202324 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202324.csv")
extract_202425 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202425.csv")

###rbindall
pis <- bind_rows(extract_202425, extract_202324)%>%
  clean_names()

##chi - UPI lookup####
clear_temp_tables(SMRAConnection)
upis <- SMRAConnection %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(pis %>% select(pat_upi_c) %>% unique() %>% rename(CHI_NUMBER = pat_upi_c), copy = TRUE) %>%
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER,UPI_NUMBER, DATE_OF_BIRTH) %>%
  distinct() %>% 
  collect()

pis <- pis  %>% left_join(upis, by = c("pat_upi_c" = "CHI_NUMBER"))

table(is.na(pis$UPI_NUMBER), phsmethods::chi_check(pis$pat_upi_c))
table(pis$UPI_NUMBER==pis$pat_upi_c, useNA="always")

pis <- pis %>% mutate(pat_upi_c = case_when(!is.na(UPI_NUMBER) ~ UPI_NUMBER, T~pat_upi_c))
  
pis <- pis  %>%
  arrange(pat_upi_c, presc_date) %>%
  group_by(pat_upi_c) %>% 
  summarise(presc_date = min_(presc_date), 
            postcode = first_(pat_postcode_c),
            sex = first_(pat_gender_code),
            dob = first_(pat_date_of_birth_c),
            ethnic_group = first_(pat_ethnic_group_code))

pis <- pis %>%
  rename_with(.cols = everything(), function(x){paste0("pis_", x)})

saveRDS(pis , "/PHI_conf/Dementia_Index/data/extracts/pis_test.rds")
##hepma and homecare extracts
## Open Connection 
dv <- dbConnect(odbc(), dsn = "DVPROD", 
                uid = Sys.info()[["user"]], 
               
                pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))

hepma_cols <- dbGetQuery(dv, "SELECT *
                            FROM hepma.hepma_administration_prescription_analysis
                            WHERE 1=2") 
# 1) Extract HEPMA data from database -------------------------------------

# HEPMA extract
# Relevant filters:
# Time period: 
# Gender: 
# BNF section: 
# Adapt the code below to extract the data required;
# VS - Added in UPI and CHI just incase, also added in formulation for use in DDD calculations, removed filter for sex
hepma_extract <- dbGetQuery(dv, "SELECT patient_chi_number, patient_upi_number, patient_date_of_birth,
                              patient_sex, patient_sex_desc, patient_date_of_death, patient_postcode,
                              presc_start_date_time, 
                              prescription_has_no_associated_admin, admin_not_given, admin_reason_not_given,
                              dmd_bnf_code,
                              dmd_code, dmd_vmp_name, dmd_vtm_name, dmd_atc_code,
                              dmd_atc_code_description, medication_name,
                             treatment_health_board_name,
                              presc_unique_id, admin_unique_id
                            FROM hepma.hepma_administration_prescription_analysis
                            WHERE admin_given_date_time >= '2014-01-01'
                            AND admin_given_date_time <= '2025-01-31'
                            AND treatment_health_board_name NOT IN ('STATE HOSPITAL')
                            AND (dmd_bnf_code LIKE '0411%'") %>%
  mutate(UPI_validity = chi_check(patient_chi_number)) %>%
  # Drop records without a valid UPI
  filter(!is.na(patient_chi_number) & UPI_validity == "Valid CHI") %>%
  select(-UPI_validity)


##2) homecare  extract


##3) combine extracts


