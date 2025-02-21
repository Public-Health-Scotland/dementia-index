#################################################
#01f.Prescribing information.r#####################
##################################################

#this file imports extracts from PIS and combines with extracts from HEPMA and homecare. 

###load in PIS and clean date formats
###Still missing 2014/15 to 2016/17 as BO having problems with queries.
##run and cleanr the rest and just rerun later with early years added

extract_20171819 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_20171819.csv")
extract_20171819 <- extract_20171819  %>%
  mutate(pat_dob_clean = as.Date(substr(`Pat Date of Birth [C]`,1,10),"%Y/%m/%d")) %>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%Y/%m/%d")) 
#head(extract_20171819$`Pat Date of Birth [C]`)

summary(extract_20171819$pat_dob_clean)
summary(extract_20171819$presc_date_clean)

##year first
extract_201920 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_201920.csv")
head(extract_201920$`Pat Date of Birth [C]`)
extract_201920 <- extract_201920  %>%
  mutate(pat_dob_clean = as.Date(substr(`Pat Date of Birth [C]`,1,10),"%Y/%m/%d"))%>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%Y/%m/%d")) 

summary(extract_201920 $pat_dob_clean)
summary(extract_201920 $presc_date_clean)

##year last
extract_202021 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202021.csv")
extract_202021 <- extract_202021  %>% 
  mutate(pat_dob_clean = as.Date(substr(`Pat Date of Birth [C]`,1,10),"%d/%m/%Y"))%>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%d/%m/%Y")) 

head(extract_202021$`Presc Date`)
summary(extract_202021$pat_dob_clean)
summary(extract_202021$presc_date_clean)

extract_202122 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202122.csv")
head(extract_202122$`Pat Date of Birth [C]`)
extract_202122 <- extract_202122  %>%
  mutate(pat_dob_clean = as.Date(substr(`Pat Date of Birth [C]`,1,10),"%Y/%m/%d")) %>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%Y/%m/%d"))
summary(extract_202122$pat_dob_clean)
summary(extract_202122$presc_date_clean)
table(substr(extract_202122$`Pat Date of Birth [C]`,7,10))



extract_202223 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202223.csv")
table(substr(extract_202223$`Pat Date of Birth [C]`,7,10))
head(extract_202223$`Pat Date of Birth [C]`)
extract_202223  <-extract_202223   %>%
  mutate(pat_dob_clean= as.Date(substr(`Pat Date of Birth [C]`,1,10),"%d/%m/%Y")) %>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%d/%m/%Y")) 
  
summary(extract_202223$presc_date_clean)

extract_202324 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202324.csv")
head(extract_202324$`Pat Date of Birth [C]`)
extract_202324   <-extract_202324    %>%
  mutate(pat_dob_clean= as.Date(substr(`Pat Date of Birth [C]`,1,10),"%d/%m/%Y"))%>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%d/%m/%Y")) 
summary(extract_202324$presc_date_clean)

extract_202425 <- read_csv("/PHI_conf/Dementia_Index/data/extracts/PIS/extract_202425.csv")
head(extract_202425$`Pat Date of Birth [C]`)
extract_202425  <-extract_202425     %>%
  mutate(pat_dob_clean= as.Date(substr(`Pat Date of Birth [C]`,1,10),"%d/%m/%Y"))%>%
  mutate(presc_date_clean = as.Date(substr(`Presc Date`,1,10),"%d/%m/%Y")) 
summary(extract_202425 $pat_dob_clean)
summary(extract_202425 $presc_date_clean)

###rbindall
pis <- bind_rows(extract_20171819, extract_201920, extract_202021,extract_202122, 
                 extract_202223, extract_202425, extract_202324)%>%
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
            ethnic_group = first_(pat_ethnic_group_code), 
            CHI_dob = first_(DATE_OF_BIRTH))

pis <- pis %>%
  rename_with(.cols = everything(), function(x){paste0("pis_", x)}) %>% mutate(source="PIS")

saveRDS(pis , "/PHI_conf/Dementia_Index/data/extracts/pis_clean.rds")
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
                              patient_sex, patient_sex_desc, patient_postcode,
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
                            AND dmd_bnf_code LIKE '0411%'") %>%
  mutate(UPI_validity = chi_check(patient_chi_number)) %>%
  # Drop records without a valid UPI
  filter(!is.na(patient_chi_number) & UPI_validity == "Valid CHI") %>%
  select(-UPI_validity)


##2) homecare  extract
hcm_extract <- dbGetQuery(dv, "SELECT patient_chi_number, patient_upi_number, patient_date_of_birth, patient_sex, patient_sex_desc,
                                patient_date_of_death, patient_postcode, supply_date,
                                dmd_bnf_code, dmd_code,
                                dmd_vmp_name, dmd_vtm_name, dmd_atc_code, dmd_atc_description,
                                medication_name_submitted, supply_quantity, dmd_ddd_conversion_factor,
                                treatment_health_board_name,
                                supply_id, sending_location_name, record_id, supply_unique_id
                            FROM hcm.hcm_supply_analysis
                            WHERE supply_date <= '2024-12-31'
                             AND dmd_bnf_code LIKE '0411%'") %>%
  mutate(UPI_validity = chi_check(patient_chi_number)) %>%
  # Drop records without a valid UPI
  filter(!is.na(patient_chi_number) & UPI_validity == "Valid CHI") %>%
  select(-UPI_validity)


##3) combine extracts


