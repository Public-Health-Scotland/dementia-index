######################################
## Script 02. Join extracts ##########
######################################

###Step 1 load data and use CHI database to convert CHI to UPIs
## 
smr_all<- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/smr_first_icd10.rds") %>% ungroup()
PDS <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/PDS_clean.rds") %>% ungroup()
dementia_deaths <- readRDS("/PHI_conf/Dementia_Index/data/extracts/dementia_deaths.rds")%>% ungroup()
CHC <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/CHC_clean.rds")%>% ungroup()
social_care <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/SC_clean.rds")%>% ungroup()
PIS <- readRDS("/PHI_conf/Dementia_Index/data/cleaned_extracts/pis_clean.rds")%>% ungroup()

####CHI to UPI ####
##SMRA sources should be OK.
names(PDS)
clear_temp_tables(SMRAConnection)
##test using pds
upis <- SMRAConnection %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(CHC %>% rename(CHI_NUMBER = CHC_UPI_NUMBER), copy = TRUE) %>%
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER,UPI_NUMBER, DATE_OF_BIRTH, CURRENT_POSTCODE, PREVIOUS_POSTCODE, DATE_ADDRESS_CHANGED) %>%
  distinct() %>% 
  collect() %>% 
  rename(upi = CHI_NUMBER,
         chi_dob = DATE_OF_BIRTH, 
         chi_current_postcode = CURRENT_POSTCODE, 
         chi_previous_postcode = PREVIOUS_POSTCODE, 
         chi_date_postcode_change = DATE_ADDRESS_CHANGED) %>% 
  mutate(chi_dob = as_date(chi_dob))

##Clean source names and select minimal required demographics####
##reame for consistency
code_list <- read_csv("/PHI_conf/Dementia_Index/data/code_list.csv")
smr <- smr_all %>% ungroup() %>% 
  mutate(sex = as.numeric(sex)) %>%
  mutate(diagnosis_description = case_when(substr(diagnosis,1,3)=="F03"~ "Unspecified dementia",
                                           diagnosis=="F028 A G318 D" ~ "Lewy body dementia",
                                           substr(diagnosis,1,4)=="F000"~ "Dementia in Alzheimer disease with early onset", 
                                           substr(diagnosis,1,4)=="F001"~ "Dementia in Alzheimer disease with late onset", 
                                           substr(diagnosis,1,4)=="F002"~ "Dementia in Alzheimer disease, atypical or mixed type ", 
                                           substr(diagnosis,1,4)=="F009"~ "Dementia in Alzheimer disease, unspecified",
                                           substr(diagnosis,1,3)=="F00"~ "Dementia in Alzheimer disease", 
                                           substr(diagnosis,1,4)=="F010"~ "Vascular dementia of acute onset",   
                                           substr(diagnosis,1,4)=="F011"~ "Multi-infarct dementia ", 
                                           substr(diagnosis,1,4)=="F012"~ "Subcortical vascular dementia  ", 
                                           substr(diagnosis,1,4)=="F013"~ "Mixed cortical and subcortical vascular dementia",
                                           substr(diagnosis,1,4)=="F018"~ "Other vascular dementia",
                                           substr(diagnosis,1,4)=="F019"~ "vascular dementia, unspecified",
                                           substr(diagnosis,1,3)=="F01"~ "Vascular dementia",  
                                           substr(diagnosis,1,4)=="F020"~ "Dementia in Pick disease",   
                                           substr(diagnosis,1,4)=="F021"~ " Dementia in Creutzfeldt-Jakob disease", 
                                           substr(diagnosis,1,4)=="F022"~ " Dementia in Huntington disease", 
                                           substr(diagnosis,1,4)=="F023"~ " Dementia in Parkinson disease ",
                                           substr(diagnosis,1,4)=="F024"~"Dementia in human immunodeficiency virus [HIV] disease ", 
                                           substr(diagnosis,1,4)=="F029"~ "Dementia in other specified diseases classified elsewhere",
                                           diagnosis =="F02"~ "Dementia in other diseases classified elsewhere",
                                           substr(diagnosis,1,3)=="F02"~ "Dementia in diseases classified elsewhere",   
                                           substr(diagnosis,1,5)=="F1073"~ "Dementia due to alcohol use", 
                                           substr(diagnosis,1,5)=="F1573"~ "Dementia due to use of other stimulants", 
                                           substr(diagnosis,1,4)=="F051"~ "Delirium superimposed on dementia")) %>% 
  rename(diagnosis_date = admission_date)%>%
  mutate(date_type="hospital admission date")


names(smr)
PIS <- PIS %>% mutate(diagnosis_description = "prescription from BNF ch4.11") %>%
  rename(upi_number= pis_pat_upi_c,
         diagnosis_date = pis_presc_date, 
         postcode=pis_postcode,
         sex = pis_sex, 
         dob = pis_dob,
         ethnic_group = pis_ethnic_group) %>%
  mutate(date_type="date_prescribed") %>% select(-pis_CHI_dob)

names(dementia_deaths)
PDS <- PDS %>%
  mutate(sex = case_when(pds_sex=="01 Male" ~1, 
                         pds_sex=="02 Female" ~2 )) %>%
  rename(upi_number= pds_chi_number,
         diagnosis_date = pds_diagnosis_date,
         diagnosis_description = pds_dementia_subtype,
         postcode=pds_postcode,
         dob = pds_date_of_birth,
         ethnic_group = pds_ethnic_group) %>%
  mutate(date_type="pds_date") %>% select(-pds_sex, -pds_source) %>% mutate(source="PDS")

social_care <- social_care %>%
  mutate(sex = case_when(chi_gender=="M" ~ 1,chi_gender=="F" ~2, T~9 )) %>%
  rename(dob = chi_date_of_birth, 
         ethnic_group = submitted_ethnic_group, 
         postcode = best_postcode, 
         diagnosis_description = dementia_type
  ) %>% select(-c(chi_gender, DATE_OF_BIRTH, social_care_id,financial_year,
                  quarter_client,financial_quarter_care_first, quarter_client_date,
                  dementia, quarter_fin_date, year_client_date, submitted_postcode)) %>%
  mutate(source="social care")


CHC <- CHC %>%
  mutate(sex = case_when(CHC_Sex=="Male" ~ 1,CHC_Sex=="Female" ~2, T~9 )) %>%
  rename(upi_number = CHC_UPI_NUMBER, 
         diagnosis_date = CHC_first_admission, 
         dob = CHC_DateOfBirth, 
         ethnic_group = CHC_EthnicOriginCode, 
         postcode= CHC_CareHomePostcode, 
         diagnosis_description =CHC_dementia_subtype
  ) %>% mutate(date_type = "date of carehome admission")%>%
  select(-CHC_Sex)%>% mutate(source="Care home census")

##bind sources long ####
##except dementia deaths#
df <- bind_rows(smr, PIS,PDS,CHC,social_care)
df <- df %>% select(upi_number, diagnosis_date, diagnosis, diagnosis_description, source,
                    dob, sex, postcode, ch_postcode, everything())
names(df)

##identify unlinked deaths in dementia deaths####
upi_list <- unique(df$upi_number)
unlinked_deaths <- dementia_deaths %>% filter(!upi_number %in% upi_list)
names(unlinked_deaths)
##extract nrs date of death####
death_start_date <- as.Date("2014-01-01")
##identify any duplicate deaths before linking to main list. 
deaths_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "SELECT UPI_NUMBER,CHI, DATE_OF_BIRTH, DATE_OF_DEATH,
     YEAR_OF_REGISTRATION , REGISTRATION_DISTRICT, ENTRY_NUMBER ,
     SEX, POSTCODE,HEALTH_BOARD_AREA
    FROM ANALYSIS.GRO_DEATHS_C SMR
    WHERE DATE_OF_DEATH >= TO_DATE('", death_start_date, "', 'yyyy-mm-dd')
    ")
  )) %>% clean_names()

#table(is.na(deaths_temp_1$chi) , is.na(deaths_temp_1$upi_number))
###use chi if upi unavlaible, remove those with missing chi and upi
deaths_temp_1 <- deaths_temp_1 %>% mutate(deaths_upi = case_when(is.na(upi_number)~ chi , T~upi_number)) %>%
  select(-chi, -upi_number)  

death_upi <- SMRAConnection %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(deaths_temp_1 %>% select(deaths_upi) %>% rename(CHI_NUMBER = deaths_upi), copy = TRUE) %>%
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER,UPI_NUMBER) %>%
  distinct() %>% 
  collect()
##se
deaths_temp_1 <- deaths_temp_1 %>% left_join(death_upi, by = c("deaths_upi" = "CHI_NUMBER"))
#table(deaths_temp_1$deaths_upi==deaths_temp_1$UPI_NUMBER, useNA="always")

##update the upi and remove records where no upi or chi can be found
deaths_temp_1 <- deaths_temp_1 %>% mutate(deaths_upi=case_when(is.na(UPI_NUMBER)~ deaths_upi, T~UPI_NUMBER)) %>%
  filter(!is.na(deaths_upi))
length(unique(deaths_temp_1$deaths_upi))
nrow(deaths_temp_1)

##still a few duplicate records
deaths_temp_1<- deaths_temp_1 %>% group_by(deaths_upi) %>%
  mutate(n_chi = n()) %>% ungroup()

dups <- deaths_temp_1 %>% filter(n_chi>1)

##some look like duplicate registrations -
#same date of death and consecutive reg numbers, can use these still just slice(1)
dups <- dups %>% group_by(deaths_upi) %>% 
  mutate(dup_reg = case_when(min(date_of_death)==max(date_of_death) ~1, T~0)) %>%
  ungroup()

dups_fix <- dups %>% filter(dup_reg==1) %>%
  group_by(deaths_upi) %>% slice(1)

deaths_temp_1 <- deaths_temp_1 %>% filter(!(deaths_upi %in%  dups$deaths_upi)) 

deaths <- bind_rows(deaths_temp_1, dups_fix) %>%
  select(deaths_upi, date_of_death, date_of_birth) %>%
  rename(death_dob = date_of_birth)
summary(df$diagnosis_date)

deaths<- deaths %>% group_by(deaths_upi) %>%
  mutate(n_chi = n()) %>% ungroup()

dups <- deaths_temp_1 %>% filter(n_chi>1)

#dups_in_df <- dups %>% filter(deaths_upi %in% df$upi_number)
#3 people and one is a duplicate reg,so only missing if we just drop them 

###left join to the index chi list
df_linked <- df %>% filter(diagnosis_date>=as.Date("2010-01-01") )%>%
  left_join(deaths, by= c("upi_number" = "deaths_upi"))


###find dementia deaths not linked to other sources
##
unlinked_dementia_deaths <- dementia_deaths %>%
  filter(!upi_number %in% df_linked$upi_number)
names(unlinked_dementia_deaths)
#str(unlinked_dementia_deaths$sex)
#str(df_linked$sex)
unlinked_dementia_deaths <- unlinked_dementia_deaths  %>%
  mutate(diagnosis_date = date_of_death)%>%
  rename(diagnosis = icd10_1 ,
         diagnosis_2 = icd10_2 ,
         diagnosis_3 = icd10_3 ,
         diagnosis_4 = icd10_4 ,
         ethnic_group = ethnicity_code,
         diagnosis_description = dementia_subtype_1,
         diagnosis_description2 = dementia_subtype_2,
         dob = date_of_birth
         )%>%
  mutate(sex = as.numeric(sex)) %>%
    mutate(diagnosis_date = date_of_death, source = "NRS deaths")

dementia_index <- bind_rows(df_linked, unlinked_dementia_deaths)

###check if any deaths < diagnosis dates
dementia_index <- dementia_index %>%
  ##flag if more than 1 day earlier
  mutate(wrong_dod = case_when(date_of_death < (diagnosis_date-1) ~ 1  , T~0)) %>%
  mutate(death_diff = as.Date(date_of_death) - as.Date(diagnosis_date))

#table(dementia_index$wrong_dod)
#table(dementia_index$death_diff <0)
#table(dementia_index$death_diff <(-2))
##ok, quite a lot, although small in % terms, and not just a few days out!

table(year(dementia_index$date_of_death), year(dementia_index$diagnosis_date))
table(dementia_index$source, (dementia_index$death_diff <0 & dementia_index$death_diff >= (-1) ))
###mainly social care diagnoses are linked to wrong death (or maybe SC has wrong dates)

##remove the date of death is it is 
dementia_index <- dementia_index %>%
  mutate(date_of_death = case_when(wrong_dod==1 ~NA, T~date_of_death)) %>% 
  select(-wrong_dod, -death_diff, -n_chi) %>%
  #add date of death for the death only records.
  mutate(date_of_death = case_when(source=="NRS deaths" ~ diagnosis_date, T~date_of_death))

###derive healthboard of residence and other geographies####
dementia_index <- dementia_index %>%
  mutate(postcode = phsmethods::format_postcode(postcode, "pc7"))  %>%
  left_join(geogs_lookup, by = c("postcode" = "pc7"))

table(dementia_index$hb2019name, dementia_index$source)
table(year(dementia_index$diagnosis_date)[dementia_index$source=="social care"],
      dementia_index$hb2019name[dementia_index$source=="social care"])
##social care seem to have  low returns for lothian

##SIMD###
dementia_index <- dementia_index %>%
 left_join(simd_lookup, by = c("postcode" = "pc7")) %>% 
  mutate(SIMD_at_diag  = case_when(year(diagnosis_date) >= 2017 ~ simd2020v2_sc_quintile,
                                   year(diagnosis_date) >= 2014 & year(diagnosis_date) < 2017 ~ simd2016_sc_quintile,
                                   year(diagnosis_date)>= 2010 & year(diagnosis_date) < 2014 ~ simd2012_sc_quintile,
                                          TRUE ~ NA_real_)) %>%
    mutate(SIMD_at_diag = 
           case_when(source=="Care home census" | type_of_care_group=="care home" ~NA_real_, 
                     T~ SIMD_at_diag ))

###Ethnic group codes###
dementia_index <- dementia_index %>%
  mutate(ethnic_group = 
           case_when(ethnic_group== "1" ~ "1 White",
                     ethnic_group=="09" | ethnic_group=="-" | ethnic_group=="0" |ethnic_group=="99"~"99 Not Known",
                     is.na(ethnic_group) ~ "99 Not Known",
                     ethnic_group== "1A" ~ "1A Scottish",
                     ethnic_group== "1B" ~ "1B Other British",
                     ethnic_group== "1C" ~ "1C Irish",
                     ethnic_group== "1K" ~ "1K Gypsy/Traveller",
                     ethnic_group== "1L" ~ "1L Polish",
                     ethnic_group== "1Z" ~ "1Z Other white ethnic group",
                     ethnic_group== "2" ~ "2A Any mixed or multiple ethnic groups",
                     ethnic_group== "2 Any mixed or multiple ethnic groups" ~ "2A Any mixed or multiple ethnic groups",
                     ethnic_group== "2A" ~ "2A Any mixed or multiple ethnic groups",
                     ethnic_group== "3" ~ "3 Asian, Asian Scottish or Asian British",
                     ethnic_group== "3F" ~ "3F Pakistani, Pakistani Scottish or Pakistani British",
                     ethnic_group== "3G" ~ "3G Indian, Indian Scottish or Indian British",
                     ethnic_group== "3H" ~ "3H Bangladeshi, Bangladeshi Scottish or Bangladeshi British",
                     ethnic_group== "3J" ~ "3J Chinese, Chinese Scottish or Chinese British",
                     ethnic_group== "3Z" ~ "3Z Other Asian, Asian Scottish or Asian British",
                     ethnic_group== "4D" ~ "4D African, African Scottish or African British",
                     ethnic_group== "4X" ~ "4X African, Scottish African or British African",
                     ethnic_group== "5C" ~ "5C Caribbean, Caribbean Scottish or Caribbean British",
                     ethnic_group== "5Y" ~ "5Y Other Caribbean or Black",
                     ethnic_group== "6A" ~ "6A Arab, Arab Scottish or Arab British",
                     ethnic_group== "6Z" ~ "6Z Other ethnic group",
                     ethnic_group== "98" ~ "98 Refused/Not Provided by patient",
                     
                                                            T~ethnic_group))



##trying to work out sensible date limits for deriving chi dob
dementia_index <- dementia_index %>%
  mutate(age_diag =floor(as.numeric((as.Date(diagnosis_date) - as.Date(dob))/365.25))) %>%
  mutate(death_age_diag = floor(as.numeric((as.Date(diagnosis_date) - as.Date(death_dob))/365.25))) %>%
  mutate(chi_dob = phsmethods::dob_from_chi(upi_number, min_date = as.Date("1900-01-01"), max_date = as.Date("2000-01-01")), 
         chi_sex = phsmethods::sex_from_chi(upi_number) ) %>%
  mutate(chi_age_diag= floor(as.numeric((as.Date(diagnosis_date) - as.Date(chi_dob))/365.25)))

comparison_ages <- dementia_index %>% group_by(age_diag, chi_age_diag) %>% count()
comparison_death_ages <- dementia_index %>% group_by(age_diag, death_age_diag) %>% count()
table(dementia_index$death_age_diag)


##select variables and save###
dementia_index <- dementia_index %>%
  select(source, upi_number,chi_age_diag, chi_dob, chi_sex, age_diag, dob, diagnosis_date,
         diagnosis, diagnosis_description, date_of_death, 
         postcode, ch_postcode, SIMD_at_diag,hb2019, hb2019name, everything()) %>%
  select(-c(health_board_area, chi_postcode, sex, death_dob, 
            death_age_diag, hbtreat_currentdate, institution)) %>%
  rename(date_of_birth = dob, sex=chi_sex, chi_age_at_diagnosis = chi_age_diag, 
         age_at_diagnosis = age_diag, 
         hbres = hb2019name, hbres_code= hb2019) %>% 
  # use chi_age & dob when age is less than 18 or greater than 120
  mutate(age_at_diagnosis = case_when(age_at_diagnosis < 18 | age_at_diagnosis > 120 ~ chi_age_at_diagnosis,
                                      is.na(age_at_diagnosis) ~ chi_age_at_diagnosis,
                                      .default = age_at_diagnosis),
         date_of_birth = case_when(age_at_diagnosis < 18 | age_at_diagnosis > 120 ~ chi_dob,
                                   is.na(age_at_diagnosis) ~ chi_dob,
                                   .default = date_of_birth)) %>%
  select(-c(chi_age_at_diagnosis, chi_dob))

names(dementia_index)

##save full file####
saveRDS(dementia_index, "/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")
###take first record per person, per source (some record currently have >1 type recorded)

dementia_index_1row <- dementia_index %>%
  group_by(upi_number) %>%
  arrange(upi_number, diagnosis_date) %>% slice(1) %>% ungroup()
saveRDS(dementia_index_1row , "/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds")
##save as index


