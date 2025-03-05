######################################
## Script 02. Join extracts ##########
######################################

###Step 1 load data and use CHI database to convert CHI to UPIs

###Bind records into one dataset. LONG
## Make sure the source record is identifiable
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

##select minimal variables.
##reame for consistency
names(smr_all)

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
  rename(diagnosis_date = admission_date)


names(smr)
PIS <- PIS %>% mutate(diagnosis_description = "prescription from BNF ch4.11") %>%
  rename(upi_number= pis_pat_upi_c,
         diagnosis_date = pis_presc_date, 
         postcode=pis_postcode,
         sex = pis_sex, 
         dob = pis_dob,
         ethnic_group = pis_ethnic_group) %>%
  mutate(date_type="smr_admission_date") %>% select(-pis_CHI_dob)

names(dementia_deaths)
PDS <- PDS %>%
  mutate(sex = case_when(pds_sex=="01 Male" ~1, 
                         pds_sex=="02 Female" ~1 )) %>%
  rename(upi_number= pds_chi_number,
         diagnosis_date = pds_diagnosis_date,
         diagnosis_description = pds_dementia_subtype,
         postcode=pds_postcode,
         dob = pds_date_of_birth,
         ethnic_group = pds_ethnic_group) %>%
  mutate(date_type="pds_date") %>% select(-pds_sex, -pds_source) %>% mutate(source="PDS")

social_care <- social_care %>%
  mutate(sex = case_when(chi_gender=="Male" ~ 1,chi_gender=="Female" ~2, T~9 )) %>%
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

##bind sources long  except dementia deaths####
df <- bind_rows(smr, PIS,PDS,CHC,social_care)
df <- df %>% select(upi_number, diagnosis_date, diagnosis, diagnosis_description, source,
                    dob, sex, postcode, ch_postcode, everything())
names(df)


##identify unlinked deaths in demenita deaths####
upi_list <- unique(df$upi_number)
unlinked_deaths <- dementia_deaths %>% filter(!upi_number %in% upi_list)
names(unlinked_deaths)
##extract nrs date of death####
death_start_date <- as.Date("2014-01-01")
##identify any duplicate deaths bfore trtying to link to main list. 
deaths_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "SELECT UPI_NUMBER,CHI, DATE_OF_DEATH,
     YEAR_OF_REGISTRATION , REGISTRATION_DISTRICT, ENTRY_NUMBER ,
     SEX, POSTCODE,HEALTH_BOARD_AREA
    FROM ANALYSIS.GRO_DEATHS_C SMR
    WHERE DATE_OF_DEATH >= TO_DATE('", death_start_date, "', 'yyyy-mm-dd')
    ")
  )) %>% clean_names()

table(is.na(deaths_temp_1$chi) , is.na(deaths_temp_1$upi_number))
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
table(deaths_temp_1$deaths_upi==deaths_temp_1$UPI_NUMBER, useNA="always")

##update the upi and remove records where no upi or chi can be found
deaths_temp_1 <- deaths_temp_1 %>% mutate(deaths_upi=case_when(is.na(UPI_NUMBER)~ deaths_upi, T~UPI_NUMBER)) %>%
  filter(!is.na(deaths_upi))
length(unique(deaths_temp_1$deaths_upi))
nrow(deaths_temp_1)

##still a few dups
deaths_temp_1<- deaths_temp_1 %>% group_by(deaths_upi) %>%
  mutate(n_chi = n()) %>% ungroup()
#dups2010 <- deaths_temp_1 %>% filter(n_chi>1)
#dups2010 <- dups2010 %>% group_by(deaths_upi) %>% #
#  mutate(dup_reg = case_when(min(date_of_death)==max(date_of_death) ~1, T~0)) %>%
#  ungroup()


dups <- deaths_temp_1 %>% filter(n_chi>1)


##some look like duplicate registrations - 
#same date of death and consecutive reg numbers, can probs use these still just slice(1)
dups <- dups %>% group_by(deaths_upi) %>% 
  mutate(dup_reg = case_when(min(date_of_death)==max(date_of_death) ~1, T~0)) %>%
  ungroup()

dups_fix <- dups %>% filter(dup_reg==1) %>%
  group_by(deaths_upi) %>% slice(1)

deaths_temp_1 <- deaths_temp_1 %>% filter(!(deaths_upi %in%  dups$deaths_upi)) 

deaths <- bind_rows(deaths_temp_1, dups_fix) %>%
  select(deaths_upi, date_of_death)
summary(df$diagnosis_date)
deaths<- deaths %>% group_by(deaths_upi) %>%
  mutate(n_chi = n()) %>% ungroup()
#dups2010 <- deaths_temp_1 %>% filter(n_chi>1)
#dups2010 <- dups2010 %>% group_by(deaths_upi) %>% #
#  mutate(dup_reg = case_when(min(date_of_death)==max(date_of_death) ~1, T~0)) %>%
#  ungroup()


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
         diagnosis_description2 = dementia_subtype_2
         )%>%
  mutate(sex = as.numeric(sex)) %>%
    mutate(diagnosis_date = date_of_death, source = "NRS deaths")

dementia_index <- bind_rows(df_linked, unlinked_dementia_deaths)

###check if any deaths < diagnosis dates
dementia_index <- dementia_index %>%
  ##flag if more than 1 day earlier
  mutate(wrong_dod = case_when(date_of_death < (diagnosis_date-1) ~ 1  , T~0)) %>%
  mutate(death_diff = as.Date(date_of_death) - as.Date(diagnosis_date))

table(dementia_index$wrong_dod)
table(dementia_index$death_diff <0)
table(dementia_index$death_diff <(-2))
##ok, quite a lot, although small in % terms, and not just a few days out!

table(year(dementia_index$date_of_death), year(dementia_index$diagnosis_date))
table(dementia_index$source, (dementia_index$death_diff <0 & dementia_index$death_diff >= (-1) ))
###mainly social care diagnoses are linked to wrong death (or maybe SC has wrong dates)
table(dementia_index$date_type, dementia_index$death_diff <0, useNA="always" )

##remove the date of death is it is 
dementia_index <- dementia_index %>%
  mutate(date_of_death = case_when(wrong_dod==1 ~NA, T~date_of_death)) %>% 
  select(-wrong_dod, -death_diff, -n_chi) %>%
  #add date of death for the death only records.
  mutate(date_of_death = case_when(source=="NRS deaths" ~ diagnosis_date, T~date_of_death))
names(dementia_index)
dementia_index <- dementia_index %>%
  select(source, upi_number, diagnosis_date, diagnosis, diagnosis_description, date_of_death, 
                          dob, sex, postcode, ch_postcode, everything())
##save full file####
saveRDS(dementia_index, "/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")
###take first record per person, per source (some record currently have >1 type recorded)

dementia_index_1row <- dementia_index %>%
  group_by(upi_number) %>%
  arrange(upi_number, diagnosis_date) %>% slice(1) %>% ungroup()
saveRDS(dementia_index_1row , "/PHI_conf/Dementia_Index/data/INDEX/dementia_index_first_incidence_only.rds")
##save as index

