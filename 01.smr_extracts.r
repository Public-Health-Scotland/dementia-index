###Extract from SMR01 and SMR01-1E
###uses ~4GB memory
cohort_start_date <- as.Date("2014-01-01")
## run the setup file first#
source("00.setup.r")
##extract based on 2 character codes
data_smr01_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
    SELECT UPI_NUMBER,CI_CHI_NUMBER, LINK_NO, GLS_CIS_MARKER, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE,
    HBTREAT_CURRENTDATE,LOCATION,
    MAIN_CONDITION,OTHER_CONDITION_1,
    OTHER_CONDITION_2,OTHER_CONDITION_3,
    OTHER_CONDITION_4,OTHER_CONDITION_5,
    HBRES_CURRENTDATE, DOB, ETHNIC_GROUP, DR_POSTCODE, POSTCODE
    FROM ANALYSIS.SMR01_PI SMR
    WHERE SMR.DISCHARGE_DATE >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
      AND (SUBSTR(MAIN_CONDITION, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'F0'
    OR SUBSTR(MAIN_CONDITION, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'G3')
    ORDER BY link_no, admission_date, discharge_date, admission, discharge, uri ASC")
  )
) %>%
  clean_names() %>%
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup() %>%
  mutate(upi_number = case_when(is.na(upi_number) ~ci_chi_number, T~upi_number)) %>%
  filter(!is.na(upi_number))

###SMR01e
data_smr01_1e_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
    SELECT UPI_NUMBER,CI_CHI_NUMBER, LINK_NO,CIS_MARKER, GLS_CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE,
    HBTREAT_CURRENTDATE,LOCATION,
    MAIN_CONDITION,OTHER_CONDITION_1,
    OTHER_CONDITION_2,OTHER_CONDITION_3,
    OTHER_CONDITION_4,OTHER_CONDITION_5,
    HBRES_CURRENTDATE, DOB, ETHNIC_GROUP, DR_POSTCODE, POSTCODE
    FROM ANALYSIS.SMR01_1E_PI SMR
    WHERE SMR.DISCHARGE_DATE >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
      AND (SUBSTR(MAIN_CONDITION, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'F0'
    OR SUBSTR(MAIN_CONDITION, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'G3')
    ORDER BY link_no, admission_date, discharge_date, admission, discharge, uri ASC")
  )
) %>%
  clean_names() %>%
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup()  %>%
  mutate(upi_number = case_when(is.na(upi_number) ~ci_chi_number, T~upi_number))%>%
  filter(!is.na(upi_number))

data_smr04 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
   SELECT UPI_NUMBER,CI_CHI_NUMBER, LINK_NO,CIS_MARKER,  ADMISSION_DATE, DISCHARGE_DATE,
    HBTREAT_CURRENTDATE,LOCATION,
    MAIN_CONDITION,OTHER_CONDITION_1,
    OTHER_CONDITION_2,OTHER_CONDITION_3,
    OTHER_CONDITION_4,OTHER_CONDITION_5,
    HBRES_CURRENTDATE, DOB, ETHNIC_GROUP, DR_POSTCODE, POSTCODE
    FROM ANALYSIS.SMR01_1E_PI SMR
    WHERE SMR.DISCHARGE_DATE >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
      AND (SUBSTR(MAIN_CONDITION, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'F0' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'F0'
    OR SUBSTR(MAIN_CONDITION, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'G3' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'G3')
    ORDER BY link_no, admission_date, discharge_date, admission, discharge, uri ASC")
  )
) %>%
  clean_names() %>%
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup()  %>%
  mutate(upi_number = case_when(is.na(upi_number) ~ci_chi_number, T~upi_number))%>%
  filter(!is.na(upi_number))

data_smr04_sub <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
       SELECT UPI_NUMBER,CI_CHI_NUMBER, LINK_NO,CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE,
    HBTREAT_CURRENTDATE,LOCATION,
    MAIN_CONDITION,OTHER_CONDITION_1,
    OTHER_CONDITION_2,OTHER_CONDITION_3,
    OTHER_CONDITION_4,OTHER_CONDITION_5,
    HBRES_CURRENTDATE, DOB, ETHNIC_GROUP, DR_POSTCODE, POSTCODE
    FROM ANALYSIS.SMR01_1E_PI SMR
    WHERE SMR.DISCHARGE_DATE >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
    AND  (SUBSTR(MAIN_CONDITION, 1, 2) = 'F1' 
    OR SUBSTR(OTHER_CONDITION_1, 1, 2) = 'F1' 
    OR SUBSTR(OTHER_CONDITION_2, 1, 2) = 'F1' 
    OR SUBSTR(OTHER_CONDITION_3, 1, 2) = 'F1' 
    OR SUBSTR(OTHER_CONDITION_4, 1, 2) = 'F1' 
    OR SUBSTR(OTHER_CONDITION_5, 1, 2) = 'F1')
    ORDER BY link_no, admission_date, discharge_date, admission, discharge, uri ASC")
  )
) %>%
  clean_names() %>%
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup() %>%
  mutate(upi_number = case_when(is.na(upi_number) ~ci_chi_number, T~upi_number)) %>%
  filter(!is.na(upi_number))

##filter to exact codes that we need. 
smr <- bind_rows(data_smr01_1e_temp_1, data_smr01_temp_1, data_smr04, data_smr04_sub)


smr <- smr %>%
  mutate(flag_dementia = case_when(substr(main_condition,1,3) %in% icd10_dementia ~1,
                                   substr(other_condition_1,1,3) %in% icd10_dementia ~1,
                                   substr(other_condition_2,1,3) %in% icd10_dementia ~1,
                                   substr(other_condition_3,1,3) %in% icd10_dementia ~1,
                                   substr(other_condition_4,1,3) %in% icd10_dementia ~1,
                                   substr(other_condition_5,1,3) %in% icd10_dementia ~1,
                                   substr(main_condition,1,5) %in% fifth_char_codes ~1,
                                   substr(other_condition_1,1,5) %in% fifth_char_codes ~1,
                                   substr(other_condition_2,1,5) %in% fifth_char_codes ~1,
                                   substr(other_condition_3,1,5) %in% fifth_char_codes ~1,
                                   substr(other_condition_4,1,5) %in% fifth_char_codes ~1,
                                   substr(other_condition_5,1,5) %in% fifth_char_codes ~1, 
                                   substr(main_condition,1,4) %in% icd10_dementia ~1,
                                   substr(other_condition_1,1,4) %in% icd10_dementia ~1,
                                   substr(other_condition_2,1,4) %in% icd10_dementia ~1,
                                   substr(other_condition_3,1,4) %in% icd10_dementia ~1,
                                   substr(other_condition_4,1,4) %in% icd10_dementia ~1,
                                   substr(other_condition_5,1,4) %in% icd10_dementia ~1, 
                                   T~0)) %>%
  mutate(flag_G30_codes = case_when(substr(main_condition,1,3) =="G30"~1,
                                    substr(other_condition_1,1,3) =="G30"~1,
                                    substr(other_condition_2,1,3) =="G30"~1,
                                    substr(other_condition_3,1,3) =="G30"~1,
                                    substr(other_condition_4,1,3) =="G30"~1,
                                    substr(other_condition_5,1,3) =="G30"~1, 
                                    T~0)) %>%
  mutate(flag_alzheimers = case_when(substr(main_condition,1,3) =="F00"~1,
                                     substr(other_condition_1,1,3)  =="F00"~1,
                                     substr(other_condition_2,1,3)  =="F00"~1,
                                     substr(other_condition_3,1,3)  =="F00"~1,
                                     substr(other_condition_4,1,3)  =="F00"~1,
                                     substr(other_condition_5,1,3)  =="F00"~1,
                                     T~0))

table(smr$flag_G30_codes, smr$flag_alzheimers)

#g30 does appear on its own so cant use those?
smr <- smr %>% filter(flag_dementia==1) %>% mutate()


##summarise but dont lose type of dementia
smr <- smr %>%
  group_by(upi_number, cis_marker,gls_cis_marker) %>%
  mutate( admission_date = min(admission_date), 
          discharge_date = max(discharge_date),
          postcode = first_(postcode), 
          location = first_(location), 
          hbtreat_currentdate = first_(hbtreat_currentdate), 
          hbres_currentdate = first_(hbres_currentdate)) %>% ungroup
##temporary save 
saveRDS(smr, paste0(folder_data_path, "/extracts/temp_smr_raw.rds"))
smr_raw <- readRDS(paste0(folder_data_path, "/extracts/temp_smr_raw.rds"))
##long smr extract
smr_long <- smr_raw  %>% ungroup %>%
  pivot_longer(names_to = "diagnosis position", 
               cols = main_condition:other_condition_5, values_to = "diagnosis") %>%
  filter(!is.na(diagnosis))

##flag dementia types
smr_long <- smr_long %>%
  ##start with the specific
  mutate(flag_dementia = case_when(substr(diagnosis,1,3) %in% icd10_dementia ~1,
                                   substr(diagnosis,1,4) %in% icd10_dementia ~1,
                                   T~0),
         alzheimer_flag = case_when(substr(diagnosis,1,3) %in% alzheimer_codes ~1,
                                    substr(diagnosis,1,4) %in% alzheimer_codes ~1,
                                    T~0)) %>%
  mutate(dementia_alcohol = case_when(diagnosis =="F1073" ~1, T~0),
         lewy_f = case_when(diagnosis =="F028 A"  ~1, T~0), 
         lewy_g = case_when(diagnosis =="G318 D" ~1, T~0),
         vascular_dementia = case_when(substr(diagnosis ,1,3)=="F01" ~1,
                                       T~0), 
         dementia_other_dis = case_when(substr(diagnosis,1,3) == "F02" ~1,T~0), 
         dementia_picks = case_when(substr(diagnosis,1,4) =="F020" ~1,
                                    T~0),
         dementia_cjd = case_when(substr(diagnosis,1,4) =="F021" ~1,
                                  T~0) ,
         dementia_hunting = case_when(   substr(diagnosis,1,4) =="F022" ~1,
                                         T~0) ,
         dementia_park = case_when(substr(diagnosis,1,4) =="F023" ~1,
                                   T~0) ,
         dementia_hiv = case_when(substr(diagnosis,1,4) =="F024" ~1,
                                  T~0), 
         dementia_other_substances =
           case_when(substr(diagnosis,1,5) %in% substance_codes~1, 
                     T~0)) %>%
  filter(flag_dementia==1| lewy_g==1)

##aggregate on upi and date 

smr_agg <- smr_long %>%
  group_by(upi_number, admission_date, discharge_date,
           hbtreat_currentdate, location, hbres_currentdate , 
           dob, ethnic_group, 
           dr_postcode, postcode) %>%
  summarise(flag_dementia= max(flag_dementia), 
            alzheimer_flag = max(alzheimer_flag), 
            lewy_f = max_(lewy_f), 
            lewy_g = max_(lewy_g), 
            dementia_alcohol = max_(dementia_alcohol),
            vascular_dementia = max_(vascular_dementia), 
            dementia_other_dis = max_(dementia_other_dis), 
            dementia_picks = max_(dementia_picks), 
            dementia_cjd = max_(dementia_cjd), 
            dementia_hunting = max_(dementia_hunting), 
            dementia_park = max_(dementia_park), 
            dementia_hiv = max_(dementia_hiv)) %>%
  ungroup() %>%
  mutate(dementia_lewy_body = case_when(lewy_f==1 & lewy_g==1 ~1, T~0)) %>%
  mutate(dementia_other_dis = case_when(dementia_lewy_body==1 ~0, T~dementia_other_dis))%>%
  select(-lewy_f, lewy_g)
#select first date for each dementia type.     

first_dementia_unspec <-smr_agg %>%
  filter(flag_dementia==1 & alzheimer_flag==0 & dementia_lewy_body==0 &
           dementia_other_dis==0 & vascular_dementia==0 & dementia_alcohol==0) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number, flag_dementia) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode)) %>%
  ungroup() %>%
  mutate(dementia_unspecified=1)

first_alzheimer <-smr_agg %>%
  filter(alzheimer_flag==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number,alzheimer_flag) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()

first_alcohol <-smr_agg %>%
  filter(dementia_alcohol==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number,dementia_alcohol) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()

first_vascular <-smr_agg %>%
  filter(vascular_dementia==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number, vascular_dementia) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()

first_lewy <-smr_agg %>%
  filter(dementia_lewy_body==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number,dementia_lewy_body) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()

first_picks <-smr_agg %>%
  filter(dementia_picks==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number,dementia_picks) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()


first_dementia_other_dis <-smr_agg %>%
  filter(dementia_picks==0 & dementia_lewy_body==0 &  dementia_other_dis==1) %>%
  arrange(upi_number,admission_date) %>%
  group_by(upi_number,dementia_other_dis) %>%
  summarise(diagnosis_date = first_(admission_date),
            hbtreat_currentdate = first(hbtreat_currentdate),
            location = first(location),   hbres_currentdate = first(hbres_currentdate), 
            dob = first(dob), ethnic_group = first(ethnic_group), dr_postcode = first(dr_postcode), 
            postcode = first(postcode))%>%
  ungroup()


all_first_diags <- bind_rows(first_dementia_unspec, first_alzheimer, first_dementia_other_dis, 
                             first_lewy, first_picks, first_vascular, first_alcohol)
names(all_first_diags  )

all_first_diags_wide <-all_first_diags %>% select(upi_number, diagnosis_date ,dementia_unspecified, alzheimer_flag,
                                                  vascular_dementia , dementia_lewy_body, dementia_picks, 
                                                  dementia_other_dis, dementia_alcohol) %>%
  mutate(dementia_type = case_when(dementia_unspecified==1~ "Unspecified_dementia", 
                                   alzheimer_flag==1 ~ "Dementia_Alzheimers", 
                                   vascular_dementia==1 ~ "Vascular_dementia", 
                                   dementia_lewy_body==1 ~ "Dementia_Lewy_body", 
                                   dementia_picks==1 ~ "Dementia_Picks", 
                                   dementia_other_dis== 1~ "Dementia_other_diseases", 
                                   dementia_alcohol==1 ~ "Dementia_alcohol")) %>%
  select(upi_number, diagnosis_date, dementia_type) %>%
  pivot_wider(names_from= dementia_type, values_from = diagnosis_date)

##if there are no dementia alcohol diagnoses
if(is.null(all_first_diags_wide$Dementia_alcohol)){
  all_first_diags_wide$Dementia_alcohol<-NA
}

all_first_diags_wide <- all_first_diags_wide %>%
  mutate(flag_unspec = case_when(!is.na(Unspecified_dementia)~1, T~0), 
         flag_alz = case_when(!is.na(Dementia_Alzheimers)~1, T~0), 
         flag_vasc = case_when(!is.na(Vascular_dementia)~1, T~0), 
         flag_lewy = case_when(!is.na(Dementia_Lewy_body)~1, T~0), 
         flag_pick = case_when(!is.na(Dementia_Picks)~1, T~0),
         flag_other_dis = case_when(!is.na(Dementia_other_diseases)~1, T~0), 
         flag_alcohol = case_when(!is.na(Dementia_alcohol)~1, T~0)) %>%
  mutate(total_types = flag_unspec+flag_alz+flag_vasc+flag_lewy+flag_pick+flag_other_dis+flag_alcohol)

###split off those with just one record
one_type <- all_first_diags_wide %>% filter(total_types==1)

###identify people with more than one type of dementia diagnosis
multiple_type <- all_first_diags_wide %>% filter(total_types>1)

##if unspecificed dementia was not the first diagnosis, we don't care about it
multiple_type <- multiple_type %>%
  rowwise() %>% 
  mutate(minimum_date  = min_(c(Unspecified_dementia, Dementia_Alzheimers, Vascular_dementia, Dementia_Lewy_body, 
                                Dementia_Picks, Dementia_other_diseases, Dementia_alcohol))) %>%
  ungroup() %>%
  ##if unspecified dementia is not the first record we drop it
  mutate(flag_unspec = case_when(Unspecified_dementia!=minimum_date ~0, T~flag_unspec)) %>%
  mutate(Unspecified_dementia = case_when(flag_unspec==0 ~ NA, T~Unspecified_dementia)) %>%
  #check if unspecified dementia recorded at the SAME date as another diagnosis discard in this case also
  mutate(Unspecified_dementia = case_when(Unspecified_dementia==Dementia_Alzheimers ~ NA, 
                                          Unspecified_dementia==Vascular_dementia ~NA,
                                          Unspecified_dementia== Dementia_Lewy_body ~NA,
                                          Unspecified_dementia== Dementia_Picks ~NA,
                                          Unspecified_dementia== Dementia_other_diseases ~NA,
                                          Unspecified_dementia== Dementia_alcohol ~NA,
                                          T~Unspecified_dementia)) %>%
  mutate(flag_unspec = case_when(is.na(Unspecified_dementia) ~0, T~flag_unspec)) %>%
  mutate(total_types = flag_unspec+flag_alz+flag_vasc+flag_lewy+flag_pick+flag_other_dis+flag_alcohol)

##those with just one remaining specific diangosis get joined to one_type
one_specific_type <- multiple_type %>% filter(total_types==1)
one_type <- bind_rows(one_type, one_specific_type)

###those that really have multiple types, or have non-specfic followed by specific
multiple_type <- multiple_type %>% filter(total_types>1)
table(multiple_type$total_types)

# combine and save dates,

all_diags <-bind_rows(one_type, multiple_type)

##ultimately need to define the order of diagnoses but this can wait until the different extracts are combined.
saveRDS(all_diags, paste0(folder_data_path, "/extracts/smr_all_aggregated.rds" ))


smr <- readRDS(paste0(folder_data_path, "/extracts/smr_all_aggregated.rds" ))
names(smr)
smr <- smr %>% mutate(dementia_subtype =  case_when(total_types==1 & flag_unspec==1 ~ "07 yet to be determined", 
                                                    total_types==1 & flag_vasc==1 ~ "02 Vascular Dementia",
                                                    total_types==1 & flag_alz==1 ~ "01 Dementia in Alzheimer's Disease",
                                                    total_types==2 & flag_alz==1 & flag_vasc==1 ~ "03 Alzheimer's/Vascular (Mixed)",
                                                    total_types==1 & flag_lewy==1 ~ "04 Lewy Body Dementia",
                                                    total_types==1 & flag_pick==1 ~ "05 Frontotemporal Dementia",
                                                    total_types==1 & flag_alcohol==1 ~ "06 Alcohol-Related Cognitive Impairment",
                                                    total_types==1 & flag_other_dis ~ "97 Other", 
                                                    total_types >1 ~"Other mixed types", T~"Unknown"
)) %>%
  rowwise() %>% 
  mutate(last_diagnosis_date =
           case_when(total_types >1 ~
                       max_(c(Unspecified_dementia, Dementia_Alzheimers,
                              Vascular_dementia, Dementia_Lewy_body, 
                              Dementia_Picks, Dementia_other_diseases, Dementia_alcohol)), T~NA)) %>% 
  rowwise() %>%
  mutate(minimum_date =
           min_(c(Unspecified_dementia, Dementia_Alzheimers,
                              Vascular_dementia, Dementia_Lewy_body, 
                              Dementia_Picks, Dementia_other_diseases, Dementia_alcohol))) %>% 
  ungroup() %>%
  mutate(dementia_subtype_1 = case_when(total_types==1 ~ dementia_subtype, 
                                        total_types> 1 & !is.na(Dementia_Alzheimers) & 
                                          !is.na(Vascular_dementia) &
                                          Dementia_Alzheimers==Vascular_dementia &
                                          Vascular_dementia==minimum_date ~ "03 Alzheimer's/Vascular (Mixed)",
                                        total_types> 1 & Vascular_dementia==minimum_date ~ "02 Vascular Dementia",
                                        total_types> 1 & Dementia_Alzheimers==minimum_date ~ "01 Dementia in Alzheimer's Disease",
                                        total_types> 1 & Dementia_Lewy_body==minimum_date ~ "04 Lewy Body Dementia",
                                        total_types> 1 & Dementia_Picks==minimum_date ~ "05 Frontotemporal Dementia",
                                        total_types> 1 & Dementia_alcohol==minimum_date ~"06 Alcohol-Related Cognitive Impairment",
                                        total_types> 1 & Dementia_other_diseases==minimum_date ~"97 Other", 
                                        total_types> 1 & Unspecified_dementia ==minimum_date ~ "07 yet to be determined", 
                                        T~NA))

two <- smr %>% filter(total_types==2)
three<- smr %>% filter(total_types==3)
one <- smr %>% filter(total_types==1)

one <- one %>% select(upi_number, dementia_subtype_1, minimum_date) %>%
  rename(diagnosis_date_1 = minimum_date)

two <- two %>% rowwise() %>% 
  mutate(last_diagnosis_date = 
           max_(c(Unspecified_dementia, Dementia_Alzheimers, Vascular_dementia, Dementia_Lewy_body, 
                  Dementia_Picks, Dementia_other_diseases, Dementia_alcohol))) %>% 
  mutate(dementia_subtype_2 = 
           case_when(minimum_date!=last_diagnosis_date & 
                       Dementia_Alzheimers==Vascular_dementia & Vascular_dementia==last_diagnosis_date ~ "03 Alzheimer's/Vascular (Mixed)",
                     minimum_date!=last_diagnosis_date & Vascular_dementia==last_diagnosis_date ~ "02 Vascular Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_Alzheimers==last_diagnosis_date ~ "01 Dementia in Alzheimer's Disease",
                     minimum_date!=last_diagnosis_date & Dementia_Lewy_body==last_diagnosis_date ~ "04 Lewy Body Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_Picks==last_diagnosis_date ~ "05 Frontotemporal Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_alcohol==last_diagnosis_date ~"06 Alcohol-Related Cognitive Impairment",
                     minimum_date!=last_diagnosis_date & Dementia_other_diseases==last_diagnosis_date ~"97 Other", 
                     minimum_date!=last_diagnosis_date & Unspecified_dementia ==last_diagnosis_date ~ "07 yet to be determined",
                     minimum_date==last_diagnosis_date &  
                       Dementia_Alzheimers==last_diagnosis_date & dementia_subtype_1 != "01 Dementia in Alzheimer's Disease"  ~
                       "01 Dementia in Alzheimer's Disease",
                     minimum_date==last_diagnosis_date &  
                       Vascular_dementia==last_diagnosis_date & dementia_subtype_1 != "02 Vascular Dementia"  ~
                       "02 Vascular Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_Lewy_body==last_diagnosis_date & dementia_subtype_1 != "04 Lewy Body Dementia"  ~
                       "04 Lewy Body Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_Picks==last_diagnosis_date & dementia_subtype_1 != "05 Frontotemporal Dementia"  ~
                       "05 Frontotemporal Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_alcohol==last_diagnosis_date & dementia_subtype_1 != "06 Alcohol-Related Cognitive Impairment"  ~
                       "06 Alcohol-Related Cognitive Impairment",
                     minimum_date==last_diagnosis_date &   
                       Dementia_other_diseases==last_diagnosis_date & dementia_subtype_1 != "97 Other"  ~
                       "97 Other", 
                     T~NA))
table(two$dementia_subtype_1, two$dementia_subtype_2, useNA="always")  

three <- three %>%
  rowwise() %>% 
  mutate(last_diagnosis_date = 
           max_(c(Unspecified_dementia, Dementia_Alzheimers, Vascular_dementia, Dementia_Lewy_body, 
                  Dementia_Picks, Dementia_other_diseases, Dementia_alcohol))) %>% 
  mutate(dementia_subtype_3 = 
           case_when(minimum_date!=last_diagnosis_date & 
                       Dementia_Alzheimers==Vascular_dementia & Vascular_dementia==last_diagnosis_date ~ "03 Alzheimer's/Vascular (Mixed)",
                     minimum_date!=last_diagnosis_date & Vascular_dementia==last_diagnosis_date ~ "02 Vascular Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_Alzheimers==last_diagnosis_date ~ "01 Dementia in Alzheimer's Disease",
                     minimum_date!=last_diagnosis_date & Dementia_Lewy_body==last_diagnosis_date ~ "04 Lewy Body Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_Picks==last_diagnosis_date ~ "05 Frontotemporal Dementia",
                     minimum_date!=last_diagnosis_date & Dementia_alcohol==last_diagnosis_date ~"06 Alcohol-Related Cognitive Impairment",
                     minimum_date!=last_diagnosis_date & Dementia_other_diseases==last_diagnosis_date ~"97 Other", 
                     minimum_date!=last_diagnosis_date & Unspecified_dementia ==last_diagnosis_date ~ "07 yet to be determined",
                     minimum_date==last_diagnosis_date &  
                       Dementia_Alzheimers==last_diagnosis_date & dementia_subtype_1 != "01 Dementia in Alzheimer's Disease"  ~
                       "01 Dementia in Alzheimer's Disease",
                     minimum_date==last_diagnosis_date &  
                       Vascular_dementia==last_diagnosis_date & dementia_subtype_1 != "02 Vascular Dementia"  ~
                       "02 Vascular Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_Lewy_body==last_diagnosis_date & dementia_subtype_1 != "04 Lewy Body Dementia"  ~
                       "04 Lewy Body Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_Picks==last_diagnosis_date & dementia_subtype_1 != "05 Frontotemporal Dementia"  ~
                       "05 Frontotemporal Dementia",
                     minimum_date==last_diagnosis_date &   
                       Dementia_alcohol==last_diagnosis_date & dementia_subtype_1 != "06 Alcohol-Related Cognitive Impairment"  ~
                       "06 Alcohol-Related Cognitive Impairment",
                     minimum_date==last_diagnosis_date &   
                       Dementia_other_diseases==last_diagnosis_date & dementia_subtype_1 != "97 Other"  ~
                       "97 Other", 
                     T~NA)) %>%
  mutate(dementia_subtype_2 = case_when(flag_alz==1 & 
                                          dementia_subtype_1 != "01 Dementia in Alzheimer's Disease"  &
                                          dementia_subtype_3 != "01 Dementia in Alzheimer's Disease" ~"01 Dementia in Alzheimer's Disease",
                                        flag_vasc ==1 & 
                                          dementia_subtype_1 != "02 Vascular Dementia"  &
                                          dementia_subtype_3 != "02 Vascular Dementia" ~"02 Vascular Dementia",
                                        flag_lewy ==1 & 
                                          dementia_subtype_1 != "04 Lewy Body Dementia"  &
                                          dementia_subtype_3 != "04 Lewy Body Dementia" ~ "04 Lewy Body Dementia",
                                        flag_pick ==1 & 
                                          dementia_subtype_1 != "05 Frontotemporal Dementia"  &
                                          dementia_subtype_3 != "05 Frontotemporal Dementia" ~"05 Frontotemporal Dementia",
                                        flag_alcohol ==1 & 
                                          dementia_subtype_1 != "06 Alcohol-Related Cognitive Impairment"  &
                                          dementia_subtype_3 != "06 Alcohol-Related Cognitive Impairment" ~ "06 Alcohol-Related Cognitive Impairment",
                                        flag_other_dis ==1 & 
                                          dementia_subtype_1 != "97 Other"  &
                                          dementia_subtype_3 != "97 Other" ~"97 Other",T~NA)) %>%
  mutate(diagnosis_date_2 =case_when(flag_alz==1 & 
                                       dementia_subtype_1 != "01 Dementia in Alzheimer's Disease"  &
                                       dementia_subtype_3 != "01 Dementia in Alzheimer's Disease" ~ Dementia_Alzheimers, 
                                     flag_vasc ==1 & 
                                       dementia_subtype_1 != "02 Vascular Dementia"  &
                                       dementia_subtype_3 != "02 Vascular Dementia" ~Vascular_dementia,
                                     flag_lewy ==1 & 
                                       dementia_subtype_1 != "04 Lewy Body Dementia"  &
                                       dementia_subtype_3 != "04 Lewy Body Dementia" ~ Dementia_Lewy_body,
                                     flag_pick ==1 & 
                                       dementia_subtype_1 != "05 Frontotemporal Dementia"  &
                                       dementia_subtype_3 != "05 Frontotemporal Dementia" ~Dementia_Picks,
                                     flag_alcohol ==1 & 
                                       dementia_subtype_1 != "06 Alcohol-Related Cognitive Impairment"  &
                                       dementia_subtype_3 != "06 Alcohol-Related Cognitive Impairment" ~ Dementia_alcohol,
                                     flag_other_dis ==1 & 
                                       dementia_subtype_1 != "97 Other"  &
                                       dementia_subtype_3 != "97 Other" ~ Unspecified_dementia,T~NA))

table(three$dementia_subtype_1, three$dementia_subtype_2, three$dementia_subtype_3, useNA = "always")


two <- two %>% 
  select(upi_number, minimum_date, dementia_subtype_1, last_diagnosis_date, dementia_subtype_2) %>%
  rename(diagnosis_date_1 = minimum_date, diagnosis_date_2 = last_diagnosis_date)


three  <- three %>% 
  select(upi_number, minimum_date, dementia_subtype_1,diagnosis_date_2, dementia_subtype_2,
         last_diagnosis_date, dementia_subtype_3) %>%
  rename(diagnosis_date_1 = minimum_date, diagnosis_date_3 = last_diagnosis_date)

###bind together
all_smr_diags <- bind_rows(one, two, three)
#prefix names#
all_smr_diags <-all_smr_diags %>%
  rename_with(.cols = everything(), function(x){paste0("smr_", x)})

saveRDS(all_smr_diags, "/PHI_conf/Dementia_Index/data/extracts/smr_diags_clean.rds")

#save SMR diags file
names(smr_raw)
##Save smr demographics file
smr_demogs <- smr_raw %>% filter(upi_number %in% all_smr_diags$smr_upi_number) %>%
  select(upi_number, admission_date, discharge_date, cis_marker, gls_cis_marker, hbtreat_currentdate, 
         location, dob, ethnic_group, dr_postcode, postcode)
saveRDS(smr_demogs, "/PHI_conf/Dementia_Index/data/extracts/smr_demogs.rds")

#