###NRS deaths dementia diagnoses###
##NRS extract 1
###Extract all deaths with a dementia flag
cohort_start_date <- as.Date("2014-01-01")

####################################################
##data extract####
deaths_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "SELECT UPI_NUMBER,CHI, DATE_OF_DEATH,
     YEAR_OF_REGISTRATION , REGISTRATION_DISTRICT, ENTRY_NUMBER ,
    UNDERLYING_CAUSE_OF_DEATH ,
    CAUSE_OF_DEATH_CODE_0 ,CAUSE_OF_DEATH_CODE_1 ,CAUSE_OF_DEATH_CODE_2,
    CAUSE_OF_DEATH_CODE_3,CAUSE_OF_DEATH_CODE_4,CAUSE_OF_DEATH_CODE_5,
    CAUSE_OF_DEATH_CODE_6,CAUSE_OF_DEATH_CODE_7,CAUSE_OF_DEATH_CODE_8,
    CAUSE_OF_DEATH_CODE_9,INSTITUTION, SEX, POSTCODE,HEALTH_BOARD_AREA
    FROM ANALYSIS.GRO_DEATHS_C SMR
    WHERE DATE_OF_DEATH >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')
    ")
  )
) %>%
  clean_names() %>%
  ungroup() %>%
  mutate(upi_number = case_when(is.na(upi_number) ~chi, T~upi_number)) %>%
  filter(!is.na(upi_number))



#####################################################

##flag and select deaths with any dementia code
deaths <- deaths_temp_1 %>% 
  mutate(flag_dementia = case_when(substr(underlying_cause_of_death,1,3) %in% icd10_dementia ~1,
                                   substr(underlying_cause_of_death,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_0,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_0,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_1,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_1,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_2,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_2,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_3,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_3,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_4,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_4,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_5,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_5,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_6,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_6,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_7,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_7,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_8,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_8,1,4) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_9,1,3) %in% icd10_dementia ~1,
                                   substr(cause_of_death_code_9,1,4) %in% icd10_dementia ~1,
                                   T~0))
                                   
                                   
table(deaths$flag_dementia)  
dementia_deaths <- deaths %>% filter(flag_dementia==1)


###Flag dementia types####
#5th character codes dont appeat to be used so just need to check 3 and 4 char codes
dementia_deaths  <-dementia_deaths  %>%
  mutate(flag_G30_codes = case_when(substr(underlying_cause_of_death,1,3) =="G30"~1,
                                    substr(cause_of_death_code_0,1,3) =="G30"~1,
                                    substr(cause_of_death_code_1,1,3) =="G30"~1,
                                    substr(cause_of_death_code_2,1,3) =="G30"~1,
                                    substr(cause_of_death_code_3,1,3) =="G30"~1,
                                    substr(cause_of_death_code_4,1,3) =="G30"~1,
                                    substr(cause_of_death_code_5,1,3) =="G30"~1,
                                    substr(cause_of_death_code_6,1,3) =="G30"~1,
                                    substr(cause_of_death_code_7,1,3) =="G30"~1,
                                    substr(cause_of_death_code_8,1,3) =="G30"~1,
                                    substr(cause_of_death_code_9,1,3) =="G30"~1,
                                    T~0)) %>%
  mutate(flag_alzheimers = case_when(substr(underlying_cause_of_death,1,3) =="F00"~1,
                                     substr(cause_of_death_code_0,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_1,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_2,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_3,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_4,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_5,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_6,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_7,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_8,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_9,1,3)  =="F00"~1,
                                    T~0))
table(dementia_deaths$flag_alzheimers, dementia_deaths$flag_G30_codes)
##none in combination so  ignore the g30 codes.
dementia_deaths  <- dementia_deaths  %>%
  mutate(dementia_alzheimers = case_when(substr(underlying_cause_of_death,1,3) =="F00"~1,
                                     substr(cause_of_death_code_0,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_1,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_2,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_3,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_4,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_5,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_6,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_7,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_8,1,3)  =="F00"~1,
                                     substr(cause_of_death_code_9,1,3)  =="F00"~1,
                                     T~0)) %>%
             mutate(vascular_dementia = case_when(substr(underlying_cause_of_death,1,3) =="F01"~1,
                                                  substr(cause_of_death_code_0,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_1,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_2,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_3,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_4,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_5,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_6,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_7,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_8,1,3)  =="F01"~1,
                                                  substr(cause_of_death_code_9,1,3)  =="F01"~1,
                                                                T~0), 
                                  dementia_picks = case_when(substr(underlying_cause_of_death,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_0,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_1,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_2,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_3,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_4,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_5,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_6,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_7,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_8,1,4) =="F020" ~1,
                                                             substr(cause_of_death_code_9,1,4) =="F020" ~1,
                                                             T~0),
                                  dementia_cjd = case_when(substr(underlying_cause_of_death,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_0,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_1,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_2,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_3,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_4,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_5,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_6,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_7,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_8,1,4) =="F021" ~1,
                                                           substr(cause_of_death_code_9,1,4) =="F021" ~1,
                                                           T~0),
                                  dementia_hunting = case_when(substr(underlying_cause_of_death,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_0,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_1,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_2,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_3,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_4,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_5,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_6,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_7,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_8,1,4) =="F022" ~1,
                                                                substr(cause_of_death_code_9,1,4) =="F022" ~1,
                                                                T~0),
                                  dementia_park = case_when(substr(underlying_cause_of_death,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_0,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_1,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_2,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_3,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_4,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_5,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_6,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_7,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_8,1,4) =="F023" ~1,
                                                            substr(cause_of_death_code_9,1,4) =="F023" ~1,
                                                            T~0),
                                  dementia_hiv = case_when(substr(underlying_cause_of_death,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_0,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_1,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_2,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_3,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_4,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_5,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_6,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_7,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_8,1,4) =="F024" ~1,
                                                           substr(cause_of_death_code_9,1,4) =="F024" ~1,
                                                           T~0), 
                    dementia_other_dis = case_when(dementia_picks==0 &
                                                     substr(underlying_cause_of_death,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_0,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_1,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_2,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_3,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_4,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_5,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_6,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_7,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_8,1,3) == "F02" ~1,
                                                   substr(cause_of_death_code_9,1,3) == "F02" ~1,
                                                   T~0))

###save dementia deaths extract####
saveRDS(dementia_deaths, paste0(folder_data_path, "/extracts/dementia_deaths.rds"))
