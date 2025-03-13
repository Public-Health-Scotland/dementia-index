###NRS deaths dementia diagnoses###
##NRS extract 1
###Extract all deaths with a dementia flag
cohort_start_date <- as.Date("2014-01-01")
## run the setup file first#
source("00.setup.r")
####################################################
##data extract####
deaths_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "SELECT UPI_NUMBER,CHI, DATE_OF_BIRTH, DATE_OF_DEATH, AGE, AGE_UNITS, ETHNICITY_CODE,
      ETHNICITY_INDICATOR,
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

#table(deaths_temp_1$ethnicity_indicator, deaths_temp_1$ethnicity_code)

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
dementia_deaths <- dementia_deaths %>%
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
                                                   dementia_picks==0 &substr(cause_of_death_code_0,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_1,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_2,1,3) == "F02" ~1,
                                                   dementia_picks==0 &substr(cause_of_death_code_3,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_4,1,3) == "F02" ~1,
                                                   dementia_picks==0 &substr(cause_of_death_code_5,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_6,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_7,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_8,1,3) == "F02" ~1,
                                                   dementia_picks==0 & substr(cause_of_death_code_9,1,3) == "F02" ~1,
                                                   T~0)) %>%
  mutate(dementia_unspecified = case_when(
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
    underlying_cause_of_death == "F03"| underlying_cause_of_death %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_0 == "F03" |cause_of_death_code_0 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_1 == "F03" |cause_of_death_code_1 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_2 == "F03" |cause_of_death_code_2 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_3 == "F03" |cause_of_death_code_3 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_4 == "F03" |cause_of_death_code_4 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_5 == "F03" |cause_of_death_code_5 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_6 == "F03" |cause_of_death_code_6 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_7 == "F03" |cause_of_death_code_7 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_8 == "F03" |cause_of_death_code_8 %in% c("F03X", "F051") ~1,
    dementia_picks==0 & dementia_alzheimers==0 & vascular_dementia==0 &
      cause_of_death_code_9 == "F03" |cause_of_death_code_9 %in% c("F03X", "F051") ~1,
                                           T~0)) 
dementia_deaths  <- dementia_deaths  %>% mutate(source = "NRSdeaths")
dementia_deaths  <-dementia_deaths %>%  mutate(total_types = dementia_alzheimers+vascular_dementia + dementia_picks+
                           dementia_other_dis+ dementia_unspecified)
##SLim down to identifiers, and named diagnoses. just one date for this extract obv
dementia_deaths  <- dementia_deaths  %>% 
  mutate(dementia_subtype_1 = case_when(dementia_alzheimers==1 & vascular_dementia==1 ~ "03 Alzheimer's/Vascular (Mixed)",
                                        vascular_dementia==1 ~ "02 Vascular Dementia",
                                        dementia_alzheimers==1 ~ "01 Dementia in Alzheimer's Disease",
                                        dementia_picks==1 ~ "05 Frontotemporal Dementia",
                                        dementia_other_dis==1 ~ "97 Other", 
                                        dementia_unspecified==1 ~ "07 yet to be determined", T~"Unknown")) %>% 
  mutate(dementia_subtype_2 = case_when(dementia_subtype_1!="01 Dementia in Alzheimer's Disease" & 
                                   dementia_subtype_1!="03 Alzheimer's/Vascular (Mixed)" & dementia_alzheimers==1  ~
                                     "01 Dementia in Alzheimer's Disease",
                                 dementia_subtype_1!="02 Vascular Dementia" & 
                                   dementia_subtype_1!="03 Alzheimer's/Vascular (Mixed)" & vascular_dementia==1 ~ "02 Vascular Dementia",
                                dementia_subtype_1!="05 Frontotemporal Dementia" & dementia_picks==1 ~ "05 Frontotemporal Dementia",
                               dementia_subtype_1!="97 Other"& dementia_other_dis ~ "97 Other", T~NA
                              ))

###pull out the relevant codes

dementia_deaths  <- dementia_deaths  %>%
  mutate(icd10_1 = case_when(underlying_cause_of_death %in% icd10_dementia |
                               underlying_cause_of_death %in% dagger_code |
                               underlying_cause_of_death %in% fifth_char_codes ~ underlying_cause_of_death, 
                             cause_of_death_code_0 %in% icd10_dementia |
                             cause_of_death_code_0 %in% dagger_code |
                             cause_of_death_code_0 %in% fifth_char_codes ~ cause_of_death_code_0,
                             cause_of_death_code_1 %in% icd10_dementia |
                               cause_of_death_code_1 %in% dagger_code |
                               cause_of_death_code_1 %in% fifth_char_codes ~ cause_of_death_code_1,
                             cause_of_death_code_2 %in% icd10_dementia |
                               cause_of_death_code_2 %in% dagger_code |
                               cause_of_death_code_2 %in% fifth_char_codes ~ cause_of_death_code_2,
                             cause_of_death_code_3 %in% icd10_dementia |
                               cause_of_death_code_3 %in% dagger_code |
                               cause_of_death_code_3 %in% fifth_char_codes ~ cause_of_death_code_3,
                             cause_of_death_code_4 %in% icd10_dementia |
                               cause_of_death_code_4 %in% dagger_code |
                               cause_of_death_code_4 %in% fifth_char_codes ~ cause_of_death_code_4,
                             cause_of_death_code_5 %in% icd10_dementia |
                               cause_of_death_code_5 %in% dagger_code |
                               cause_of_death_code_5 %in% fifth_char_codes ~ cause_of_death_code_5,
                             cause_of_death_code_6 %in% icd10_dementia |
                               cause_of_death_code_6 %in% dagger_code |
                               cause_of_death_code_6 %in% fifth_char_codes ~ cause_of_death_code_6,
                             cause_of_death_code_7 %in% icd10_dementia |
                               cause_of_death_code_7 %in% dagger_code |
                               cause_of_death_code_7 %in% fifth_char_codes ~ cause_of_death_code_7,
                             cause_of_death_code_8 %in% icd10_dementia |
                               cause_of_death_code_8 %in% dagger_code |
                               cause_of_death_code_8 %in% fifth_char_codes ~ cause_of_death_code_8,
                             cause_of_death_code_9 %in% icd10_dementia |
                               cause_of_death_code_9 %in% dagger_code |
                               cause_of_death_code_9 %in% fifth_char_codes ~ cause_of_death_code_9,
                             T~NA), 
         icd10_2 = case_when((icd10_1 != cause_of_death_code_0 & cause_of_death_code_0 %in% icd10_dementia) |
                             (icd10_1 != cause_of_death_code_0 & cause_of_death_code_0 %in% dagger_code) |
                             (icd10_1 != cause_of_death_code_0 & cause_of_death_code_0 %in% fifth_char_codes) ~ cause_of_death_code_0,
                             (icd10_1 != cause_of_death_code_1 & cause_of_death_code_1 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_1 & cause_of_death_code_1 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_1 & cause_of_death_code_1 %in% fifth_char_codes) ~ cause_of_death_code_1,
                             (icd10_1 != cause_of_death_code_2 & cause_of_death_code_2 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_2 & cause_of_death_code_2 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_2 & cause_of_death_code_2 %in% fifth_char_codes) ~ cause_of_death_code_2,
                             (icd10_1 != cause_of_death_code_3 & cause_of_death_code_3 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_3 & cause_of_death_code_3 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_3 & cause_of_death_code_3 %in% fifth_char_codes) ~ cause_of_death_code_3,
                             (icd10_1 != cause_of_death_code_4 & cause_of_death_code_4 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_4 & cause_of_death_code_4 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_4 & cause_of_death_code_4 %in% fifth_char_codes) ~ cause_of_death_code_4,
                             (icd10_1 != cause_of_death_code_5 & cause_of_death_code_5 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_5 & cause_of_death_code_5 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_5 & cause_of_death_code_5 %in% fifth_char_codes) ~ cause_of_death_code_5,
                             (icd10_1 != cause_of_death_code_6 & cause_of_death_code_6 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_6 & cause_of_death_code_6 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_6 & cause_of_death_code_6 %in% fifth_char_codes) ~ cause_of_death_code_6,
                             (icd10_1 != cause_of_death_code_7 & cause_of_death_code_7 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_7 & cause_of_death_code_7 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_7 & cause_of_death_code_7 %in% fifth_char_codes) ~ cause_of_death_code_7,
                             (icd10_1 != cause_of_death_code_8 & cause_of_death_code_8 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_8 & cause_of_death_code_8 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_8 & cause_of_death_code_8 %in% fifth_char_codes) ~ cause_of_death_code_8,
                             (icd10_1 != cause_of_death_code_9 & cause_of_death_code_9 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_9 & cause_of_death_code_9 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_9 & cause_of_death_code_9 %in% fifth_char_codes) ~ cause_of_death_code_9,
                             T~NA) ,
         icd10_3 = case_when( (icd10_1 != cause_of_death_code_1 & icd10_2 != cause_of_death_code_1 &cause_of_death_code_1 %in% icd10_dementia) |
                               (icd10_1 != cause_of_death_code_1 & icd10_2 != cause_of_death_code_1 & cause_of_death_code_1 %in% dagger_code) |
                               (icd10_1 != cause_of_death_code_1 & icd10_2 != cause_of_death_code_1 & cause_of_death_code_1 %in% fifth_char_codes) ~ cause_of_death_code_1,
                              (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 &cause_of_death_code_2 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 & cause_of_death_code_2 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 & cause_of_death_code_2 %in% fifth_char_codes) ~ cause_of_death_code_2,
                              (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 &cause_of_death_code_3 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 & cause_of_death_code_3 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 & cause_of_death_code_3 %in% fifth_char_codes) ~ cause_of_death_code_3,
                              (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 &cause_of_death_code_4 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 & cause_of_death_code_4 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 & cause_of_death_code_4 %in% fifth_char_codes) ~ cause_of_death_code_4,
                              (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 &cause_of_death_code_5 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 & cause_of_death_code_5 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 & cause_of_death_code_5 %in% fifth_char_codes) ~ cause_of_death_code_5,
                              (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 &cause_of_death_code_6 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 & cause_of_death_code_6 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 & cause_of_death_code_6 %in% fifth_char_codes) ~ cause_of_death_code_6,
                              (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 &cause_of_death_code_7 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 & cause_of_death_code_7 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 & cause_of_death_code_7 %in% fifth_char_codes) ~ cause_of_death_code_7,
                              (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 &cause_of_death_code_8 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 & cause_of_death_code_8 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 & cause_of_death_code_8 %in% fifth_char_codes) ~ cause_of_death_code_8,
                              (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 &cause_of_death_code_9 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 & cause_of_death_code_9 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 & cause_of_death_code_9 %in% fifth_char_codes) ~ cause_of_death_code_9,
         ) ,
         icd10_4 = case_when( (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 & icd10_3 != cause_of_death_code_2 & cause_of_death_code_2 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 & icd10_3 != cause_of_death_code_2 & cause_of_death_code_2 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_2 & icd10_2 != cause_of_death_code_2 & icd10_3 != cause_of_death_code_2 & cause_of_death_code_2 %in% fifth_char_codes) ~ cause_of_death_code_2,
                              (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 & icd10_3 != cause_of_death_code_3 & cause_of_death_code_3 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 & icd10_3 != cause_of_death_code_3 & cause_of_death_code_3 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_3 & icd10_2 != cause_of_death_code_3 & icd10_3 != cause_of_death_code_3 & cause_of_death_code_3 %in% fifth_char_codes) ~ cause_of_death_code_3,
                               (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 & icd10_3 != cause_of_death_code_4 & cause_of_death_code_4 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 & icd10_3 != cause_of_death_code_4 & cause_of_death_code_4 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_4 & icd10_2 != cause_of_death_code_4 & icd10_3 != cause_of_death_code_4 & cause_of_death_code_4 %in% fifth_char_codes) ~ cause_of_death_code_4,
                          (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 & icd10_3 != cause_of_death_code_5 & cause_of_death_code_5 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 & icd10_3 != cause_of_death_code_5 & cause_of_death_code_5 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_5 & icd10_2 != cause_of_death_code_5 & icd10_3 != cause_of_death_code_5 & cause_of_death_code_5 %in% fifth_char_codes) ~ cause_of_death_code_5,
                               (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 & icd10_3 != cause_of_death_code_6 & cause_of_death_code_6 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 & icd10_3 != cause_of_death_code_6 & cause_of_death_code_6 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_6 & icd10_2 != cause_of_death_code_6 & icd10_3 != cause_of_death_code_6 & cause_of_death_code_6 %in% fifth_char_codes) ~ cause_of_death_code_6,
                               (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 & icd10_3 != cause_of_death_code_7 & cause_of_death_code_7 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 & icd10_3 != cause_of_death_code_7 & cause_of_death_code_7 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_7 & icd10_2 != cause_of_death_code_7 & icd10_3 != cause_of_death_code_7 & cause_of_death_code_7 %in% fifth_char_codes) ~ cause_of_death_code_7,
         
                              (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 & icd10_3 != cause_of_death_code_8 & cause_of_death_code_8 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 & icd10_3 != cause_of_death_code_8 & cause_of_death_code_8 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_8 & icd10_2 != cause_of_death_code_8 & icd10_3 != cause_of_death_code_8 & cause_of_death_code_8 %in% fifth_char_codes) ~ cause_of_death_code_8,
         
                              (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 & icd10_3 != cause_of_death_code_9 & cause_of_death_code_9 %in% icd10_dementia) |
                                (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 & icd10_3 != cause_of_death_code_9 & cause_of_death_code_9 %in% dagger_code) |
                                (icd10_1 != cause_of_death_code_9 & icd10_2 != cause_of_death_code_9 & icd10_3 != cause_of_death_code_9 & cause_of_death_code_9 %in% fifth_char_codes) ~ cause_of_death_code_9,T~NA
         )) 
  


##prefix names
dementia_deaths <- dementia_deaths %>% 
  select(upi_number,date_of_birth, date_of_death, sex, postcode,institution,health_board_area, ethnicity_code,
         dementia_subtype_1, dementia_subtype_2, icd10_1, icd10_2, icd10_3, icd10_4) %>% 
  mutate(date_type = "date of death", source= "NRS deaths") 
###save dementia deaths extract####
saveRDS(dementia_deaths, paste0(folder_data_path, "/extracts/dementia_deaths.rds"))
