###Care home census####
##We get this extract form th chc team###

#
library(readxl)
library(dplyr)
library(hablar)
CareHomeCensus <- read_excel("/PHI_conf/Dementia_Index/data/extracts/dementia_CareHomeCensus_2017_18_to_2023_24.xlsx")
View(CareHomeCensus)


table(CareHomeCensus$DementiaMD)
table(CareHomeCensus$DementiaNMD)
table(CareHomeCensus$EthnicOrigin)
table(CareHomeCensus$Sex)
table(is.na(CareHomeCensus$UPI_NUMBER))
table(CareHomeCensus$DementiaNMD, CareHomeCensus$DementiaMD, useNA="always")
#Clean up variables with inconsistent values

CareHomeCensus <-CareHomeCensus %>%
  mutate(DementiaMD = case_when(DementiaMD=="yes" ~ "Yes", T~DementiaMD)) %>%
  mutate(DementiaNMD = case_when(DementiaNMD=="yes" ~ "Yes", T~DementiaNMD)) %>%
  mutate(Sex = case_when(Sex %in% c("Blank", "Not Known", "Other" ) ~"Unknown", 
                                    T~Sex)) %>%
  mutate(EthnicOrigin = case_when(EthnicOrigin %in% c("Blank", "Not Known", "Not Disclosed") ~"Unknown", 
                                  T~EthnicOrigin)) %>%
  ##Add corresponding code to the ethnic origin description
  mutate(EthnicOriginCode = case_when(EthnicOrigin== "African, Caribbean or Black" ~ "3", 
                                      EthnicOrigin== "Asian, Asian Scottish or Asian British" ~ "2",
                                      EthnicOrigin== "Mixed or Multiple Ethnic Group" ~ "4",
                                      EthnicOrigin== "Other Ethnic Background" ~ "4",
                                      EthnicOrigin== "White" ~ "1",
                                      EthnicOrigin=="Unknown" ~NA, T~NA))


##Find first record per person with NMD and MD dementia
CHC_agg <- CareHomeCensus %>%
  filter(!is.na(UPI_NUMBER)) %>%
  arrange(UPI_NUMBER,DateOfAdmission) %>%
  group_by(UPI_NUMBER, DementiaMD, DementiaNMD) %>%
  summarise(first_admission = min_(DateOfAdmission), 
            last_discharge = max_(DateOfDischarge),
            DateOfBirth=first_(DateOfBirth), 
            Sex = first_(Sex), 
            EthnicOriginCode = first_(EthnicOriginCode), 
            CareHomePostcode = first_(CareHomePostcode)) %>%
  mutate(dementia_type = case_when(DementiaMD =="Yes" ~ "Dementia unspecified", 
                                   DementiaNMD =="Yes" ~ "Suspected dementia", T~NA )) 

##split into MD and NMD - if both are yes then we just want MD
CHC_NMD <-CHC_agg %>%
  filter(DementiaNMD=="Yes" & DementiaMD=="No")
CHC_MD <-CHC_agg %>%
  filter(DementiaMD=="Yes")

##prefix names and left join

##CHeck the dates. If the NMD diag is before the MD diag, we want to know the NMD date as well, if its after, drop it
