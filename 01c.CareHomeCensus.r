###Care home census####
##We get this extract form th chc team###


CareHomeCensus <- read_excel("/PHI_conf/Dementia_Index/data/extracts/dementia_CareHomeCensus_2017_18_to_2023_24.xlsx")
View(CareHomeCensus)


#Clean up variables with inconsistent values
CareHomeCensus <-CareHomeCensus %>%
  filter(!is.na(UPI_NUMBER)) %>% 
  mutate(UPI_NUMBER = phsmethods::chi_pad(UPI_NUMBER)) %>% 
  mutate(valid_chi = phsmethods::chi_check(UPI_NUMBER)) %>%
  filter(valid_chi=="Valid CHI") %>%
  select(-valid_chi) %>%
  #remove records with dates that are not reliable
  #remove if dicharge before admisison
  filter(DateOfAdmission <= DateOfDischarge |is.na(DateOfDischarge)) %>% 
  #remve if DOB is duplicated in admission field
  filter(DateOfAdmission!=DateOfBirth | is.na(DateOfBirth)) %>%
  filter(DateOfDischarge >= as.Date("2014-01-01") | is.na(DateOfDischarge))


##filter out rows where admission is after discharge date
##and admission before 1990
CareHomeCensus <-CareHomeCensus %>%
  mutate(los_years = round((as.Date(DateOfDischarge)- as.Date(DateOfAdmission))/365.25,1)) %>% 
  filter(los_years >=0 | is.na(los_years)) %>%
  filter(DateOfAdmission >= as.Date("1994-01-01")) %>%
  filter(is.na(DateOfDischarge) | DateOfDischarge >= as.Date("1994-01-01"))


CareHomeCensus <-CareHomeCensus %>%
  filter(!is.na(UPI_NUMBER)) %>%
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


CHC_early <- CareHomeCensus %>% filter(DateOfAdmission <= as.Date("2013-12-31"))
CHC_late <- CareHomeCensus %>% filter(DateOfAdmission > as.Date("2013-12-31"))

table(CHC_early$UPI_NUMBER %in% CHC_late$UPI_NUMBER)
summary(CHC_early$DateOfDischarge)
table(is.na(CHC_early$DateOfDischarge))

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
            CareHomePostcode = first_(CareHomePostcode))%>% 
  ungroup()%>%
  mutate(dementia_subtype = case_when(DementiaMD =="Yes" ~ "07 Yet to be determined", 
                                   DementiaNMD =="Yes" ~ "99 Suspected dementia", T~NA )) 

##split into MD and NMD - if both are yes then we just want MD
CHC_NMD <-CHC_agg %>%
  filter(DementiaNMD=="Yes" & DementiaMD=="No") %>%
  select(-DementiaMD, -DementiaNMD)
CHC_MD <-CHC_agg %>%
  filter(DementiaMD=="Yes") %>%
  select(-DementiaMD, -DementiaNMD)

two_diags_nmd <- CHC_NMD %>% filter(UPI_NUMBER %in% CHC_MD$UPI_NUMBER)
two_diags_md <- CHC_MD %>% filter(UPI_NUMBER %in% CHC_NMD$UPI_NUMBER)

two_diags <- rbind(two_diags_md, two_diags_nmd)
CHC_MD <- CHC_MD %>% filter(!UPI_NUMBER %in% two_diags$UPI_NUMBER)
CHC_NMD <- CHC_NMD %>% filter(!UPI_NUMBER %in% two_diags$UPI_NUMBER)

##CHeck the dates. If the NMD diag is before the MD diag,
#we want to know the NMD date as well, if its after, drop it
two_diags <- two_diags %>% arrange(UPI_NUMBER, first_admission) %>%
  group_by(UPI_NUMBER) %>%
  mutate(drop = case_when(min_(first_admission)==max_(first_admission) &
                            dementia_subtype=="99 Suspected dementia" ~1, 
                          dementia_subtype=="99 Suspected dementia" & 
                            lag(dementia_subtype)=="07 Yet to be determined"~2)) 
two_diags <- two_diags %>% ungroup() %>% filter(is.na(drop))  %>% select(-drop)


all_CHC <- rbind(CHC_MD, CHC_NMD, two_diags)
##prefix names and left join

all_CHC <- all_CHC %>%
rename_with(.cols = everything(), function(x){paste0("CHC_", x)})

saveRDS(all_CHC, "/PHI_conf/Dementia_Index/data/cleaned_extracts/CHC_clean.rds")
