##############################################################
## this script checks and cleans the PDS extract           ###
## minimal changes required as PDS is already fairly clean ##
#############################################################
## run the setup file first#
source("00.setup.r")
##get data
pdsextract <- 
  readRDS("/PHI_conf/Dementia_Index/data/extracts/pds_dementia_index_extract-2025_26-Q2.rds")
names(pdsextract)

#check only one record per chi
check_chi <- pdsextract %>% group_by(chi_number) %>% count()
table(check_chi$n)
table(phsmethods::chi_check(pdsextract$chi_number))
##remove missing chi
pdsextract  <-pdsextract %>% filter(!is.na(chi_number)) %>%
  mutate(chi_number = phsmethods::chi_pad(chi_number)) %>%
  mutate(valid_chi= phsmethods::chi_check(chi_number)) %>%
  filter(valid_chi=="Valid CHI") %>%
  select(-valid_chi) 

###Remove placeholder postcodes
#table(substr(pdsextract$postcode,1,2))
pdsextract  <-pdsextract %>% mutate(postcode = case_when(postcode=="NK010AA"~NA , T~postcode))

###There are some NAs in the subtype field. Replace with 07 yet to be determined.
#there are no missing dates of diagnosis, so there is no suggestion that those missing 
# a subtype are not diagnosed.
##timing of NAs - more towards earlier years.
#table(pdsextract$subtype_of_dementia,
#      lubridate::year(pdsextract$dementia_diagnosis_confirmed_date), useNA="always")

pdsextract  <-pdsextract %>% 
  mutate(subtype_of_dementia = case_when(is.na(subtype_of_dementia) ~"07 Yet to be determined", 
                                         T~subtype_of_dementia))
pdsextract  <-pdsextract %>% mutate(source= "PDS")
names(pdsextract)
pdsextract  <-pdsextract %>%
  rename(diagnosis_date  =  dementia_diagnosis_confirmed_date,
         dementia_subtype = subtype_of_dementia) %>%
  rename_with(.cols = everything(), function(x){paste0("pds_", x)})

saveRDS(pdsextract, "/PHI_conf/Dementia_Index/data/cleaned_extracts/PDS_clean.rds")
