##############################################################
## this script checks and cleans the PDS extract           ###
## minimal changes required as PDS is already fairly clean ##
#############################################################
## run the setup file first#

##get data
pdsextract <- 
  readRDS("/PHI_conf/Dementia_Index/data/extracts/pds_dementia_index_extract-2024_25-Q2.rds")
names(pdsextract)

#check only one record per chi
check_chi <- pdsextract %>% group_by(chi_number) %>% count()
table(check_chi$n)

##remove missing chi
pdsextract  <-pdsextract   %>% filter(!is.na(chi_number))

###Remove placeholder postcodes
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


saveRDS(pdsextract, "/PHI_conf/Dementia_Index/data/extracts/PDS_clean.rds")
