###########################################
###01e. Social care extract
## run the setup file first#
source("00.setup.r")
library(lubridate)
SC <- read_parquet(paste0(folder_data_path, "/extracts/Social_care_output.parquet"))

###infill dates where missing ####

##work out "quarter client" start date. 
##flag dates that are just start of 1/4 or of year
SC <- SC %>% 
  ##remove spurious dates
  mutate(start_date_of_care = case_when(year(start_date_of_care) < 1994 ~NA, T~ start_date_of_care)) %>%
  mutate(year_client_date = case_when(quarter_client %in% 1:3 ~ substr(financial_year,1,4), 
                                      quarter_client ==4 ~ paste0("20", substr(financial_year,6,7)))) %>%
  mutate(year_client_date = case_when(is.na(quarter_client) & financial_quarter_care_first %in% 1:3 ~ substr(financial_year,1,4), 
                                      is.na(quarter_client) & financial_quarter_care_first ==4 ~ paste0("20", substr(financial_year,6,7)), 
                                      T~year_client_date)) %>% 
  mutate(quarter_client_date =case_when(quarter_client==1 ~ as.Date(paste0(year_client_date, "-04-01")), 
                                        quarter_client==2 ~ as.Date(paste0(year_client_date, "-07-01")),
                                        quarter_client==3 ~ as.Date(paste0(year_client_date, "-10-01")),
                                        quarter_client==4 ~ as.Date(paste0(year_client_date, "-01-01")),
                                        T~NA)) %>%
  mutate(quarter_fin_date =case_when(financial_quarter_care_first==1 ~ as.Date(paste0(year_client_date, "-04-01")), 
                                     financial_quarter_care_first==2 ~ as.Date(paste0(year_client_date, "-07-01")),
                                     financial_quarter_care_first==3 ~ as.Date(paste0(year_client_date, "-10-01")),
                                     financial_quarter_care_first==4 ~ as.Date(paste0(year_client_date, "-01-01")),
                                     T~NA)) %>%
  
  mutate(diagnosis_date = 
           case_when(!is.na(start_date_of_care) & 
                       start_date_of_care >= as.Date("2018/01/01") ~ start_date_of_care, 
                     !is.na(quarter_client_date) ~quarter_client_date, 
                     !is.na(quarter_fin_date) ~ quarter_fin_date,
                     !is.na(financial_quarter_care_first) ~ quarter_fin_date,
                     is.na(financial_quarter_care_first) & is.na(quarter_client_date) &
                       (is.na(start_date_of_care) | start_date_of_care < as.Date("2018/01/01")) ~ 
                       as.Date(paste0("20", substr(financial_year,6,7), "-04-01")),  T~NA  )) %>% 
  mutate(date_type =  case_when(!is.na(start_date_of_care) & 
                                  start_date_of_care >= as.Date("2018/01/01") ~"start care date" , 
                                !is.na(quarter_client_date)| !is.na(financial_quarter_care_first) ~"quarter start date",
                                T ~ "start financial year"))

#table((SC$financial_quarter_care_first), is.na(SC$quarter_fin_date), useNA="always")
#table(is.na(SC$quarter_fin_date), is.na(SC$quarter_client))
#check_missing <- SC %>% filter(is.na(diagnosis_date))
#table(check_missing$financial_quarter_care_first)
#find first date for those with >1 record

check_chi <- SC %>% group_by(chi) %>% count()
table(check_chi$n)  

##POstcodes
# clean by removing NK postcodes and NF postcodes (placeholders)
# link simd to ID non-scottish postcodes in submitted or chi and replace with the other if the other is scottish 
SC<- SC %>%
  ##remove nk and nf
  mutate(submitted_postcode = case_when(substr(submitted_postcode,1,2)=="NK" ~NA,
                                        substr(submitted_postcode,1,2)=="NF" ~NA,
                                        T~ submitted_postcode)) %>% 
  mutate(ch_postcode = case_when(substr(ch_postcode,1,2)=="NK" ~NA,
                                        substr(ch_postcode,1,2)=="NF" ~NA,
                                        T~ ch_postcode))  %>%
  ##definitive postcode is the CH postcode if present, them submitted , then the chi postcode
  mutate(best_postcode= coalesce(ch_postcode, submitted_postcode, chi_postcode))
  
#table(SC$chi %in% PDS$chi_number, useNA="always")  
#table(lubridate::year(SC$start_date_of_care), SC$chi %in% PDS$chi_number, useNA="always")  
table(phsmethods::chi_check(SC$chi))



##chi - UPI lookup####
clear_temp_tables(SMRAConnection)
upis <- SMRAConnection %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(SC %>% rename(CHI_NUMBER = chi), copy = TRUE) %>%
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER,UPI_NUMBER, DATE_OF_BIRTH) %>%
  distinct() %>% 
  collect()

names(upis)
names(SC)
SC<- left_join(SC, upis, by=c("chi"="CHI_NUMBER"))
table(SC$chi[is.na(SC$UPI_NUMBER)])

SC <-SC %>% mutate(grouping_chi = case_when(!is.na(UPI_NUMBER) ~ UPI_NUMBER, T~chi))

### retain first social care care home record ####
#and first social care any other type###
SC_first <-SC %>% 
  mutate(type_care2 = case_when(type_of_care=="care home" ~ "care home", 
                                type_of_care=="not recorded" ~ "not recorded",
                                T~ "other care")) %>%
  group_by(grouping_chi, type_care2) %>%
  arrange(diagnosis_date) %>%
  slice(1) %>% 
  select(chi, chi_gender, UPI_NUMBER, diagnosis_date, chi_date_of_birth, chi_date_of_birth, best_postcode,
         submitted_ethnic_group, ch_admission_date, date_type) %>%
  mutate(dementia_type = "99 Social care flag") %>%
  rename(type_of_care_group = type_care2) %>% 
  rename(upi_number = grouping_chi) %>%
  select(-c(chi, UPI_NUMBER))


#SC_first <- SC_first %>%
#  rename_with(.cols = everything(), function(x){paste0("SC_", x)})
saveRDS(SC_first, "/PHI_conf/Dementia_Index/data/cleaned_extracts/SC_clean.rds")


