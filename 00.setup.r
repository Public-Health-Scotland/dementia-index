##Setup file####
##Setup filespaths libraries and code lists 
## libraries ###
library(renv)
library(here)
library(odbc)
library(tidyverse)
library(janitor)
library(hablar)
library(readxl)
library(arrow)
library(odbc)
library(lubridate)


folder_data_path <- "/PHI_conf/Dementia_Index/data"
cohort_start_date <- as.Date("2014-01-01")
# connect to SMRA####
#using keyring
##this will only work if you have keyring setup for SMRA
# and have the password to the keyring (NOT your smra password)
# saved in the file "~/database_keyring.R"
keyring::keyring_unlock(keyring = "DATABASE",
                        password = source("~/database_keyring.R")[["value"]])

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], # Assumes the user's SMR01 username is the same as their R server username
                            pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))
# pwd = .rs.askForPassword("Password:"))
#Scottish postcode directory
SPD <- readRDS("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds")
geogs_lookup <- SPD %>% select(pc7, ca2019, ca2019name, hb2019, hb2019name, ur6_2022, ur6_2022_name, ur8_2022, ur8_2022_name)

##simd lookup
postcode_simd_carstairs <- readRDS("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2025_1_all_simd_carstairs.rds")
simd_lookup <- postcode_simd_carstairs %>%
  select(pc7, simd2020v2_sc_quintile,simd2020v2_sc_decile, simd2016_sc_quintile,
         simd2016_sc_decile, simd2012_sc_quintile, simd2012_sc_decile)
rm(postcode_simd_carstairs)
##code lists####

icd10_dementia <- c("F00", "F000", "F001", "F002", "F009",
                    "F01","F010", "F011", "F012",  "F013","F018","F019",
                    "F02","F020", "F021", "F022",  "F023", "F024","F028",
                    "F03", "F051","G318 D","F028 A", "F1073",  "F1173",  
                    "F1273",  "F1373",  "F1473",  "F1573",  "F1673",  "F1773", "F1873",  "F1973")
dagger_code <- c("G30", "G301", "G308", "G309")
fifth_char_codes <- c("G318 D","F028 A", "F1073",  "F1173",  "F1273",  "F1373",  
                      "F1473",  "F1573",  "F1673",  "F1773", "F1873",  "F1973"  )
alcohol_code <-"F1073"
##dementia due to substaces other than alcohol.
substance_codes <- c(  "F1173",  "F1273",  "F1373",  "F1473",  "F1573",  "F1673", 
                       "F1773", "F1873",  "F1973"  )

##To id type
dementia_unspecified <- c("F03X", "F051")
alzheimer_codes <- c("F00", "F000", "F001", "F002", "F009")
lewy_body_g <- "G318 D"
lewy_body_f <- "F028 A"
vascular_dementia <- c("F01")
dementia_other_dis <- c("F02")
dementia_picks <- "F020"
dementia_cjd <- "F021"
dementia_hunting <- "F022"
dementia_parkinson <- "F023"
dementia_hiv <- "F024"
bnf <- "0411"

##functions####

clear_temp_tables <- function(conn){
  # clears old dbplyr tables off the SQL database where possible
  tables <- odbc::dbListTables(conn)
  for(table in tables[str_sub(tables, 1, 7) == "dbplyr_"]){try(odbc::dbRemoveTable(conn, table), silent = TRUE)}
}
