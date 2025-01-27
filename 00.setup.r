##Setup file####
##Setup filespaths libraries and code lists 
## libraries ###
library(renv)
library(here)
library(odbc)
library(tidyverse)
library(janitor)
library(hablar)

folder_data_path <- "/PHI_conf/Dementia_Index/data"

# connect to SMRA####


keyring::keyring_unlock(keyring = "DATABASE",
                        password = source("~/database_keyring.R")[["value"]])

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], # Assumes the user's SMR01 username is the same as their R server username
                            pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))


##code lists####

icd10_dementia <- c("F00", "F000", "F001", "F002", "F009",
                    "F01","F010", "F011", "F012",  "F013","F018","F019",
                    "F02","F020", "F021", "F022",  "F023", "F024","F028",
                    "F03",
                    "F051")
dagger_code <- c("G30", "G301", "G308", "G309")
fifth_char_codes <- c("G318 D","F028 A", "F1073",  "F1173",  "F1273",  "F1373",  "F1473",  "F1573",  "F1673",  "F1773", "F1873",  "F1973"  )

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