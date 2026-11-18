###############################################################################
## Ad-hoc: Everyone in the Dementia Index who received social care from 2018
##
## Customer ask: "a list of everyone who is in the index who received social
## care from 2018 onwards, with their postcode, start/end date, client group
## and the type of care they received if possible."
##
## Social care comes from the Source Linkage File (SLF) episode files via
## {slfhelper}, which hold all social care clients (not just those flagged
## with dementia).
##
###############################################################################

library(tidyverse)
library(lubridate)
library(slfhelper)

## Parameters #################################################################

folder_data_path <- "/PHI_conf/Dementia_Index/data"
di_path          <- paste0(folder_data_path, "/INDEX/dementia_index.rds")
output_folder    <- paste0(folder_data_path, "/ad-hoc/SLF check for Susan McMillan")

## "From 2018 onwards": any care episode still running on or after this date
start_date <- as.Date("2018-01-01")

## SLF year files to read. 1718 is included because it covers Jan-Mar 2018.
## Add the latest year as new files become available.
slf_years <- c("1718", "1819", "1920", "2021", "2122", "2223", "2324", "2425")

## Social care record types in the SLF and their labels
care_types <- c(HC  = "Home care",
                CH  = "Care home",
                AT  = "Alarms/telecare",
                SDS = "Self-directed support")

## Social care client group flags in the SLF and their labels
## (sc_dementia is the social care client group, NOT the health-derived
## `dementia` LTC flag)
client_groups <- c(
  sc_dementia                        = "Dementia",
  sc_elderly_frail                   = "Elderly/frail",
  sc_learning_disability             = "Learning disability",
  sc_mental_health_disorders         = "Mental health",
  sc_physical_and_sensory_disability = "Physical and sensory disability",
  sc_neurological_condition          = "Neurological condition",
  sc_autism                          = "Autism",
  sc_palliative_care                 = "Palliative care",
  sc_carer                           = "Carer",
  sc_drugs                           = "Drugs",
  sc_alcohol                         = "Alcohol",
  sc_other_vulnerable_groups         = "Other vulnerable groups"
)

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)


## 1. People in the Dementia Index ############################################

di <- readRDS(di_path) %>%
  ungroup() %>%
  filter(!is.na(diagnosis_date)) %>%
  group_by(chi = upi_number) %>%
  summarise(di_first_diagnosis_date = min(diagnosis_date),
            di_sources = paste(sort(unique(source)), collapse = "; "),
            .groups = "drop")


## 2. Social care records from the SLF ########################################

sc <- read_slf_episode(
  slf_years,
  recids = names(care_types),
  col_select = c("year", "recid", "anon_chi", "record_keydate1", "record_keydate2",
                 "postcode", "ch_postcode", "ch_name", "sc_send_lca",
                 "social_care_id",
                 names(client_groups))
) %>%
  get_chi() %>%                                  # anon_chi -> chi
  filter(chi %in% di$chi) %>%                    # people in the index only
  filter(is.na(record_keydate2) | record_keydate2 >= start_date)

## Sending local authority name
la_lookup <- sc_partnerships %>% distinct(lca, local_authority = partnership_name)

sc <- sc %>%
  left_join(la_lookup, by = c("sc_send_lca" = "lca")) %>%
  mutate(type_of_care = unname(care_types[recid]),
         local_authority = coalesce(local_authority, sc_send_lca))

## Client group: one text column listing every group flagged, e.g.
## "Dementia; Elderly/frail". In the SLF the flags are factors: Yes / No / NA.
##   "None flagged" = all groups answered No
##   "Not recorded" = all groups NA (no client group returned)
sc <- sc %>%
  mutate(client_group_dementia = as.character(sc_dementia),   # keep Yes/No/NA
         client_group_missing  = if_all(all_of(names(client_groups)), is.na)) %>%
  mutate(across(all_of(names(client_groups)),
                ~ toupper(str_trim(as.character(.x))) %in%
                  c("1", "1.0", "Y", "YES", "TRUE", "T"))) %>%
  rowwise() %>%
  mutate(client_group = paste(client_groups[names(client_groups)[
    c_across(all_of(names(client_groups)))]], collapse = "; ")) %>%
  ungroup() %>%
  mutate(client_group = case_when(client_group_missing ~ "Not recorded",
                                  client_group == ""   ~ "None flagged",
                                  TRUE                 ~ client_group))


## 3. One row per care episode ################################################
## An episode spanning several financial years appears in each year's SLF
## file. Keep one row per person / care type / LA / start date, taking the
## postcode and client group from the latest year.

sc_episodes <- sc %>%
  arrange(chi, recid, local_authority, record_keydate1, year) %>%
  group_by(chi, type_of_care, local_authority, care_start_date = record_keydate1) %>%
  summarise(
    care_end_date      = if (any(is.na(record_keydate2))) as.Date(NA) else max(record_keydate2),
    social_care_id     = last(social_care_id),
    postcode           = last(postcode),
    care_home_postcode = last(ch_postcode),
    care_home_name     = last(ch_name),
    client_group       = last(client_group),
    client_group_dementia = last(client_group_dementia),
    .groups = "drop"
  )


## 4. Output ##################################################################

## Identifiers, then episode details, then the person they belong to
output <- sc_episodes %>%
  left_join(di, by = "chi") %>%
  arrange(chi, care_start_date, type_of_care) %>%
  group_by(chi) %>%
  mutate(person_episode_number = row_number(),
         person_total_episodes = n()) %>%
  ungroup() %>%
  mutate(care_ongoing = if_else(is.na(care_end_date), "Yes", "No")) %>%
  select(
    ## identifiers
    chi, social_care_id,
    ## the episode
    type_of_care, local_authority,
    care_start_date, care_end_date, care_ongoing,
    client_group, client_group_dementia,
    postcode, care_home_name, care_home_postcode,
    ## the person
    person_episode_number, person_total_episodes,
    di_first_diagnosis_date, di_sources
  )

output_file <- file.path(output_folder, "SLF check for Susan McMillan.csv")
write_csv(output, output_file, na = "")
saveRDS(output, file.path(output_folder, "SLF check for Susan McMillan.rds"))

## Quick checks
cat("People in index:", n_distinct(di$chi), "\n")
cat("People in index with social care from", format(start_date), ":",
    n_distinct(output$chi), "\n")
cat("Care episodes:", nrow(output), "\n")
output %>% count(type_of_care)
output %>% count(client_group_dementia)
