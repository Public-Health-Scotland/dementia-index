#create dictionary

library(labelled)
dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")
names(dementia_index)

var_label(dementia_index$source) <- "Source of diagnosis information"
var_label(dementia_index$upi_number) <- "Patient UPI number"
var_label(dementia_index$date_of_birth) <- "Date of birth"
var_label(dementia_index$sex) <- "Sex: 1= Male, 2=Female"
var_label(dementia_index$diagnosis_2) <- "Second diagnosis from same source and date"
var_label(dementia_index$ca2019) <- "Council area (2019 version) of residence at diagnosis date "
var_label(dementia_index$hbres) <- "Healthboard of residence at diagnosis date "
var_label(dementia_index$ch_postcode) <- "Care home postcode (where applicable) "
var_label(dementia_index$SIMD_at_diag) <- "SIMD level based on postcode at diagnosis. Not applicable if in carehome at time of diagnosis"
var_label(dementia_index$ch_admission_date) <- "Care home admission date (where applicable)"
var_label(dementia_index$type_of_care) <- "Social care source only: type of social care recieved"
var_label(dementia_index$type_of_care_group) <- "Social care source only: type of social care recieved grouped as carehome or other"
var_label(dementia_index$CHC_last_discharge) <- "Care home census only: last recorded discharge from carehome on care home census"
var_label(dementia_index$ethnic_group) <- "Ethnic group as recorded on original source."
var_label(dementia_index$diagnosis_date) <- "Date of diagnosis: diagnosis date where avalible; where the source does not record a diagnosis date, the date of admission or date of the start of the episode where dementia is recorded is used"
var_label(dementia_index$start_date_of_care) <- "Social care only: date of start of social care"
var_label(dementia_index$end_date_of_care) <- "Social care only: date of end of social care episode"
var_label(dementia_index$date_type) <- "Where the diagnosis date is derived from e.g. admission date of hospital episode"

dictionary <-generate_dictionary(dementia_index)
