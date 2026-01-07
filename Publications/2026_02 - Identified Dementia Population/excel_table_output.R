library(tidyverse)
library(openxlsx)
library(readxl)
library(phsmethods) 

# ==============================================================================
# 1. LOAD DATA & SETUP
# ==============================================================================

dementia_index <- readRDS("/PHI_conf/Dementia_Index/data/INDEX/dementia_index.rds")
source_file_path <- "/PHI_conf/Dementia_Index/outputs/Publication 2026-02/chart_table_data_new.xlsx"

# ==============================================================================
# 2. EXTRACT EXISTING TABLES (Sections 1-6)
# ==============================================================================

extraction_config <- list(
  "df_1a" = list(sheet = "Scotland total - Rate", range = "A1:F2"), 
  "df_1b" = list(sheet = "Scotland total - Count", range = "A1:F2"), 
  "df_2"  = list(sheet = "Sex split - Rate", range = "A1:G3"),
  "df_3"  = list(sheet = "Health Boards - Rate", range = "A1:F16"),
  "df_4"  = list(sheet = "HSCP - Rate", range = "A1:F33"),
  "df_5"  = list(sheet = "Age split - Rate", range = "A1:G9"),
  "df_6"  = list(sheet = "Deprivation - Rate", range = "A1:G6")
)

read_table_data <- function(config_list, path) {
  map(config_list, function(conf) {
    message(paste("Reading", conf$sheet, "range", conf$range, "..."))
    data <- read_excel(path, sheet = conf$sheet, range = conf$range)
    colnames(data)[1] <- "Area"
    return(data)
  })
}

extracted_data <- read_table_data(extraction_config, source_file_path)

df_1a <- extracted_data$df_1a
df_1b <- extracted_data$df_1b
df_2  <- extracted_data$df_2
df_3  <- extracted_data$df_3
df_4  <- extracted_data$df_4
df_5  <- extracted_data$df_5
df_6  <- extracted_data$df_6

# ==============================================================================
# 3. CALCULATE NEW TABLES (Sections 7-8)
# ==============================================================================

#### 7: Annual new dementia cases ####
df_7 <- dementia_index %>%
  filter(!is.na(diagnosis_date)) %>%
  group_by(upi_number) %>%
  slice_min(diagnosis_date, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(financial_year = extract_fin_year(diagnosis_date)) %>%
  filter(financial_year %in% c("2020/21", "2021/22", "2022/23", "2023/24", "2024/25")) %>%
  count(financial_year, name = "n_people") %>%
  pivot_wider(names_from = financial_year, values_from = n_people, values_fill = 0) %>%
  mutate(Metric = "New Dementia Cases") %>%
  relocate(Metric)

#### 8: Distribution of dementia identified sources ####
df_8 <- dementia_index %>%
  select(diagnosis_date, source) %>%
  filter(!is.na(diagnosis_date)) %>%
  mutate(financial_year = extract_fin_year(diagnosis_date)) %>%
  filter(financial_year %in% c("2020/21", "2021/22", "2022/23", "2023/24", "2024/25")) %>%
  group_by(financial_year, source) %>%
  count() %>%
  pivot_wider(names_from = financial_year, values_from = n, values_fill = 0) %>%
  arrange(source)

# ==============================================================================
# 4. BUILD THE PHS STYLE WORKBOOK
# ==============================================================================

output_list <- list(
  "Prevalence Trend" = df_1a,
  "Count Trend" = df_1b,
  "Prevalence by Sex" = df_2,
  "NHS Board - Rate" = df_3,
  "HSCP - Rate" = df_4,
  "Age Group - Rate" = df_5,
  "SIMD - Rate" = df_6,
  "New Cases" = df_7,
  "Sources" = df_8
)

sheet_titles <- c(
  "People identified as living in Scotland with dementia, recorded prevalence EASR per 100,000; 2020/21 to 2024/25",
  "People identified as living in Scotland with dementia, recorded count of people",
  "Recorded dementia prevalence in Scotland by sex, EASR per 100,000; 2020/21 - 2024/25",
  "Recorded dementia prevalence by NHS boards, EASR per 100,000; 2024/25",
  "Recorded dementia prevalence by HSCP, EASR per 100,000; 2024/25",
  "Recorded dementia prevalence by Age Group; EASR per 100,000; 2024/25",
  "Recorded dementia prevalence by SIMD, EASR per 100,000; 2024/25",
  "Annual new dementia cases",
  "Distribution of dementia identified sources"
)



wb <- createWorkbook()
font_name <- "Arial"

# --- DEFINING STYLES EFFICIENTLY ---

# 1. Structural Styles
style_title  <- createStyle(fontName = font_name, fontSize = 14, textDecoration = "bold")
style_header <- createStyle(fontName = font_name, fontSize = 11, textDecoration = "bold", border = "Bottom")
style_text   <- createStyle(fontName = font_name, fontSize = 11) # For text columns like "Area"
style_notes  <- createStyle(fontName = font_name, fontSize = 10, wrapText = TRUE)

# 2. Data Styles (Both include commas)
# Rate: Comma + 1 decimal (e.g., 1,234.5)
style_rate   <- createStyle(fontName = font_name, fontSize = 11, numFmt = "#,##0.0")

# Count: Comma + 0 decimals (e.g., 1,234)
style_count  <- createStyle(fontName = font_name, fontSize = 11, numFmt = "#,##0")


# Add Notes Sheet
addWorksheet(wb, "Notes")
writeData(wb, "Notes", "Notes", startRow = 1); addStyle(wb, "Notes", style_title, rows = 1, cols = 1)
writeData(wb, "Notes", c("1. Data extract date:", "2. Source: PHS"), startRow = 4)

# Loop to Write Data Sheets
sheet_names <- names(output_list)

for(i in seq_along(output_list)) {
  
  sheet_name <- sheet_names[i]
  data_df <- output_list[[i]]
  title_text <- sheet_titles[i]
  
  addWorksheet(wb, sheet_name)
  
  # --- STEP 1: Write Data ---
  writeData(wb, sheet_name, data_df, startRow = 4)
  
  # --- STEP 2: Set Widths ---
  setColWidths(wb, sheet_name, cols = 1:ncol(data_df), widths = "auto")
  
  # --- STEP 3: Write Title ---
  writeData(wb, sheet_name, title_text, startRow = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)
  
  # --- STEP 4: Apply Styles ---
  
  # A. Header Style (Apply to Row 4)
  addStyle(wb, sheet_name, style_header, rows = 4, cols = 1:ncol(data_df))
  
  # B. Data Body Style (Apply to Rows 5+)
  # Define the grid for the data
  rows_idx <- 5:(4+nrow(data_df))
  data_cols_idx <- 2:ncol(data_df) # Assuming Col 1 is always text/labels
  
  # Logic: Check if it's a "Rate" sheet or a "Count" sheet
  if (grepl("Prevalence|Rate", sheet_name, ignore.case = TRUE)) {
    # It's a Rate sheet -> Use style_rate
    addStyle(wb, sheet_name, style_rate, rows = rows_idx, cols = data_cols_idx, gridExpand = TRUE)
  } else {
    # It's a Count/Source sheet -> Use style_count
    addStyle(wb, sheet_name, style_count, rows = rows_idx, cols = data_cols_idx, gridExpand = TRUE)
  }
  
  # C. Text Column Style (Apply to Column 1)
  # We always use the plain text style for the label column so it doesn't get number formatting
  addStyle(wb, sheet_name, style_text, rows = rows_idx, cols = 1, gridExpand = TRUE)
  
  # --- STEP 5: Footer ---
  footer_row <- 4 + nrow(data_df) + 2
  writeData(wb, sheet_name, "Source: Public Health Scotland", startRow = footer_row)
  addStyle(wb, sheet_name, style_notes, rows = footer_row, cols = 1)
}

# Save
saveWorkbook(wb, "Dementia_Index_Final_Output.xlsx", overwrite = TRUE)
message("Workbook created successfully!")