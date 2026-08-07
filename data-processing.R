# ============================================================
# SCRIPT: data-processing.R
# DESCRIPTION: Reads, cleans, and tidies all raw data into
#              analysis-ready dataframes for the study.
# ============================================================


# 1. PACKAGES ---------------------------------------------------------------

library(readr)
library(tidyverse)
library(lubridate)
library(fs)
library(here)
library(tibbletime)
library(dplyr)
library(smooth)
library(sf)
library(stringr)


# 2. READ DATA --------------------------------------------------------------

## 2.1 BC monitoring raw data (MA200) --------------------------------------

raw_data_bc           <- dir_ls("data-raw/ma200/")
raw_data_files_bc     <- list.files("data-raw/ma200/")
raw_data_csv_bc       <- str_subset(raw_data_bc, pattern = ".csv$")
raw_data_csv_paths_bc <- path(here::here(raw_data_csv_bc))
raw_data_list_bc      <- list()

## 2.2 Survey data ---------------------------------------------------------

survey <- read_delim(
  "data-raw/pem-study_-_all_versions_-_False_-_2024-02-21-09-56-32.csv",
  delim = ";"
)


# 3. TIDY SURVEY DATA -------------------------------------------------------

survey_tidy <- survey |>
  rename(
    lat      = `_location_latitude`,
    long     = `_location_longitude`,
    altitude = `_location_altitude`
  ) |>
  arrange(hh_id) |>
  rowwise() |>
  mutate(
    total_waste = sum(
      mass_wood_and_leaves,
      mass_plastics,
      mass_textiles,
      mass_cardboard_and_paper,
      mass_of_others
    )
  )

## 3.1 Select and format key variables -------------------------------------

data_structure_pem24 <- survey_tidy |>
  select(
    hh_id,
    MA200_serial_number_burner,
    MA200_serial_number_non_burner,
    alphasense_serial_number_burner,
    alphasense_serial_number_non_burner,
    date,
    pem_start_time,
    burning_start_time,
    burning_end_time,
    pem_end_time,
    PEM_end_date,
    lat,
    long,
    total_waste,
    mass_wood_and_leaves,
    mass_plastics,
    mass_textiles,
    mass_cardboard_and_paper,
    mass_of_others
  ) |>
  # Use local Malawi time (strip UTC offset)
  mutate(
    pem_start_time     = sub("\\+02:00", "", pem_start_time),
    pem_end_time       = sub("\\+02:00", "", pem_end_time),
    burning_start_time = sub("\\+02:00", "", burning_start_time),
    burning_end_time   = sub("\\+02:00", "", burning_end_time)
  ) |>
  arrange(hh_id)

## 3.2 Correct known data-entry mistakes -----------------------------------
# Reasons for mistakes:
#   - Data was first noted in a notebook, then entered into KoBoToolbox
#   - AM was often noted instead of PM
#   - Careful verification was performed:
#       * Burning time cannot precede PEM start time
#       * Burning time cannot fall in the middle of the night
#
# Specific corrections:
#   hh_id  2: Wrong PEM start time → correct is 9:17 AM
#   hh_id 11: Wrong burning end time → correct is 3 PM (not 3 AM)
#   hh_id 13: Wrong PEM start time → correct is 3:39 PM (not 3:39 AM)
#   hh_id 16: PEM end time and burning end time corrected
#   hh_id 22: Burning end time corrected

data_structure_pem24_correct <- data_structure_pem24 |>
  mutate(
    pem_start_time = ifelse(hh_id == 2,  "09:17:00.000", pem_start_time),
    pem_start_time = ifelse(hh_id == 13, "15:39:00.000", pem_start_time),
    burning_end_time = ifelse(hh_id == 11, "15:00:00.000", burning_end_time),
    pem_end_time     = ifelse(hh_id == 16, "14:15:00.000", pem_end_time),
    burning_end_time = ifelse(hh_id == 16, "13:25:00.000", burning_end_time),
    burning_end_time = ifelse(hh_id == 22, "19:06:00.000", burning_end_time)
  ) |>
  # Correct mistakenly exchanged burner/non-burner serial numbers:
  #   hh_id 6: One box had an OPC (fixed), but the MA200 was moved for battery
  #   recharge. The MA200 was returned to the wrong box (wrong denomination).
  mutate(
    MA200_serial_number_burner     = ifelse(hh_id == 6, "MA200-0416", MA200_serial_number_burner),
    MA200_serial_number_non_burner = ifelse(hh_id == 6, "MA200-0420", MA200_serial_number_non_burner)
  ) |>
  # After hh_id 12, monitoring times changed:
  #   Phase 1 (hh_id 1–12):  PEM start 9 AM–12 PM, end 5–7 AM next day
  #   Phase 2 (hh_id 13+):   PEM start 12 PM–5 PM, end 12 PM–5 PM same day
  # In Phase 2, burning often occurred the next calendar day.
  # Burning end dates are manually assigned for affected households.
  mutate(
    burning_date = format(date, "%Y-%m-%d"),
    burning_date = ifelse(
      hh_id %in% c(13, 14, 16, 18, 19, 23),
      format(PEM_end_date, "%Y-%m-%d"),
      burning_date
    ),
    burning_date = as.Date(burning_date)
  ) |>
  # Remove hh_id 8 and 18:
  #   hh_id  8: Raw data file for monitor 0416 shows internal date 2023-11-12,
  #             but the filename suggests 2023-11-21 — likely a software error.
  #             Conflicts with hh_id 1 data (same monitor, same date). Excluded
  #             to protect the integrity of hh_id 1 analysis.
  #   hh_id 18: Excluded for data quality reasons.
  filter(!hh_id %in% c(8, 18))


# 4. TIDY MA200 DATA --------------------------------------------------------

## 4.1 Read and combine all raw MA200 CSV files ----------------------------

for (i in seq_along(raw_data_csv_bc)) {
  data <- read_csv(file = here::here(raw_data_csv_bc)[[i]]) |>
    mutate(file_name = raw_data_files_bc[[i]]) |>
    distinct()
  
  raw_data_list_bc[[i]] <- data
}

raw_data_all_bc <- raw_data_list_bc |>
  bind_rows() |>
  distinct()

## 4.2 Select and rename required variables --------------------------------

raw_data_subset_bc <- raw_data_all_bc |>
  select(
    "Serial number",
    "Session ID",
    "Date local (yyyy/MM/dd)",
    "Time local (hh:mm:ss)",
    "GPS lat (ddmm.mmmmm)",
    "GPS long (dddmm.mmmmm)",
    ends_with(" BCc"),
    "IR BCc smoothed  (ng/m^3)"
  ) |>
  rename(
    serial_number = "Serial number",
    session_id    = "Session ID",
    date          = "Date local (yyyy/MM/dd)",
    time          = "Time local (hh:mm:ss)",
    lat           = "GPS lat (ddmm.mmmmm)",
    long          = "GPS long (dddmm.mmmmm)",
    ir_bcc        = "IR BCc smoothed  (ng/m^3)",
    ir_bcc_raw    = "IR BCc"
  ) |>
  mutate(
    date_time = ymd_hms(paste(date, time)),
    ir_bcc    = ir_bcc / 1000,                                          # Convert ng/m³ → µg/m³
    day_type  = ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
                       "weekend", "weekday")
  ) |>
  drop_na(ir_bcc) |>
  arrange(date_time)


# 5. PREPARE TIBBLETIME OBJECTS & MONITOR SPLITS ----------------------------

## 5.1 Full BC dataset as tbl_time -----------------------------------------

FB <- as_tbl_time(raw_data_subset_bc, index = date_time) |>
  arrange(date_time)

## 5.2 Build PEM data structure (start/end times) --------------------------

data_structure <- data_structure_pem24_correct |>
  as_tbl_time(index = date) |>
  mutate(
    pem_start_time = ymd_hms(paste(date, pem_start_time)),
    pem_end_time   = ymd_hms(paste(PEM_end_date, pem_end_time))
  ) |>
  arrange(pem_start_time)

## 5.3 Long-format monitor serial number lookup ----------------------------

ds_MA200 <- data_structure |>
  select(hh_id, MA200_serial_number_non_burner, MA200_serial_number_burner) |>
  rename(
    burner     = MA200_serial_number_burner,
    non_burner = MA200_serial_number_non_burner
  ) |>
  pivot_longer(
    cols      = c(burner, non_burner),
    names_to  = "type",
    values_to = "serial_number"
  )

## 5.4 Split raw data by monitor serial number -----------------------------

FB_0416 <- FB |>
  filter(serial_number == "MA200-0416") |>
  arrange(date_time) |>
  distinct()

FB_0420 <- FB |>
  filter(serial_number == "MA200-0420") |>
  arrange(date_time) |>
  distinct()


# 6. EXTRACT PEM-PERIOD DATA (FULL 24-HOUR WINDOW) -------------------------

df_0416 <- tibble()
df_0420 <- tibble()

for (i in seq_along(unique(data_structure$hh_id))) {
  
  # Assign FIRST, then mutate — test_0416 must exist before nrow() references it
  test_0416 <- FB_0416 |>
    filter_time(
      data_structure$pem_start_time[i] ~ data_structure$pem_end_time[i]
    )
  
  test_0416 <- test_0416 |>
    mutate(hh_id = rep(data_structure$hh_id[i], nrow(test_0416)))
  
  df_0416 <- df_0416 |> bind_rows(test_0416)
}

for (i in seq_along(unique(data_structure$hh_id))) {
  
  # Same fix for 0420
  test_0420 <- FB_0420 |>
    filter_time(
      data_structure$pem_start_time[i] ~ data_structure$pem_end_time[i]
    )
  
  test_0420 <- test_0420 |>
    mutate(hh_id = rep(data_structure$hh_id[i], nrow(test_0420)))
  
  df_0420 <- df_0420 |> bind_rows(test_0420)
}

## 6.1 Combined PEM dataset with burner/non-burner labels ------------------

df_bc_pem <- df_0416 |>
  bind_rows(df_0420) |>
  drop_na(ir_bcc) |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  distinct() |>
  mutate(ir_bcc = as.numeric(ir_bcc)) |>
  filter(!is.na(ir_bcc))

## 6.2 Summary: mean eBC per household per type (PEM period) --------------

df_bc_pem_summary_pre <- df_bc_pem |>
  group_by(hh_id, type, serial_number) |>
  summarise(
    count       = n(),
    mean_irbcc  = mean(ir_bcc, na.rm = TRUE),
    sd_irbcc    = sd(ir_bcc,   na.rm = TRUE),
    sum_irbcc   = sum(ir_bcc,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  drop_na()

df_bc_pem_summary <- df_bc_pem_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id", "type")) |>
  select(-serial_number, -count) |>
  pivot_wider(
    names_from  = type,
    values_from = c(mean_irbcc, sd_irbcc, sum_irbcc)
  ) |>
  drop_na()


# 7. EXTRACT OPEN WASTE BURNING (OWB) PERIOD DATA --------------------------

## 7.1 Build OWB data structure (burning start/end times) -----------------

data_structure_owb <- data_structure_pem24_correct |>
  as_tbl_time(index = burning_date) |>
  mutate(
    burning_start_time = ymd_hms(paste(burning_date, burning_start_time)),
    burning_end_time   = ymd_hms(paste(burning_date, burning_end_time))
  ) |>
  arrange(burning_start_time) |>
  mutate(
    total_hours = as.numeric(
      difftime(burning_end_time, burning_start_time, units = "hours")
    )
  )

# 7.2 Filter MA200 data to OWB windows ------------------------------------

df_0416_owb <- tibble()
df_0420_owb <- tibble()

for (i in seq_along(unique(data_structure_owb$hh_id))) {
  
  test_0416_owb <- FB_0416 |>
    filter_time(
      data_structure_owb$burning_start_time[i] ~ data_structure_owb$burning_end_time[i]
    )
  
  test_0416_owb <- test_0416_owb |>
    mutate(hh_id = rep(data_structure_owb$hh_id[i], nrow(test_0416_owb)))
  
  df_0416_owb <- df_0416_owb |> bind_rows(test_0416_owb)
}

for (i in seq_along(unique(data_structure_owb$hh_id))) {
  
  test_0420_owb <- FB_0420 |>
    filter_time(
      data_structure_owb$burning_start_time[i] ~ data_structure_owb$burning_end_time[i]
    )
  
  test_0420_owb <- test_0420_owb |>
    mutate(hh_id = rep(data_structure_owb$hh_id[i], nrow(test_0420_owb)))
  
  df_0420_owb <- df_0420_owb |> bind_rows(test_0420_owb)
}

## 7.3 Combined OWB dataset ------------------------------------------------

df_bc_owb <- df_0416_owb |>
  bind_rows(df_0420_owb) |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  distinct()

## 7.4 Summary: mean eBC per household per type (OWB period) --------------

df_bc_owb_summary_pre <- df_bc_owb |>
  group_by(hh_id, serial_number) |>
  summarise(
    count      = n(),
    mean_irbcc = mean(ir_bcc, na.rm = TRUE),
    sd_irbcc   = sd(ir_bcc,   na.rm = TRUE),
    sum_irbcc  = sum(ir_bcc,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  drop_na()

# hh_id retained here for downstream joins (e.g., df_waste_bc, df_bc_owb_ratio)
df_bc_owb_summary <- df_bc_owb_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  select(-serial_number, -count) |>
  pivot_wider(
    names_from  = type,
    values_from = c(mean_irbcc, sd_irbcc, sum_irbcc)
  ) |>
  drop_na()


# 8. EXTRACT NON-BURNING PERIOD DATA (PEM EXCLUDING OWB) -------------------

df_1 <- df_bc_pem |>
  select(hh_id, serial_number, date, time, ir_bcc, date_time, lat, long, type)

df_2 <- df_bc_owb |>
  select(hh_id, serial_number, date, time, ir_bcc, date_time, lat, long, type)

# Anti-join: keep only PEM rows that do NOT appear in the OWB window
df_unique <- anti_join(
  df_1, df_2,
  by = c("hh_id", "serial_number", "date", "time", "date_time", "type")
)

## 8.1 Summary: mean eBC per household per type (non-burning period) ------

df_unique_summary_pre <- df_unique |>
  group_by(hh_id, serial_number) |>
  summarise(
    count      = n(),
    mean_irbcc = mean(ir_bcc, na.rm = TRUE),
    sd_irbcc   = sd(ir_bcc,   na.rm = TRUE),
    sum_irbcc  = sum(ir_bcc,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  drop_na()

# hh_id retained here for downstream joins
df_unique_summary <- df_unique_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  select(-serial_number, -count) |>
  pivot_wider(
    names_from  = type,
    values_from = c(mean_irbcc, sd_irbcc, sum_irbcc)
  ) |>
  drop_na()


# 9. SURVEY + EXPOSURE COMBINED DATAFRAME -----------------------------------

## 9.1 Hours of burning per household --------------------------------------

df_hours_burning <- data_structure_owb |>
  select(total_hours, hh_id)

survey_tidy_filtered <- survey_tidy |>
  left_join(df_hours_burning, by = "hh_id") |>
  filter(!hh_id %in% c(8, 18))

## 9.2 Waste composition long format (for plotting) ------------------------

data_long <- survey_tidy_filtered |>
  pivot_longer(
    cols      = starts_with("mass_"),
    names_to  = "Waste_Type",
    values_to = "Mass"
  ) |>
  mutate(
    Waste_Type = recode(Waste_Type,
                        "mass_wood_and_leaves"    = "Organics",
                        "mass_plastics"           = "Plastics",
                        "mass_textiles"           = "Textiles",
                        "mass_cardboard_and_paper"= "Cardboard",
                        "mass_of_others"          = "Others"
    ),
    Settlement_type = recode(Settlement_type,
                             "formal"   = "Planned settlement",
                             "informal" = "Unplanned settlement"
    )
  )

## 9.3 Waste + BC exposure combined ----------------------------------------

df_waste_bc <- survey_tidy_filtered |>
  select(
    hh_id,
    starts_with("mass_"),
    total_waste,
    stirring_number,
    total_hours,
    waste_of_how_many_days,
    no_of_people
  ) |>
  left_join(df_bc_owb_summary, by = "hh_id") |>
  mutate(
    stir_and_mass_waste  = total_waste * stirring_number,
    e_bc_total           = mean_irbcc_burner * total_hours,
    waste_per_day        = total_waste / waste_of_how_many_days,
    waste_per_day_capita = waste_per_day / no_of_people
  )

## 9.4 Full survey + all exposure windows combined -------------------------

df_unique_summary_for_comb_survey <- df_unique_summary |>
  rename(
    mean_exc_burning_burner    = mean_irbcc_burner,
    mean_exc_burning_non_burner = mean_irbcc_non_burner
  ) |>
  select(hh_id, mean_exc_burning_burner, mean_exc_burning_non_burner)

df_owb_summary_for_comb_survey <- df_bc_owb_summary |>
  rename(
    mean_owb_burner     = mean_irbcc_burner,
    mean_owb_non_burner = mean_irbcc_non_burner
  ) |>
  select(hh_id, mean_owb_burner, mean_owb_non_burner)

df_pem_summary_for_comb_survey <- df_bc_pem_summary |>
  rename(
    mean_pem_burner     = mean_irbcc_burner,
    mean_pem_non_burner = mean_irbcc_non_burner
  ) |>
  select(hh_id, mean_pem_burner, mean_pem_non_burner)

df_survey_exposure <- survey_tidy_filtered |>
  left_join(df_unique_summary_for_comb_survey, by = "hh_id") |>
  left_join(df_owb_summary_for_comb_survey,    by = "hh_id") |>
  left_join(df_pem_summary_for_comb_survey,    by = "hh_id") |>
  mutate(
    diff_burning_time     = mean_owb_burner         - mean_owb_non_burner,
    diff_non_burning_time = mean_exc_burning_burner  - mean_exc_burning_non_burner,
    diff_pem_time         = mean_pem_burner          - mean_pem_non_burner
  )

## 9.5 Burner/non-burner exposure ratios -----------------------------------

df_bc_owb_ratio <- df_bc_owb_summary_pre |>
  left_join(survey_tidy,  by = "hh_id") |>
  left_join(ds_MA200,     by = c("serial_number", "hh_id")) |>
  select(hh_id, type, mean_irbcc, household_garden_waste, Settlement_type) |>
  pivot_wider(
    id_cols     = c(hh_id, household_garden_waste, Settlement_type),
    names_from  = type,
    values_from = mean_irbcc
  ) |>
  mutate(ratio = burner / non_burner)

df_bc_unique_ratio <- df_unique_summary_pre |>
  left_join(survey_tidy, by = "hh_id") |>
  left_join(ds_MA200,    by = c("serial_number", "hh_id")) |>
  select(hh_id, type, mean_irbcc, household_garden_waste, Settlement_type) |>
  pivot_wider(
    id_cols     = c(hh_id, household_garden_waste, Settlement_type),
    names_from  = type,
    values_from = mean_irbcc
  ) |>
  mutate(ratio = burner / non_burner)