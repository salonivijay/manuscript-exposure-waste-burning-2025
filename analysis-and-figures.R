# ============================================================
# SCRIPT: analysis-and-figures.R
# DESCRIPTION: Statistical analysis and figure generation for
#              the open waste burning study.
#              Requires data-processing.R to be sourced first.
# ============================================================


# 1. PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggpmisc)
library(rstatix)
library(broom)
library(pwr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(grid)
library(smooth)
library(sf)
library(ggspatial)
library(basemapR)
library(rnaturalearth)
library(rnaturalearthdata)


# 2. SOURCE DATA PROCESSING -------------------------------------------------

source(here::here("data-processing.R"))


# 3. SHARED PLOT THEME ------------------------------------------------------

theme <- theme(
  axis.text.y      = element_text(size = 12),
  axis.text.x      = element_text(size = 12),
  axis.title.y     = element_text(size = 12),
  axis.title.x     = element_text(size = 12),
  strip.text       = element_text(size = 12),
  legend.text      = element_text(size = 12),
  legend.title     = element_text(size = 12),
  plot.title       = element_text(size = 12),
  plot.subtitle    = element_text(size = 12),
  plot.caption     = element_text(size = 12),
  panel.background = element_rect(fill = "transparent"),
  plot.background  = element_rect(fill = "transparent", color = NA),
  axis.line        = element_line(colour = "black"),
  panel.border     = element_rect(colour = "black", fill = NA, size = 0.5)
)


# 4. DESCRIPTIVE SUMMARIES --------------------------------------------------

## 4.1 Waste characterisation ----------------------------------------------

df_waste_char_summary <- df_waste_bc |>
  ungroup() |>
  summarise(
    median_waste_burnt           = median(waste_per_day),
    mean_waste_burnt             = mean(waste_per_day),
    sd_waste_burnt               = sd(waste_per_day),
    median_waste_burnt_per_capita = median(waste_per_day_capita),
    mean_waste_burnt_per_capita   = mean(waste_per_day_capita),
    sd_waste_burnt_per_capita     = sd(waste_per_day_capita)
  )

# Households burning more than 10 kg of waste
df_waste_bc |>
  ungroup() |>
  filter(total_waste > 10) |>
  summarise(n_households = n())

# Waste type breakdown by type and burning category
data_long |>
  group_by(Waste_Type) |>
  summarise(
    median = median(Mass),
    mean   = mean(Mass),
    count  = n()
  )

data_long |>
  group_by(Waste_Type, household_garden_waste) |>
  summarise(
    median = median(Mass),
    mean   = mean(Mass)
  )

# Type of waste by settlement type
type_of_waste <- survey_tidy_filtered |>
  group_by(household_garden_waste, Settlement_type) |>
  summarise(count = n())

type_of_waste_complete <- type_of_waste |>
  ungroup() |>
  tidyr::complete(
    household_garden_waste,
    Settlement_type,
    fill = list(count = 0)
  )

## 4.2 Socio-demographic summaries -----------------------------------------

# Gender breakdown by role
df_gender <- survey_tidy_filtered |>
  summarise(
    burner     = sex_burner,
    non_burner = sex_non_burner
  ) |>
  pivot_longer(cols = everything(), names_to = "role", values_to = "sex") |>
  count(sex, role) |>
  pivot_wider(names_from = role, values_from = n, values_fill = 0)

# Age group: burners
df_age_group_burner <- survey_tidy_filtered |>
  mutate(age_group = case_when(
    age_burner >= 18 & age_burner <= 29 ~ "18-29",
    age_burner >= 30 & age_burner <= 49 ~ "30-49",
    age_burner >= 50 & age_burner <= 64 ~ "50-64",
    age_burner >= 65                    ~ "65+"
  )) |>
  group_by(age_group) |>
  summarise(burner = n(), .groups = "drop")

# Age group: non-burners
df_age_group_non_burner <- survey_tidy_filtered |>
  mutate(age_group = case_when(
    age_non_burner >= 18 & age_non_burner <= 29 ~ "18-29",
    age_non_burner >= 30 & age_non_burner <= 49 ~ "30-49",
    age_non_burner >= 50 & age_non_burner <= 64 ~ "50-64",
    age_non_burner >= 65                        ~ "65+"
  )) |>
  group_by(age_group) |>
  summarise(non_burner = n(), .groups = "drop")

df_age_group <- df_age_group_burner |>
  left_join(df_age_group_non_burner, by = "age_group")

# Settlement type distribution
df_location <- survey_tidy_filtered |>
  count(Settlement_type) |>
  rename("No. of households" = "n")

# Median household size
df_people <- survey_tidy_filtered |>
  ungroup() |>
  summarise(median_household_size = median(no_of_people))

# Live/work status by role
df_live_burner <- survey_tidy_filtered |>
  group_by(live_work_burner) |>
  summarise("Burner" = n()) |>
  rename(live_work = live_work_burner)

df_live_non_burner <- survey_tidy_filtered |>
  group_by(live_work_non_burner) |>
  summarise("Non-burner" = n()) |>
  rename(live_work = live_work_non_burner)

df_live_work <- df_live_non_burner |>
  left_join(df_live_burner, by = "live_work")

# Presence at home during monitoring
survey_tidy_filtered |> group_by(burner_at_home)     |> summarise(count = n())
survey_tidy_filtered |> group_by(non_burner_at_home) |> summarise(count = n())
survey_tidy_filtered |> group_by(waste_of_how_many_days) |> summarise(count = n())

## 4.3 Monitoring hours summary --------------------------------------------

df_hrs_monitor <- df_bc_pem |>
  drop_na() |>
  group_by(hh_id, type) |>
  summarise(hours = n() / 120, .groups = "drop")

df_hrs_monitor |>
  ungroup() |>
  summarise(
    median = median(hours),
    mean   = mean(hours)
  )

## 4.4 Negative value diagnostics ------------------------------------------

# Apply centred moving averages of order 3, 5, and 7 to burner data
df_smooth_raw <- df_bc_pem |>
  filter(type == "burner")

list_df_raw <- list()

for (i in unique(df_smooth_raw$hh_id)) {
  
  list_df_raw[[i]] <- df_smooth_raw |>
    filter(hh_id == i) |>
    mutate(
      ir_bcc_3 = smooth::cma(ir_bcc_raw, order = 3, silent = TRUE)$fitted,
      ir_bcc_5 = smooth::cma(ir_bcc_raw, order = 5, silent = TRUE)$fitted,
      ir_bcc_7 = smooth::cma(ir_bcc_raw, order = 7, silent = TRUE)$fitted
    )
}

df_smooth_temp <- bind_rows(list_df_raw)

# Count negative values (%) before and after smoothing
df_negative_count_raw <- df_smooth_temp |>
  group_by(hh_id, type) |>
  summarise(
    neg_irbcc   = sum(ir_bcc_raw < 0) / n() * 100,
    neg_irbcc_3 = sum(ir_bcc_3  < 0) / n() * 100,
    neg_irbcc_5 = sum(ir_bcc_5  < 0) / n() * 100,
    neg_irbcc_7 = sum(ir_bcc_7  < 0) / n() * 100,
    .groups = "drop"
  ) |>
  mutate_if(is.numeric, round, digits = 1)

df_negative_count_smooth <- df_bc_pem |>
  drop_na() |>
  summarise(
    neg_irbcc     = sum(ir_bcc     < 0) / n() * 100,
    neg_irbcc_raw = sum(ir_bcc_raw < 0) / n() * 100,
    count         = n(),
    hours         = n() / 120
  ) |>
  mutate_if(is.numeric, round, digits = 1)

## 4.5 Burner vs non-burner difference during OWB --------------------------

# Number of households where burner exposure exceeded non-burner
df_bc_owb_summary |>
  ungroup() |>
  summarise(count = sum(mean_irbcc_burner > mean_irbcc_non_burner))

df_diff_burner_nonburner <- df_bc_owb_summary |>
  mutate(diff_burner_nonburner = mean_irbcc_burner - mean_irbcc_non_burner) |>
  select(diff_burner_nonburner) |>
  mutate_if(is.numeric, round, digits = 1)

## 4.6 Exposure summary statistics -----------------------------------------

# Non-burning period
df_bc_exc_owb_daily <- df_unique_summary |>
  ungroup() |>
  summarise(
    median_bc_burner_daily    = median(mean_irbcc_burner),
    iqr_burner                = IQR(mean_irbcc_burner),
    median_bc_non_burner_daily = median(mean_irbcc_non_burner),
    iqr_non_burner            = IQR(mean_irbcc_non_burner)
  )

# Full PEM period
df_bc_daily <- df_bc_pem_summary |>
  ungroup() |>
  summarise(
    median_bc_burner_daily    = median(mean_irbcc_burner),
    iqr_burner                = IQR(mean_irbcc_burner),
    median_bc_non_burner_daily = median(mean_irbcc_non_burner),
    iqr_non_burner            = IQR(mean_irbcc_non_burner)
  )

# OWB period
df_bc_owb_daily <- df_bc_owb_summary |>
  ungroup() |>
  summarise(
    median_bc_burner_daily    = median(mean_irbcc_burner),
    iqr_burner                = IQR(mean_irbcc_burner),
    median_bc_non_burner_daily = median(mean_irbcc_non_burner),
    iqr_non_burner            = IQR(mean_irbcc_non_burner)
  )

# Ratio summary (non-burning period)
df_bc_unique_ratio |>
  ungroup() |>
  summarise(
    mean = mean(ratio),
    IQR  = IQR(ratio)
  )

# Min/max summary (non-burning period)
df_unique_summary |>
  ungroup() |>
  summarise(
    max_burner       = max(mean_irbcc_burner),
    min_burner       = min(mean_irbcc_burner),
    max_non_burner   = max(mean_irbcc_non_burner),
    min_non_burner   = min(mean_irbcc_non_burner),
    median_burner    = median(mean_irbcc_burner),
    median_non_burner = median(mean_irbcc_non_burner)
  )


# 5. STATISTICAL TESTS ------------------------------------------------------

## 5.1 Normality tests (Shapiro-Wilk) -------------------------------------

# PEM period
shapiro.test(df_bc_pem_summary$mean_irbcc_burner)
shapiro.test(df_bc_pem_summary$mean_irbcc_non_burner)

# OWB period
# W = 0.82302, p-value = 0.0009163 → not normally distributed
shapiro.test(df_bc_owb_summary$mean_irbcc_burner)
# W = 0.80099, p-value = 0.0003974 → not normally distributed
shapiro.test(df_bc_owb_summary$mean_irbcc_non_burner)

# Non-burning period
shapiro.test(df_unique_summary$mean_irbcc_burner)
shapiro.test(df_unique_summary$mean_irbcc_non_burner)

## 5.2 Wilcoxon signed-rank tests (paired) --------------------------------

# PEM period
wilcox_test_result <- wilcox.test(
  df_bc_pem_summary$mean_irbcc_burner,
  df_bc_pem_summary$mean_irbcc_non_burner,
  paired = TRUE
)
print(wilcox_test_result)

# OWB period
# W = 430, p-value = 0.0001626 → burner significantly higher than non-burner
wilcox_test_owb <- wilcox.test(
  df_bc_owb_summary$mean_irbcc_burner,
  df_bc_owb_summary$mean_irbcc_non_burner,
  paired = TRUE
)
print(wilcox_test_owb)

# Non-burning period
wilcox_test_exc_burning <- wilcox.test(
  df_unique_summary$mean_irbcc_burner,
  df_unique_summary$mean_irbcc_non_burner,
  paired = TRUE
)
print(wilcox_test_exc_burning)

## 5.3 T-tests (for reference; data are non-normal — use with caution) ----

t.test(df_bc_pem_summary$mean_irbcc_burner,
       df_bc_pem_summary$mean_irbcc_non_burner,
       alternative = "two.sided", var.equal = TRUE)

t.test(df_bc_owb_summary$mean_irbcc_burner,
       df_bc_owb_summary$mean_irbcc_non_burner,
       alternative = "two.sided", var.equal = TRUE)

t.test(df_unique_summary$mean_irbcc_burner,
       df_unique_summary$mean_irbcc_non_burner,
       alternative = "two.sided", var.equal = TRUE)


# 6. EFFECT SIZES -----------------------------------------------------------

## 6.1 Wilcoxon effect sizes (r) -------------------------------------------

# Prepare per-period effect size inputs (exclude hh_id 21 — outlier)
df_bc_pem_effect_size <- df_bc_pem_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id", "type")) |>
  select(hh_id, type, mean_irbcc) |>
  filter(!hh_id %in% 21) |>
  distinct() |>
  group_by(hh_id, type) |>
  summarise(mean_irbcc = mean(mean_irbcc, na.rm = TRUE), .groups = "drop")

df_bc_owb_effect_size <- df_bc_owb_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  select(hh_id, type, mean_irbcc) |>
  filter(!hh_id %in% 21) |>
  distinct() |>
  group_by(hh_id, type) |>
  summarise(mean_irbcc = mean(mean_irbcc, na.rm = TRUE), .groups = "drop")

df_unique_effect_size <- df_unique_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  select(hh_id, type, mean_irbcc) |>
  filter(!hh_id %in% 21) |>
  distinct() |>
  group_by(hh_id, type) |>
  summarise(mean_irbcc = mean(mean_irbcc, na.rm = TRUE), .groups = "drop")

effect_size_pem    <- df_bc_pem_effect_size    |> wilcox_effsize(mean_irbcc ~ type, paired = TRUE)
effect_size_bc_owb <- df_bc_owb_effect_size    |> wilcox_effsize(mean_irbcc ~ type, paired = TRUE)
effect_size_unique <- df_unique_effect_size    |> wilcox_effsize(mean_irbcc ~ type, paired = TRUE)

print(effect_size_pem)
print(effect_size_bc_owb)
print(effect_size_unique)


# 7. POWER ANALYSIS ---------------------------------------------------------

## 7.1 Parametric power from effect size r (convert r → Cohen's d) --------

calculate_power_from_effsize <- function(effsize_result) {
  
  r <- effsize_result$effsize
  n <- effsize_result$n1       # Paired design: n1 = n2
  
  d <- (2 * r) / sqrt(1 - r^2)
  
  power <- pwr::pwr.t.test(
    n         = n,
    d         = d,
    sig.level = 0.05,
    type      = "paired"
  )$power
  
  effsize_result |>
    mutate(
      cohens_d = round(d, 3),
      power    = round(power, 3)
    )
}

effect_size_bc_owb |> calculate_power_from_effsize()
effect_size_unique |> calculate_power_from_effsize()
effect_size_pem    |> calculate_power_from_effsize()

## 7.2 Bootstrap simulation of Wilcoxon power -----------------------------

df_wide_bc_pem    <- df_bc_pem_effect_size    |> tidyr::pivot_wider(names_from = type, values_from = mean_irbcc)
df_wide_bc_owb    <- df_bc_owb_effect_size    |> tidyr::pivot_wider(names_from = type, values_from = mean_irbcc)
df_wide_bc_unique <- df_unique_effect_size    |> tidyr::pivot_wider(names_from = type, values_from = mean_irbcc)

set.seed(123)

sim_power_wilcox <- function(data, n_sim = 1000, alpha = 0.05) {
  pvals <- replicate(n_sim, {
    sample_data <- data |> sample_frac(replace = TRUE)
    wilcox.test(sample_data$burner, sample_data$non_burner, paired = TRUE)$p.value
  })
  mean(pvals < alpha)
}

power_wilcox_bc_pem    <- sim_power_wilcox(df_wide_bc_pem)
power_wilcox_bc_owb    <- sim_power_wilcox(df_wide_bc_owb)
power_wilcox_bc_unique <- sim_power_wilcox(df_wide_bc_unique)


# 8. PERMUTATION & BOOTSTRAP TESTS (EXPOSURE DIFFERENCES) ------------------

diff_vars <- c("diff_burning_time", "diff_non_burning_time", "diff_pem_time")

df_survey_exposure <- df_survey_exposure |>
  mutate(across(all_of(diff_vars), as.numeric))

# Sign-flip permutation test on the median
perm_test <- function(x, B = 10000, stat = median) {
  obs  <- stat(x, na.rm = TRUE)
  perm <- replicate(B, {
    signs <- sample(c(-1, 1), length(x), replace = TRUE)
    stat(x * signs, na.rm = TRUE)
  })
  tibble(
    observed  = obs,
    p_value   = mean(abs(perm) >= abs(obs)),
    null_dist = list(perm)
  )
}

# Bootstrap confidence interval on the median
boot_test <- function(df, x, B = 10000, stat = median) {
  boot_vals <- replicate(B, {
    idx <- sample(1:nrow(df), nrow(df), replace = TRUE)
    stat(df[[x]][idx], na.rm = TRUE)
  })
  tibble(
    boot_mean = mean(boot_vals),
    ci_low    = quantile(boot_vals, 0.025),
    ci_high   = quantile(boot_vals, 0.975),
    boot_dist = list(boot_vals)
  )
}

results <- map_dfr(diff_vars, function(v) {
  perm <- perm_test(df_survey_exposure[[v]], B = 10000)
  boot <- boot_test(df_survey_exposure, v,  B = 10000)
  bind_cols(tibble(outcome = v), perm, boot)
})

results |> select(outcome, observed, p_value, ci_low, ci_high)


# 9. EXPLORATORY REGRESSION & CORRELATION ANALYSIS -------------------------

## 9.1 Prepare modelling dataframe ----------------------------------------

df <- df_survey_exposure |>
  mutate(
    log_mean_owb_burner  = log(mean_owb_burner + 1),
    total_owb_burner     = mean_owb_burner * total_hours,
    log_total_owb_burner = log(total_owb_burner + 1),
    log_ratio            = log((mean_owb_burner + 1) / (mean_owb_non_burner + 1)),
    mass_cardboard_paper = mass_cardboard_and_paper,
    mass_others          = mass_of_others,
    burner_at_home       = as.factor(burner_at_home),
    Settlement_type      = as.factor(Settlement_type),
    sex_burner           = as.factor(sex_burner),
    live_work_burner     = as.factor(live_work_burner),
    household_garden_waste = as.factor(household_garden_waste)
  )

continuous_vars <- c(
  "total_hours", "total_waste", "stirring_number", "reignition_number",
  "waste_of_how_many_days", "mass_plastics", "mass_wood_and_leaves",
  "mass_textiles", "mass_cardboard_paper", "mass_others"
)

categorical_vars <- c(
  "burner_at_home", "Settlement_type", "sex_burner",
  "live_work_burner", "household_garden_waste"
)

## 9.2 Spearman correlations (continuous predictors) ----------------------

run_continuous <- function(outcome) {
  results <- data.frame()
  for (v in continuous_vars) {
    cat("\n====================\nOutcome:", outcome, "\nPredictor:", v, "\n")
    sp <- cor.test(df[[v]], df[[outcome]], method = "spearman", exact = FALSE)
    print(sp)
    results <- rbind(results, data.frame(
      outcome      = outcome,
      predictor    = v,
      spearman_rho = unname(sp$estimate),
      spearman_p   = sp$p.value
    ))
  }
  results
}

## 9.3 Wilcoxon/Kruskal + Spearman (categorical predictors) ---------------

run_categorical <- function(outcome) {
  results <- data.frame()
  for (v in categorical_vars) {
    cat("\n====================\nOutcome:", outcome, "\nPredictor:", v, "\n")
    df_tmp   <- df |> filter(!is.na(.data[[v]]))
    n_levels <- length(unique(df_tmp[[v]]))
    cat("Levels:", n_levels, "\n")
    group_p  <- NA
    
    if (n_levels == 2) {
      test    <- wilcox.test(as.formula(paste(outcome, "~", v)), data = df_tmp)
      print(test)
      group_p <- test$p.value
    } else if (n_levels > 2) {
      test    <- kruskal.test(as.formula(paste(outcome, "~", v)), data = df_tmp)
      print(test)
      group_p <- test$p.value
    } else {
      cat("Skipped (1 level)\n")
    }
    
    df_tmp[[v]] <- as.numeric(as.factor(df_tmp[[v]]))
    sp <- cor.test(df_tmp[[v]], df_tmp[[outcome]], method = "spearman", exact = FALSE)
    print(sp)
    
    results <- rbind(results, data.frame(
      outcome      = outcome,
      predictor    = v,
      spearman_rho = unname(sp$estimate),
      spearman_p   = sp$p.value,
      group_test_p = group_p
    ))
  }
  results
}

## 9.4 Run for all three outcomes ------------------------------------------

# A. Mean exposure intensity
cont_mean <- run_continuous("log_mean_owb_burner")
cat_mean  <- run_categorical("log_mean_owb_burner")

# B. Total exposure dose
cont_total <- run_continuous("log_total_owb_burner")
cat_total  <- run_categorical("log_total_owb_burner")

# C. Burner/non-burner ratio
cont_ratio <- run_continuous("log_ratio")
cat_ratio  <- run_categorical("log_ratio")

all_results <- bind_rows(cont_mean, cat_mean, cont_total, cat_total, cont_ratio, cat_ratio)

write.csv(all_results, "final_exposure_results_all_outcomes.csv", row.names = FALSE)
print(all_results)

## 9.5 Linear models (exploratory) ----------------------------------------

lm(e_bc_total ~ total_waste + stirring_number, data = df_waste_bc)
lm(e_bc_total ~ total_waste * stirring_number, data = df_waste_bc)


# 10. FIGURES ---------------------------------------------------------------

## 10.1 Study area map [Figure 1] -----------------------------------------

# --- Left panel: Africa map with Malawi highlighted and arrow to Blantyre ---

malawi <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(admin == "Malawi")

africa <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(region_un == "Africa")

blantyre    <- tibble(city = "Blantyre", lon = 35.0, lat = -15.8)
blantyre_sf <- st_as_sf(blantyre, coords = c("lon", "lat"), crs = 4326)

proj_lines <- tibble(
  x    = c(35, 35),
  y    = c(-15.78, -15.78),
  xend = c(44.9, 44.9),
  yend = c(-21.9, 4.9)
)

p_africa <- ggplot() +
  geom_sf(data = africa, fill = "gray90", color = "white") +
  geom_sf(data = malawi, fill = "darkgreen", color = "black") +
  geom_text(data = blantyre, aes(x = lon, y = lat, label = city),
            color = "brown", hjust = 1.2, size = 6) +
  geom_segment(data = proj_lines,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", linewidth = 1,
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_sf(data = blantyre_sf, color = "brown", size = 2.5) +
  coord_sf(xlim = c(25, 45), ylim = c(-22, 5), expand = FALSE) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())

# --- Right panel: Blantyre city boundary + household locations --------------

# Read Malawi admin level 2 shapefile (Blantyre City boundary)

malawi_shp_adm2 <- read_sf(
  here::here("data-raw/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp")
)

malawi_shp_adm2_4326 <- st_transform(malawi_shp_adm2, crs = 4326)

# Bounding box for the right panel (Blantyre city zoom)
df_bound_box_min <- tibble(lat = -14.00, long = 33.00)
df_bound_box_max <- tibble(lat = -18.00, long = 36.00)
df_bound_box     <- bind_rows(df_bound_box_min, df_bound_box_max)
map_data_point   <- st_as_sf(df_bound_box, coords = c("long", "lat"), crs = 4326)

# --- Right panel: Blantyre satellite map + household locations ---

p_blantyre <- ggplot() +
  base_map(st_bbox(map_data_point),
           basemap       = "google-satellite",
           increase_zoom = 3) +
  geom_sf(data = malawi_shp_adm2_4326 |>
            filter(ADM2_EN == "Blantyre City"),
          alpha     = 0,
          color     = "white",
          linewidth = 1) +
  geom_point(data = data_structure_pem24_correct,
             aes(x = long, y = lat),
             shape  = 21,
             color  = "black",
             fill   = "#FFA07A",
             size   = 4,
             stroke = 1,
             alpha  = 0.8) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# --- Combine panels and save [Figure 1] ---

p_africa   <- p_africa   + theme(plot.margin = unit(c(0, -0.1, 0,  0),  "cm"))
p_blantyre <- p_blantyre + theme(plot.margin = unit(c(0,  0,  0, -0.1), "cm"))

p_study_area <- (p_africa | p_blantyre) +
  plot_layout(widths = c(0.915, 1))

ggsave(here::here("figures/fig-1-study-area-map.jpeg"),
       plot   = p_study_area,
       width  = 12.7,
       height = 8,
       dpi    = 300,
       units  = "cm")

## 10.2 Boxplots: eBC exposure by period [Figure 2] -----------------------

# (A) OWB period
p3 <- df_bc_owb_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  mutate(type = recode(type, "burner" = "Burner", "non_burner" = "Non-Burner")) |>
  ggplot(aes(x = type, y = mean_irbcc, fill = type)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 22, size = 2,
               color = "black", fill = "white") +
  labs(y = "eBC concentration (µg/m³)", title = "(A)") +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(ylim = c(0, 30)) +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank())

# (B) Non-burning period (PEM excluding OWB)
p2 <- df_unique_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id")) |>
  mutate(type = recode(type, "burner" = "Burner", "non_burner" = "Non-Burner")) |>
  ggplot(aes(x = type, y = mean_irbcc, fill = type)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 22, size = 2,
               color = "black", fill = "white") +
  labs(y = "eBC concentration (µg/m³)", title = "(B)") +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(ylim = c(0, 30)) +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank())

# (C) Full PEM period
p1 <- df_bc_pem_summary_pre |>
  left_join(ds_MA200, by = c("serial_number", "hh_id", "type")) |>
  mutate(type = recode(type, "burner" = "Burner", "non_burner" = "Non-Burner")) |>
  filter(!hh_id == 21) |>
  ggplot(aes(x = type, y = mean_irbcc, fill = type)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 22, size = 2,
               color = "black", fill = "white") +
  labs(y = "eBC concentration (µg/m³)", title = "(C)") +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(ylim = c(0, 30)) +
  theme +
  theme(legend.position = "none", axis.title.x = element_blank())

p_exposures <- p3 + p2 + p1 + plot_layout(guides = "collect")

ggsave(here::here("figures/fig-2-ebc-exposure-boxplots.jpeg"),
       plot = p_exposures, width = 18, height = 7, dpi = 300, units = "cm")

## 10.3 Waste mass vs eBC exposure [Figure 4] -----------------------------

p_waste_bc_total <- ggplot(df_waste_bc, aes(x = total_waste, y = e_bc_total)) +
  geom_point(aes(size = as.factor(stirring_number), color = as.factor(stirring_number))) +
  scale_x_log10() +
  geom_smooth(method = "lm", color = "brown") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  labs(x = "Mass of waste burnt (kg)", y = "Total eBC exposure (µg.h/m³)",
       size = "Number of Stirrings", color = "Number of Stirrings", title = "(A)") +
  coord_cartesian(ylim = c(0, 160)) +
  theme

p_waste_bc_avg <- ggplot(df_waste_bc, aes(x = total_waste, y = mean_irbcc_burner)) +
  geom_point(aes(size = as.factor(stirring_number), color = as.factor(stirring_number))) +
  scale_x_log10() +
  geom_smooth(method = "lm", color = "brown") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  labs(x = "Mass of waste burnt (kg)", y = "Mean eBC exposure (µg/m³)",
       size = "Number of Stirrings", color = "Number of Stirrings", title = "(B)") +
  coord_cartesian(ylim = c(0, 60)) +
  theme

p_waste_bc <- p_waste_bc_total + p_waste_bc_avg + plot_layout(guides = "collect")

ggsave(here::here("figures/fig-4-waste-mass-vs-ebc.jpeg"),
       plot = p_waste_bc, width = 20, height = 9, dpi = 300, units = "cm")

## 10.4 Stirring number vs eBC exposure [Figure 5] ------------------------

p_stirr_bc_total <- ggplot(df_waste_bc, aes(x = stirring_number, y = e_bc_total)) +
  geom_smooth(method = "lm", color = "brown") +
  geom_point(color = "royalblue", size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  labs(x = "Number of Stirrings", y = "Total eBC exposure (µg.h/m³)", title = "(A)") +
  coord_cartesian(ylim = c(0, 160)) +
  theme

p_stirr_bc_avg <- ggplot(df_waste_bc, aes(x = stirring_number, y = mean_irbcc_burner)) +
  geom_smooth(method = "lm", color = "brown") +
  geom_point(color = "royalblue", size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  labs(x = "Number of Stirrings", y = "Mean eBC exposure (µg/m³)", title = "(B)") +
  coord_cartesian(ylim = c(0, 60)) +
  theme

p_stirr_bc <- p_stirr_bc_total + p_stirr_bc_avg

ggsave(here::here("figures/fig-5-stirring-vs-ebc.jpeg"),
       plot = p_stirr_bc, width = 18, height = 9, dpi = 300, units = "cm")

## 10.5 Waste composition boxplots [Appendix Figure A6] -------------------

a_1_p_waste_characterisation <- data_long |>
  mutate(household_garden_waste = fct_recode(household_garden_waste,
                                             "Household & Garden Waste" = "household_and_garden_waste",
                                             "Garden Waste"             = "garden_waste",
                                             "Household Waste"          = "household_waste"
  )) |>
  ggplot(aes(x = Waste_Type, y = Mass, fill = Waste_Type)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Waste Type", y = "Mass of waste burnt (kg)") +
  theme +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

a_2_p_waste_characterisation <- data_long |>
  filter(Waste_Type != "Organics") |>
  ggplot(aes(Waste_Type, Mass, fill = Waste_Type)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.7) +
  geom_jitter(aes(color = Waste_Type), width = 0.15, size = 2.5, alpha = 0.75) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  labs(x = "Waste Type", y = "Mass of waste burnt (kg)") +
  theme +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_waste_characterisation <- a_1_p_waste_characterisation + a_2_p_waste_characterisation

ggsave(here::here("figures/fig-app-6-waste-composition.jpeg"),
       plot = p_waste_characterisation, width = 20, height = 9, dpi = 300, units = "cm")

## 10.6 Burner/non-burner exposure ratio bar charts [Figure 3] ------------

p_bc_ratio_owb <- ggplot(df_bc_owb_ratio, aes(x = reorder(hh_id, ratio), y = ratio)) +
  geom_col(aes(fill = Settlement_type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  labs(x = "Household ID",
       y = str_wrap("eBC exposure ratio: Burners vs Non-Burners", width = 25)) +
  scale_y_continuous(breaks = seq(0, 27, by = 3)) +
  scale_fill_discrete(name = "Settlement type", labels = c("Formal", "Informal")) +
  theme

p_bc_ratio_unique <- ggplot(df_bc_unique_ratio, aes(x = reorder(hh_id, ratio), y = ratio)) +
  geom_col(aes(fill = Settlement_type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  labs(x = "Household ID",
       y = str_wrap("eBC exposure ratio: Burners vs Non-Burners", width = 25)) +
  scale_y_continuous(breaks = seq(0, 27, by = 3)) +
  scale_fill_discrete(name = "Settlement type", labels = c("Formal", "Informal")) +
  theme

p_bc_ratio <- p_bc_ratio_owb / p_bc_ratio_unique +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here::here("figures/fig-3-ebc-exposure-ratio.jpeg"),
       plot = p_bc_ratio, width = 7, height = 6, dpi = 300, units = "in")

## 10.7 Time series: hh_id 1–12 [Appendix Figure A2] ---------------------

plot_data_time_series_pem_1_12 <- df_bc_pem |>
  left_join(data_structure_owb, by = c("hh_id", "date")) |>
  filter(hh_id %in% c(1:12)) |>
  mutate(date_time_10min = floor_date(date_time, "0.5 minutes")) |>
  group_by(hh_id, type, serial_number, date_time_10min) |>
  summarise(
    avg_ir_bcc_10min   = mean(ir_bcc, na.rm = TRUE),
    burning_start_time = first(burning_start_time),
    burning_end_time   = first(burning_end_time),
    .groups = "drop"
  )

p_timeseries_1_12 <- ggplot(plot_data_time_series_pem_1_12,
                            aes(x = date_time_10min, y = avg_ir_bcc_10min, color = type)) +
  geom_rect(aes(xmin = burning_start_time, xmax = burning_end_time,
                ymin = -Inf, ymax = Inf),
            fill = "lightpink", alpha = 0.3, color = NA) +
  geom_line(size = 0.5, alpha = 0.6) +
  scale_x_datetime(date_labels = "%H:%M\n%d-%m", date_breaks = "6 hour") +
  facet_wrap(~ hh_id, ncol = 3, scales = "free") +
  labs(x = "Date Time", y = "eBC concentrations (µg/m³)", color = "Type") +
  scale_color_manual(values  = c("burner" = "brown", "non_burner" = "darkgreen"),
                     labels  = c("burner" = "Burner", "non_burner" = "Non-Burner")) +
  theme

ggsave(here::here("figures/fig-app-2-timeseries-hh-1-12.jpeg"),
       plot = p_timeseries_1_12, width = 8.3, height = 11.7, dpi = 300, units = "in")

## 10.8 Time series: hh_id 13–26 [Appendix Figure A3] --------------------

plot_data_time_series_pem_13_26 <- df_bc_pem |>
  left_join(data_structure_owb, by = c("hh_id", "date")) |>
  filter(hh_id %in% c(13:26)) |>
  mutate(date_time_10min = floor_date(date_time, "0.5 minutes")) |>
  group_by(hh_id, type, serial_number, date_time_10min) |>
  summarise(
    avg_ir_bcc_10min   = mean(ir_bcc, na.rm = TRUE),
    burning_start_time = first(burning_start_time),
    burning_end_time   = first(burning_end_time),
    .groups = "drop"
  )

p_timeseries_13_26 <- ggplot(plot_data_time_series_pem_13_26,
                             aes(x = date_time_10min, y = avg_ir_bcc_10min, color = type)) +
  geom_rect(aes(xmin = burning_start_time, xmax = burning_end_time,
                ymin = -Inf, ymax = Inf),
            fill = "lightpink", alpha = 0.3, color = NA) +
  geom_line(size = 0.5, alpha = 0.6) +
  scale_x_datetime(date_labels = "%H:%M\n%d-%m", date_breaks = "6 hour") +
  facet_wrap(~ hh_id, ncol = 3, scales = "free") +
  labs(x = "Date Time", y = "eBC concentrations (µg/m³)", color = "Type") +
  scale_color_manual(values  = c("burner" = "brown", "non_burner" = "darkgreen"),
                     labels  = c("burner" = "Burner", "non_burner" = "Non-Burner")) +
  theme

ggsave(here::here("figures/fig-app-3-timeseries-hh-13-26.jpeg"),
       plot = p_timeseries_13_26, width = 8.3, height = 11.7, dpi = 300, units = "in")

## 10.9 Diurnal pattern — all participants [Appendix Figure A4] -----------
## Includes hh_id 21 — shows how the outlier skews the burner diurnal pattern

df_pm_hourly <- df_unique |>
  mutate(hour = hour(time)) |>
  group_by(hour, date, hh_id, type) |>
  summarise(across(c(ir_bcc), mean), .groups = "drop")

confidence_intervals_pm <- df_pm_hourly |>
  group_by(hour, type) |>
  summarize(
    lower_ci_ir = t.test(ir_bcc)$conf.int[1],
    upper_ci_ir = t.test(ir_bcc)$conf.int[2],
    mean_ir     = mean(ir_bcc),
    count       = n(),
    .groups = "drop"
  )

confidence_intervals_pm$type <- factor(
  confidence_intervals_pm$type,
  levels = c("burner", "non_burner"),
  labels = c("Burners", "Non-burners")
)

p_diurnal <- confidence_intervals_pm |>
  ggplot(aes(x = hour, group = type)) +
  geom_line(aes(y = mean_ir, color = type), linewidth = 1) +
  geom_ribbon(aes(ymin = lower_ci_ir, ymax = upper_ci_ir, fill = type),
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Burners" = "red",  "Non-burners" = "blue")) +
  scale_fill_manual( values = c("Burners" = "red",  "Non-burners" = "blue")) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  labs(x = "Hour", y = "eBC concentrations (µg/m³)", color = "Type", fill = "Type") +
  theme +
  theme(legend.position = "bottom")

ggsave(here::here("figures/fig-app-4-diurnal-pattern-all-participants.jpeg"),
       plot = p_diurnal, width = 5, height = 3, dpi = 300, units = "in")

confidence_intervals_pm |>
  ungroup() |>
  group_by(type) |>
  summarise(max = max(mean_ir))

## 10.10 Diurnal pattern — excluding hh_id 21 [Appendix Figure A5] -------
## hh_id 21 (burner) showed abnormally high concentrations between 22:00
## and 4:00 during a non-burning period, skewing the burner diurnal pattern.
## This figure shows the diurnal pattern with that participant excluded.

df_pm_hourly_exc_21 <- df_unique |>
  filter(!hh_id == 21) |>                        # exclude outlier participant
  mutate(hour = hour(time)) |>
  group_by(hour, date, hh_id, type) |>
  summarise(across(c(ir_bcc), mean), .groups = "drop")

confidence_intervals_pm_exc_21 <- df_pm_hourly_exc_21 |>
  group_by(hour, type) |>
  summarize(
    lower_ci_ir = t.test(ir_bcc)$conf.int[1],
    upper_ci_ir = t.test(ir_bcc)$conf.int[2],
    mean_ir     = mean(ir_bcc),
    count       = n(),
    .groups = "drop"
  )

confidence_intervals_pm_exc_21$type <- factor(
  confidence_intervals_pm_exc_21$type,
  levels = c("burner", "non_burner"),
  labels = c("Burners", "Non-burners")
)

p_diurnal_exc_21 <- confidence_intervals_pm_exc_21 |>
  ggplot(aes(x = hour, group = type)) +
  geom_line(aes(y = mean_ir, color = type), linewidth = 1) +
  geom_ribbon(aes(ymin = lower_ci_ir, ymax = upper_ci_ir, fill = type),
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Burners" = "red",  "Non-burners" = "blue")) +
  scale_fill_manual( values = c("Burners" = "red",  "Non-burners" = "blue")) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  labs(x = "Hour", y = "eBC concentrations (µg/m³)", color = "Type", fill = "Type") +
  theme +
  theme(legend.position = "bottom")

ggsave(here::here("figures/fig-app-5-diurnal-pattern-exc-hh-21.jpeg"),
       plot = p_diurnal_exc_21, width = 5, height = 3, dpi = 300, units = "in")

confidence_intervals_pm_exc_21 |>
  ungroup() |>
  group_by(type) |>
  summarise(max = max(mean_ir))

## 10.11 Collocation plots [Appendix Figure A1] ---------------------------

p_col <- FB |>
  mutate(date = as.Date(date)) |>
  filter(session_id %in% c(0147, 0107), ir_bcc > 0) |>
  drop_na() |>
  ggplot() +
  geom_line(aes(x = date_time, y = ir_bcc, color = serial_number), alpha = 0.6) +
  labs(y = expression("eBC concentration ("*µ*"g/m"^3*")"),
       x = "Date and time",
       color = "MA200 serial number") +
  theme

p_col_r <- FB |>
  filter(session_id %in% c(0147, 0107), ir_bcc > 0) |>
  select(ir_bcc, serial_number, date_time) |>
  pivot_wider(names_from = serial_number, values_from = ir_bcc) |>
  ggplot(aes(x = `MA200-0416`, y = `MA200-0420`)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_smooth(method = "lm", color = "#0072B2") +
  labs(x = expression("eBC concentration ("*µ*"g/m"^3*") - 0416"),
       y = expression("eBC concentration ("*µ*"g/m"^3*") - 0420")) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "left", label.y = "top") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#D55E00") +
  scale_color_viridis_d() +
  theme

p_collocation <- p_col / p_col_r

ggsave(here::here("figures/fig-app-1-collocation.jpeg"),
       plot = p_collocation, width = 5, height = 8, dpi = 300, units = "in")

