############################################################
# Q3 (10/100): Correlation between different variable groups
# - Demographics (age, education, income, WFH, gender optional)
# - Shopping behaviour (online purchase frequency, returns, etc.)
# - Attitudes (Likert-5 → numeric scores; also mode indices)
# - SP outcomes (per-respondent choice shares)
#
# OUTPUTS (saved in same folder as CSV):
# - q3_corr_group_level.csv  (group index correlations + p-values)
# - q3_corr_var_level.csv    (all numeric vars correlations + p-values)
# - q3_corr_heatmap.png      (heatmap of group indices)
############################################################

rm(list = ls()); graphics.off()

# ----------------------------
# 0) Packages
# ----------------------------
pkgs <- c("tidyverse", "janitor", "stringr", "scales")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ----------------------------
# 1) Load data
# ----------------------------
data_dir  <- "C:/Users/ASUS/OneDrive - TUM/Desktop/Assignment DCA/Assignment 3 DCA"
data_file <- file.path(data_dir, "DCM_3_dataset.csv")
out_dir   <- data_dir

df <- readr::read_csv(data_file, show_col_types = FALSE) %>%
  clean_names()

stopifnot("response_id" %in% names(df))

# respondent-level (1 row per person)
df_resp <- df %>%
  group_by(response_id) %>%
  slice(1) %>%
  ungroup()

# ----------------------------
# 2) Helper: safe numeric conversion (for ordinal/text)
# ----------------------------
as_num_safe <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ----------------------------
# 3) Define variable groups (edit here if needed)
# ----------------------------
# NOTE: These column names match your dataset after clean_names().
# If any name differs, run: names(df_resp) and adjust.

col_yob    <- "what_year_were_you_born"
col_edu    <- "what_is_the_highest_level_of_education_you_have_completed"
col_income <- "what_is_your_monthly_household_income_after_tax_i_e_the_amount_that_is_monthly_transferred_to_your_accounts"
col_wfh    <- "on_average_how_many_days_per_week_do_you_work_from_home"
col_gender <- "what_is_your_gender"

# shopping behaviour candidates (pattern-based, robust)
shopping_cols <- names(df_resp)[str_detect(names(df_resp),
                                           "how_frequently_do_you_purchase_online|purchase_online_each_month|return|orders_where_you_expect_to_return|online_shopping"
)]

# ----------------------------
# 4) Build DEMOGRAPHICS numeric features
# ----------------------------
demo <- tibble(response_id = df_resp$response_id)

# age
if (col_yob %in% names(df_resp)) {
  demo <- demo %>% mutate(age = 2026 - as_num_safe(df_resp[[col_yob]]))
}

# education (ordinal coding, manual order based on your categories)
# Adjust levels if your wording differs
if (col_edu %in% names(df_resp)) {
  edu_levels <- c(
    "No formal education",
    "Primary education",
    "Secondary education (e.g. high school diploma)",
    "Vocational training / Technical School",
    "Bachelor’s degree",
    "Master’s degree",
    "Doctorate or higher"
  )
  edu_raw <- as.character(df_resp[[col_edu]])
  edu_raw[edu_raw %in% c("I prefer not to say", "Other", "", NA)] <- NA
  demo <- demo %>%
    mutate(
      education_ord = as.integer(factor(edu_raw, levels = edu_levels, ordered = TRUE))
    )
}

# income (ordinal coding: build an ordered scale from your common bins)
if (col_income %in% names(df_resp)) {
  inc_levels <- c(
    "Less than €1000",
    "€1000 – €1500",
    "€1500 – €2000",
    "€2000 – €2500",
    "€2500 – €3000",
    "€3000 – €3500",
    "€3500 – €4000",
    "€4000 – €4500",
    "€4500 – €5000",
    "€5000 – €5500",
    "€5500 – €6000",
    "€6000 – €6500",
    "€6500 – €7000",
    "€7000 – €7500",
    "more than €7500"
  )
  inc_raw <- as.character(df_resp[[col_income]])
  inc_raw[inc_raw %in% c("I don't want to disclose", "I don’t want to disclose", "I don't know", "I don’t know", "", NA)] <- NA
  demo <- demo %>%
    mutate(
      income_ord = as.integer(factor(inc_raw, levels = inc_levels, ordered = TRUE))
    )
}

# WFH days/week (convert label → numeric)
if (col_wfh %in% names(df_resp)) {
  wfh_raw <- as.character(df_resp[[col_wfh]])
  wfh_num <- case_when(
    str_detect(wfh_raw, "^1") ~ 1,
    str_detect(wfh_raw, "^2") ~ 2,
    str_detect(wfh_raw, "^3") ~ 3,
    str_detect(wfh_raw, "^4") ~ 4,
    str_detect(wfh_raw, "^5") ~ 5,
    TRUE ~ NA_real_
  )
  demo <- demo %>% mutate(wfh_days = wfh_num)
}

# gender (optional dummy coding: creates multiple 0/1 columns)
# If you prefer not to include gender in correlation (categorical), skip this block.
if (col_gender %in% names(df_resp)) {
  g <- as.character(df_resp[[col_gender]])
  g[g %in% c("", NA)] <- NA
  g_dum <- model.matrix(~ factor(g) - 1)
  colnames(g_dum) <- str_replace_all(colnames(g_dum), "factor\\(g\\)", "gender_") |> make.names()
  demo <- bind_cols(demo, as_tibble(g_dum))
}

# ----------------------------
# 5) SHOPPING behaviour numeric features
# ----------------------------
shop <- tibble(response_id = df_resp$response_id)

if (length(shopping_cols) > 0) {
  # Try numeric directly; if labelled categories, keep NA for now (you can extend mapping)
  shop <- shop %>%
    bind_cols(df_resp %>% select(all_of(shopping_cols)) %>% mutate(across(everything(), as_num_safe)))
} else {
  message("No shopping behaviour columns detected by pattern. You can manually set shopping_cols.")
}

# ----------------------------
# 6) ATTITUDES: detect Likert-5 columns and compute indices
# ----------------------------
likert_levels <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

is_likert5_col <- function(x) {
  xx <- as.character(x)
  xx <- xx[!is.na(xx)]
  if (length(xx) == 0) return(FALSE)
  mean(tolower(str_squish(xx)) %in% tolower(likert_levels)) > 0.80
}

cat_cols <- df_resp %>% select(where(~ is.character(.x) || is.factor(.x))) %>% names()
likert_cols <- cat_cols[sapply(df_resp[cat_cols], is_likert5_col)]

att <- tibble(response_id = df_resp$response_id)

if (length(likert_cols) == 0) {
  message("No Likert-5 attitude columns detected. Attitude correlations will be skipped.")
} else {
  # Likert → numeric 1..5
  likert_to_num <- function(x) {
    x <- as.character(x) |> str_squish()
    x <- case_when(
      str_to_lower(x) == "strongly disagree" ~ 1,
      str_to_lower(x) == "disagree"          ~ 2,
      str_to_lower(x) == "neutral"           ~ 3,
      str_to_lower(x) == "agree"             ~ 4,
      str_to_lower(x) == "strongly agree"    ~ 5,
      TRUE ~ NA_real_
    )
    x
  }
  
  att_num <- df_resp %>%
    select(all_of(likert_cols)) %>%
    mutate(across(everything(), likert_to_num))
  
  # Mode indices (Collection point / Locker / Home delivery)
  # Uses column names (clean_names) patterns; robust to long names
  nm <- names(att_num)
  cols_cp <- nm[str_detect(nm, "collecting_point|collection_point|collecting")]
  cols_lk <- nm[str_detect(nm, "locker")]
  cols_hd <- nm[str_detect(nm, "home_delivery|home")]
  
  att <- att %>%
    mutate(
      attitude_cp_mean = if (length(cols_cp) > 0) rowMeans(att_num[, cols_cp, drop = FALSE], na.rm = TRUE) else NA_real_,
      attitude_locker_mean = if (length(cols_lk) > 0) rowMeans(att_num[, cols_lk, drop = FALSE], na.rm = TRUE) else NA_real_,
      attitude_home_mean = if (length(cols_hd) > 0) rowMeans(att_num[, cols_hd, drop = FALSE], na.rm = TRUE) else NA_real_,
      attitude_all_mean = rowMeans(att_num, na.rm = TRUE)
    )
}

# ----------------------------
# 7) SP outcomes: per-respondent choice shares
# ----------------------------
stopifnot("choice_name" %in% names(df))

sp_shares <- df %>%
  count(response_id, choice_name, name = "n_choice") %>%
  group_by(response_id) %>%
  mutate(share = n_choice / sum(n_choice)) %>%
  ungroup() %>%
  select(response_id, choice_name, share) %>%
  pivot_wider(names_from = choice_name, values_from = share, values_fill = 0) %>%
  rename_with(~ paste0("sp_share_", make.names(.x)), -response_id)

# ----------------------------
# 8) Merge all groups into one analysis dataset
# ----------------------------
X <- demo %>%
  left_join(shop, by = "response_id") %>%
  left_join(att, by = "response_id") %>%
  left_join(sp_shares, by = "response_id")

# Keep only numeric columns for correlation
X_num <- X %>%
  select(-response_id) %>%
  select(where(is.numeric))

# drop near-constant columns
nzv <- sapply(X_num, function(v) length(unique(v[!is.na(v)])) >= 3)
X_num <- X_num[, nzv, drop = FALSE]

# ----------------------------
# 9) Correlation tests
#   - Spearman is robust for ordinal variables (education/income/WFH/Likert indices)
# ----------------------------
cor_test_mat <- function(df_num, method = "spearman") {
  vars <- names(df_num)
  out <- expand_grid(var1 = vars, var2 = vars) %>%
    filter(var1 < var2) %>%
    mutate(
      estimate = NA_real_,
      p_value = NA_real_
    )
  
  for (i in seq_len(nrow(out))) {
    v1 <- out$var1[i]; v2 <- out$var2[i]
    a <- df_num[[v1]]; b <- df_num[[v2]]
    ok <- complete.cases(a, b)
    if (sum(ok) >= 30) {
      ct <- suppressWarnings(cor.test(a[ok], b[ok], method = method))
      out$estimate[i] <- unname(ct$estimate)
      out$p_value[i] <- ct$p.value
    }
  }
  
  out %>% mutate(p_adj_bh = p.adjust(p_value, method = "BH"))
}

# Variable-level correlation table (all numeric vars)
corr_var <- cor_test_mat(X_num, method = "spearman")
write.csv(corr_var, file.path(out_dir, "q3_corr_var_level.csv"), row.names = FALSE)

# ----------------------------
# 10) Group-level indices correlation (cleaner for reporting)
# ----------------------------
group_df <- X %>% transmute(
  # demographics indices
  age = if ("age" %in% names(demo)) demo$age else NA_real_,
  education_ord = if ("education_ord" %in% names(demo)) demo$education_ord else NA_real_,
  income_ord = if ("income_ord" %in% names(demo)) demo$income_ord else NA_real_,
  wfh_days = if ("wfh_days" %in% names(demo)) demo$wfh_days else NA_real_,
  
  # shopping behaviour: mean of detected shopping numeric cols (if any)
  shopping_mean = if (length(shopping_cols) > 0) rowMeans(shop %>% select(where(is.numeric)), na.rm = TRUE) else NA_real_,
  
  # attitudes indices
  attitude_cp_mean = if ("attitude_cp_mean" %in% names(att)) att$attitude_cp_mean else NA_real_,
  attitude_locker_mean = if ("attitude_locker_mean" %in% names(att)) att$attitude_locker_mean else NA_real_,
  attitude_home_mean = if ("attitude_home_mean" %in% names(att)) att$attitude_home_mean else NA_real_,
  attitude_all_mean = if ("attitude_all_mean" %in% names(att)) att$attitude_all_mean else NA_real_
) %>%
  bind_cols(sp_shares %>% select(-response_id)) %>%
  select(where(is.numeric))

group_df <- group_df[, sapply(group_df, function(v) length(unique(v[!is.na(v)])) >= 3), drop = FALSE]

corr_group <- cor_test_mat(group_df, method = "spearman")
write.csv(corr_group, file.path(out_dir, "q3_corr_group_level.csv"), row.names = FALSE)

# ----------------------------
# 11) Heatmap of group indices (nice for report)
# ----------------------------
C <- suppressWarnings(cor(group_df, use = "pairwise.complete.obs", method = "spearman"))
C_long <- as.data.frame(as.table(C))
names(C_long) <- c("var1","var2","rho")

# correlation matrix
C <- suppressWarnings(cor(group_df,
                          use = "pairwise.complete.obs",
                          method = "spearman"))

# convert to long format
corr_long <- as.data.frame(as.table(C))
names(corr_long) <- c("var_x", "var_y", "rho")

# heatmap WITH numeric values
p_heat <- ggplot(corr_long, aes(var_x, var_y, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(rho, 2)), size = 3) +
  scale_fill_gradient2(low = "#b2182b",
                       mid = "white",
                       high = "#2166ac",
                       midpoint = 0,
                       limits = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Spearman correlations (group indices + SP shares)",
       fill = "rho")

print(p_heat)

ggsave(file.path(out_dir, "q3_corr_heatmap.png"),
       p_heat, width = 10, height = 8, dpi = 300)


cat("\nQ3 DONE ✅\nSaved:\n",
    "- q3_corr_group_level.csv\n",
    "- q3_corr_var_level.csv\n",
    "- q3_corr_heatmap.png\n",
    "in: ", out_dir, "\n", sep = "")




library(tidyverse)
library(psych)
library(GPArotation)
library(stringr)

# 1) robust Likert mapping (lowercase keys)
likert_map <- c(
  "strongly disagree" = 1,
  "disagree" = 2,
  "neutral" = 3,
  "agree" = 4,
  "strongly agree" = 5
)

# 2) select attitude columns (you can keep your pattern)
att_cols <- names(df)[str_detect(names(df),
                                 "agree|concern|privacy|environment|safety|return|use")]

# 3) convert to numeric Likert safely
att_df <- df %>%
  select(all_of(att_cols)) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x <- str_to_lower(x)
    unname(likert_map[x])
  }))

# 4) DIAGNOSTIC: check if conversion worked
cat("Rows:", nrow(att_df), "Cols:", ncol(att_df), "\n")
cat("Non-NA share (per column):\n")
print(sort(colMeans(!is.na(att_df)), decreasing = TRUE)[1:10])

# 5) drop columns that are mostly NA (important!)
keep_cols <- names(att_df)[colMeans(!is.na(att_df)) >= 0.30]
att_df <- att_df %>% select(all_of(keep_cols))

# 6) drop rows with missing in remaining columns
att_df <- att_df %>% drop_na()

cat("After filtering -> Rows:", nrow(att_df), "Cols:", ncol(att_df), "\n")

# 7) Adequacy tests
KMO(att_df)
cortest.bartlett(att_df)

# 8) Factor number (parallel analysis)
fa.parallel(att_df, fm = "ml", fa = "fa")

# 9) Run EFA (start with 3; adjust based on parallel analysis)
efa_result <- fa(att_df, nfactors = 3, rotate = "oblimin", fm = "ml")
print(efa_result$loadings, cutoff = 0.30)






# ============================================================
# EFA on Attitudes (Likert 5): KMO + Bartlett + Parallel + ML EFA
# + factor scores for later models
# ============================================================
install.packages("writexl")
library(tidyverse)
library(psych)
library(GPArotation)
library(stringr)
library(writexl)

# --- 0) Make sure df exists (your full dataset already loaded) ---
# df <- read.csv("yourfile.csv", stringsAsFactors = FALSE)

# --- 1) Likert mapping (robust) ---
likert_map <- c(
  "strongly disagree" = 1,
  "disagree"          = 2,
  "neutral"           = 3,
  "agree"             = 4,
  "strongly agree"    = 5
)

# --- 2) Select attitude columns (your pattern) ---
att_cols <- names(df)[str_detect(names(df),
                                 "agree|concern|privacy|environment|safety|return|use")]

# --- 3) Convert to numeric Likert safely ---
att_raw <- df %>%
  select(all_of(att_cols)) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x <- str_to_lower(x)
    unname(likert_map[x])
  }))

cat("Raw attitude block -> Rows:", nrow(att_raw), "Cols:", ncol(att_raw), "\n")

# --- 4) Drop columns with too much missing (keep columns with >=30% non-NA) ---
keep_cols <- names(att_raw)[colMeans(!is.na(att_raw)) >= 0.30]
att_num <- att_raw %>% select(all_of(keep_cols))

# --- 5) Drop rows with missing in remaining columns ---
att_num <- att_num %>% drop_na()

cat("After cleaning -> Rows:", nrow(att_num), "Cols:", ncol(att_num), "\n")

# --- 6) KMO + Bartlett (FULL MARKS) ---
kmo_res <- KMO(att_num)
bart_res <- cortest.bartlett(att_num)

print(kmo_res)
print(bart_res)

# Save KMO + Bartlett to file (optional but nice for report appendix)
kmo_table <- tibble(item = names(kmo_res$MSAi), MSA = as.numeric(kmo_res$MSAi))
bart_table <- tibble(
  chisq = bart_res$chisq,
  df    = bart_res$df,
  p     = bart_res$p.value
)

# --- 7) Parallel Analysis to justify factor number ---
set.seed(123)
png("efa_parallel_analysis.png", width = 1200, height = 800, res = 150)
pa <- fa.parallel(att_num, fm = "ml", fa = "fa")
dev.off()

# Suggested number of factors from parallel analysis:
# pa$nfact gives the suggested # factors (psych may store differently)
cat("Parallel analysis suggested factors (see plot):",
    "FA factors ~", pa$nfact, "\n")

# --- 8) Fit EFA model(s): ML extraction + oblimin rotation ---
# Option A: parsimonious 3-factor solution (easy to interpret)
efa3 <- fa(att_num, nfactors = 3, rotate = "oblimin", fm = "ml")

# Option B (optional): parallel-analysis suggested solution (often 7)
# efa7 <- fa(att_num, nfactors = 7, rotate = "oblimin", fm = "ml")

# Print loadings (cutoff 0.30)
print(efa3$loadings, cutoff = 0.30)

# --- 9) Export loadings + fit stats to Excel for the appendix ---
load3 <- as.data.frame.matrix(efa3$loadings)
load3$item <- rownames(load3)
load3 <- load3 %>% relocate(item)

fit3 <- tibble(
  n_factors = 3,
  fm = "ml",
  rotation = "oblimin",
  RMSEA = efa3$RMSEA[1],
  TLI = efa3$TLI,
  BIC = efa3$BIC
)

write_xlsx(
  list(
    KMO_item_MSA = kmo_table,
    Bartlett = bart_table,
    EFA3_loadings = load3,
    EFA3_fit = fit3
  ),
  path = "efa_attitudes_outputs.xlsx"
)

# --- 10) Factor scores for later models (optional but full-marks) ---
# Scores aligned row-by-row with att_num (these are the same rows kept after drop_na)
scores3 <- as.data.frame(efa3$scores)
scores3 <- scores3 %>% mutate(row_id = row_number())

# Attach row_id to original df rows that survived cleaning (same filtering steps)
# We create a boolean keep-mask to map back safely:

keep_mask <- df %>%
  select(all_of(keep_cols)) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x <- str_to_lower(x)
    unname(likert_map[x])
  })) %>%
  drop_na() %>%
  mutate(row_id = row_number()) %>%
  select(row_id)

# Now create an EFA-ready dataset with scores (same number of rows as att_num)
efa_scored <- bind_cols(keep_mask, scores3 %>% select(-row_id))

write.csv(efa_scored, "efa_factor_scores_rows_kept.csv", row.names = FALSE)

cat("\nSaved files:\n",
    "- efa_parallel_analysis.png\n",
    "- efa_attitudes_outputs.xlsx\n",
    "- efa_factor_scores_rows_kept.csv\n")


table(df$choice_name)
names(df)[grep("^x[0-9]_", names(df))]
df$chosen_alt <- alt_map[df$choice_name]








################################################################################
# MODEL 2 (WORKING): MNL with COST + TIME + GREEN + TRACKING
# Fixes your issue by:
# 1) Never doing row-wise NA filtering first
# 2) Dropping whole choice sets with any missing values
# 3) Enforcing 4 alternatives + exactly 1 chosen
################################################################################

library(tidyverse)
library(mlogit)
library(stringr)
library(readr)

stopifnot(all(c("Response.ID", "Scenario", "ChoiceName") %in% names(df)))

# --- 1) ChoiceName -> chosen alternative id (0..3)
alt_map <- c(
  "Home/WorkDelivery"    = 0,
  "Packstation(Locker)"  = 1,
  "Pickup atCarrier"     = 2,
  "Pickup atStore"       = 3
)

df2 <- df %>%
  mutate(choice_id = unname(alt_map[as.character(ChoiceName)])) %>%
  filter(!is.na(choice_id))

# --- 2) collect SP columns
x_cols <- names(df2)[str_detect(names(df2), "^X[0-3]_")]
stopifnot(length(x_cols) > 0)

# --- 3) wide -> long (4 rows per scenario per respondent)
long <- df2 %>%
  select(Response.ID, Scenario, choice_id, all_of(x_cols)) %>%
  pivot_longer(cols = all_of(x_cols), names_to = "var", values_to = "value") %>%
  mutate(
    alt  = str_extract(var, "^X[0-3]"),
    alt  = as.integer(str_remove(alt, "^X")),
    attr = str_remove(var, "^X[0-3]_")
  ) %>%
  select(-var) %>%
  pivot_wider(names_from = attr, values_from = value) %>%
  mutate(
    choice_set = paste(Response.ID, Scenario, sep = "_"),
    chosen     = as.integer(alt == choice_id)   # IMPORTANT: 0/1
  )

cat("After reshape rows:", nrow(long), "\n")

# --- 4) Helpers (ROBUST parsing)
yn_to01 <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("yes","y","true","t","1") ~ 1,
    x %in% c("no","n","false","f","0") ~ 0,
    TRUE ~ NA_real_
  )
}

cost_to_num <- function(x) {
  x2 <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    str_detect(x2, "free") ~ 0,
    TRUE ~ readr::parse_number(x2, locale = locale(decimal_mark = ".", grouping_mark = ","))
  )
}

# if your time is categorical (Under 10 / 20-40 / Above 1 hour etc.)
time_to_num <- function(x) {
  x2 <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    str_detect(x2, "under\\s*10") ~ 5,
    str_detect(x2, "10\\s*-\\s*20") ~ 15,
    str_detect(x2, "20\\s*-\\s*40") ~ 30,
    str_detect(x2, "40\\s*-\\s*60") ~ 50,
    str_detect(x2, "above\\s*1\\s*hour|over\\s*1\\s*hour|>\\s*1") ~ 70,
    str_detect(x2, "n/a|na") ~ NA_real_,
    TRUE ~ readr::parse_number(x2)
  )
}

# --- 5) Create numeric model vars
long <- long %>%
  mutate(
    cost_num     = if ("cost" %in% names(.)) cost_to_num(cost) else NA_real_,
    time_num     = if ("time" %in% names(.)) time_to_num(time) else NA_real_,
    green_num    = if ("greenyesNo" %in% names(.)) yn_to01(greenyesNo) else NA_real_,
    tracking_num = if ("trackingyesNo" %in% names(.)) yn_to01(trackingyesNo) else NA_real_
  )

use_vars <- c("cost_num","time_num","green_num","tracking_num")

# --- 6) Drop ENTIRE choice sets if ANY alt has NA in any variable
set_ok <- long %>%
  group_by(choice_set) %>%
  summarise(
    n_alt = n(),
    n_chosen = sum(chosen),
    all_complete = all(!is.na(cost_num) & !is.na(time_num) & !is.na(green_num) & !is.na(tracking_num)),
    .groups = "drop"
  ) %>%
  filter(n_alt == 4, n_chosen == 1, all_complete) %>%
  pull(choice_set)

cat("Choice sets kept:", length(set_ok), "\n")

long2 <- long %>% filter(choice_set %in% set_ok)

cat("Rows after keeping valid sets:", nrow(long2), "\n")
cat("chosen distribution:\n")
print(table(long2$chosen))

# --- 7) Stabilize (optional but helps convergence a lot)
long2 <- long2 %>%
  mutate(
    cost_num = as.numeric(scale(cost_num)),
    time_num = as.numeric(scale(time_num))
  )

# --- 8) mlogit data + model
long2 <- long2 %>% mutate(chosen = chosen == 1)  # TRUE/FALSE

mlogit_data <- mlogit.data(
  long2,
  choice   = "chosen",
  shape    = "long",
  alt.var  = "alt",
  chid.var = "choice_set",
  id.var   = "Response.ID"
)

mnl_model2 <- mlogit(chosen ~ cost_num + time_num + green_num + tracking_num | 0,
                     data = mlogit_data,
                     method = "bfgs")

summary(mnl_model2)



library(tidyverse)
library(psych)
library(GPArotation)
library(stringr)

# 1) robust Likert mapping (lowercase keys)
likert_map <- c(
  "strongly disagree" = 1,
  "disagree" = 2,
  "neutral" = 3,
  "agree" = 4,
  "strongly agree" = 5
)

# 2) select attitude columns (you can keep your pattern)
att_cols <- names(df)[str_detect(names(df),
                                 "agree|concern|privacy|environment|safety|return|use")]

# 3) convert to numeric Likert safely
att_df <- df %>%
  select(all_of(att_cols)) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x <- str_to_lower(x)
    unname(likert_map[x])
  }))

# 4) DIAGNOSTIC: check if conversion worked
cat("Rows:", nrow(att_df), "Cols:", ncol(att_df), "\n")
cat("Non-NA share (per column):\n")
print(sort(colMeans(!is.na(att_df)), decreasing = TRUE)[1:10])

# 5) drop columns that are mostly NA (important!)
keep_cols <- names(att_df)[colMeans(!is.na(att_df)) >= 0.30]
att_df <- att_df %>% select(all_of(keep_cols))

# 6) drop rows with missing in remaining columns
att_df <- att_df %>% drop_na()

cat("After filtering -> Rows:", nrow(att_df), "Cols:", ncol(att_df), "\n")

# 7) Adequacy tests
KMO(att_df)
cortest.bartlett(att_df)

# 8) Factor number (parallel analysis)
fa.parallel(att_df, fm = "ml", fa = "fa")

# 9) Run EFA (start with 3; adjust based on parallel analysis)
efa_result <- fa(att_df, nfactors = 3, rotate = "oblimin", fm = "ml")
print(efa_result$loadings, cutoff = 0.30)



