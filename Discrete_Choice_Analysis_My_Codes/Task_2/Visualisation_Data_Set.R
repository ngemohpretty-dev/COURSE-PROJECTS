
# Assignment 3 (DCA) — Descriptive Statistics 
# ✅ Demographics + Attitudes (Likert-5) + SP choices + SP attributes
# ✅ Saves ALL outputs in SAME folder as the CSV
# ✅ Shows plots in RStudio while also saving PNG files
# ✅ Demographics titles: "Working from home" and "Education"
# ✅ Attitude colors (as requested): Disagree = red, Agree = green, Neutral = white
# ✅ Robust SP attributes for x0_/x1_/x2_/x3_
############################################################

rm(list = ls())
graphics.off()

# ----------------------------
# 0) Packages
# ----------------------------
pkgs <- c("tidyverse", "janitor", "scales", "gt", "stringr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ----------------------------
# 1) Path + load data
# ----------------------------
data_dir  <- "C:/Users/ASUS/OneDrive - TUM/Desktop/Assignment DCA/Assignment 3 DCA"
data_file <- file.path(data_dir, "DCM_3_dataset.csv")

# Save outputs in SAME folder as CSV
out_dir <- data_dir

save_and_show_plot <- function(p, filename, w = 10, h = 6) {
  print(p)  # show in RStudio Plots pane
  ggsave(filename = file.path(out_dir, filename), plot = p, width = w, height = h, dpi = 300)
}
save_gt <- function(gt_obj, filename) {
  gtsave(gt_obj, filename = file.path(out_dir, filename))
}

df <- readr::read_csv(data_file, show_col_types = FALSE) %>%
  janitor::clean_names()

if (!("response_id" %in% names(df))) stop("response_id not found. Run names(df).")

# Respondent-level (1 row per person)
df_resp <- df %>%
  group_by(response_id) %>%
  slice(1) %>%
  ungroup()

############################################################
# 2) DEMOGRAPHICS — tables + plots
############################################################

col_country <- "in_which_country_do_you_live"
col_city    <- "in_which_city_do_you_live"
col_yob     <- "what_year_were_you_born"
col_gender  <- "what_is_your_gender"
col_edu     <- "what_is_the_highest_level_of_education_you_have_completed"
col_income  <- "what_is_your_monthly_household_income_after_tax_i_e_the_amount_that_is_monthly_transferred_to_your_accounts"
col_wfh     <- "on_average_how_many_days_per_week_do_you_work_from_home"

if (col_yob %in% names(df_resp)) {
  df_resp <- df_resp %>% mutate(age = 2026 - .data[[col_yob]])
}

tab_pct <- function(data, varname) {
  data %>%
    count(.data[[varname]], name = "n") %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(n))
}

plot_bar <- function(data, varname, title) {
  data %>%
    count(.data[[varname]]) %>%
    mutate(x = as.factor(.data[[varname]]),
           x = forcats::fct_reorder(x, n)) %>%
    ggplot(aes(x = x, y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal()
}

demo_summary <- tibble(
  n_respondents = nrow(df_resp),
  age_mean   = if ("age" %in% names(df_resp)) mean(df_resp$age, na.rm = TRUE) else NA_real_,
  age_median = if ("age" %in% names(df_resp)) median(df_resp$age, na.rm = TRUE) else NA_real_,
  age_sd     = if ("age" %in% names(df_resp)) sd(df_resp$age, na.rm = TRUE) else NA_real_,
  age_min    = if ("age" %in% names(df_resp)) min(df_resp$age, na.rm = TRUE) else NA_real_,
  age_max    = if ("age" %in% names(df_resp)) max(df_resp$age, na.rm = TRUE) else NA_real_
)

gt_demo <- demo_summary %>%
  gt() %>%
  tab_header(title = "Demographic summary") %>%
  fmt_number(columns = everything(), decimals = 2)
save_gt(gt_demo, "table_demographics_summary.html")

demo_vars <- list(
  country = list(var = col_country, title = "Country"),
  city    = list(var = col_city,    title = "City"),
  gender  = list(var = col_gender,  title = "Gender"),
  edu     = list(var = col_edu,     title = "Education"),
  income  = list(var = col_income,  title = "Monthly household income (after tax)"),
  wfh     = list(var = col_wfh,     title = "Working from home")
)

for (nm in names(demo_vars)) {
  v <- demo_vars[[nm]]$var
  ttl <- demo_vars[[nm]]$title
  
  if (v %in% names(df_resp)) {
    tt <- tab_pct(df_resp, v)
    gt_t <- tt %>%
      gt() %>%
      tab_header(title = paste0("Demographics: ", ttl)) %>%
      fmt_percent(columns = pct, decimals = 1)
    save_gt(gt_t, paste0("table_demo_", nm, ".html"))
    
    pp <- plot_bar(df_resp, v, paste0("Demographics: ", ttl))
    save_and_show_plot(pp, paste0("plot_demo_", nm, ".png"), w = 10, h = 6)
  }
}

if ("age" %in% names(df_resp)) {
  p_age <- ggplot(df_resp, aes(x = age)) +
    geom_histogram(bins = 30) +
    labs(title = "Age distribution", x = "Age", y = "Count") +
    theme_minimal()
  save_and_show_plot(p_age, "plot_demo_age_hist.png", w = 9, h = 6)
}

############################################################
# 3) ATTITUDES — Likert-5 stacked plots (3 plots + optional combined)
############################################################

likert_levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

# ✅ Colors you requested: Disagree=red, Agree=green, Neutral=white
likert_palette <- c(
  "Strongly Disagree" = "#8B0000",  # dark red
  "Disagree"          = "#F4A6A6",  # light red
  "Neutral"           = "#FFFFFF",  # white
  "Agree"             = "#A8D5A2",  # light green
  "Strongly Agree"    = "#006400"   # dark green
)

standardize_likert <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x_low <- tolower(x)
  
  x <- dplyr::case_when(
    x_low == "strongly disagree" ~ "Strongly Disagree",
    x_low == "disagree"          ~ "Disagree",
    x_low == "neutral"           ~ "Neutral",
    x_low == "agree"             ~ "Agree",
    x_low == "strongly agree"    ~ "Strongly Agree",
    TRUE                         ~ NA_character_
  )
  factor(x, levels = likert_levels, ordered = TRUE)
}

is_likert5_col <- function(x) {
  xx <- as.character(x)
  xx <- xx[!is.na(xx)]
  if (length(xx) == 0) return(FALSE)
  mean(tolower(stringr::str_squish(xx)) %in% tolower(likert_levels)) > 0.80
}

cat_cols <- df_resp %>% select(where(~ is.character(.x) || is.factor(.x))) %>% names()
likert_cols <- cat_cols[sapply(df_resp[cat_cols], is_likert5_col)]
if (length(likert_cols) == 0) stop("No Likert-5 attitude columns detected.")

# shorter labels
nice_item_label2 <- function(nm) {
  x <- nm
  if (stringr::str_detect(x, "_i_")) {
    x <- stringr::str_replace(x, "^.*_i_", "i_")
  } else if (stringr::str_detect(x, "_do_you_")) {
    x <- stringr::str_replace_all(x, "_", " ")
    x <- stringr::str_to_sentence(stringr::str_squish(x))
    return(stringr::str_trunc(x, 90))
  } else {
    x2 <- stringr::str_replace_all(x, "_", " ")
    x2 <- stringr::str_squish(x2)
    words <- unlist(strsplit(x2, " "))
    x <- paste(tail(words, 12), collapse = " ")
  }
  x <- stringr::str_replace_all(x, "_", " ")
  x <- stringr::str_squish(x)
  x <- stringr::str_to_sentence(x)
  stringr::str_trunc(x, width = 90, side = "right", ellipsis = "…")
}

label_map2 <- setNames(sapply(likert_cols, nice_item_label2), likert_cols)

att_df2 <- df_resp %>%
  select(all_of(likert_cols)) %>%
  rename_with(~ make.unique(label_map2[.x]), everything()) %>%
  mutate(across(everything(), standardize_likert))

att_long2 <- att_df2 %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "response") %>%
  filter(!is.na(response)) %>%
  count(item, response, name = "n") %>%
  group_by(item) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  tidyr::complete(
    item,
    response = factor(likert_levels, levels = likert_levels, ordered = TRUE),
    fill = list(n = 0, pct = 0)
  )

# classify mode for 3 separate plots
att_long2 <- att_long2 %>%
  mutate(mode = case_when(
    stringr::str_detect(tolower(item), "collecting point|collection point|collecting") ~ "Collection point",
    stringr::str_detect(tolower(item), "locker") ~ "Locker",
    stringr::str_detect(tolower(item), "home") ~ "Home delivery",
    TRUE ~ "Other"
  ))

# order items by mean score (for each mode plot)
score_map <- c("Strongly Disagree"=1, "Disagree"=2, "Neutral"=3, "Agree"=4, "Strongly Agree"=5)

plot_att_mode <- function(mode_name, filename) {
  d <- att_long2 %>% filter(mode == mode_name)
  if (nrow(d) == 0) {
    message("No items found for mode: ", mode_name, " (skipping).")
    return(invisible(NULL))
  }
  
  ord <- d %>%
    mutate(score = score_map[as.character(response)]) %>%
    group_by(item) %>%
    summarise(mean_score = sum(score * pct, na.rm = TRUE), .groups="drop") %>%
    arrange(mean_score) %>%
    pull(item)
  
  d <- d %>% mutate(item = factor(item, levels = ord))
  
  p <- ggplot(d, aes(x = item, y = pct, fill = response)) +
    geom_col(width = 0.85, color = "grey60", linewidth = 0.2) +  # border shows white neutral
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = likert_palette, drop = FALSE) +
    labs(title = paste0("Attitudes (Likert 5) — ", mode_name),
         x = NULL, y = "Share of responses", fill = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())
  
  save_and_show_plot(p, filename, w = 12, h = 7)
  invisible(p)
}

# Save + show 3 plots
plot_att_mode("Collection point", "plot_attitudes_collection_point.png")
plot_att_mode("Locker",           "plot_attitudes_locker.png")
plot_att_mode("Home delivery",    "plot_attitudes_home_delivery.png")

# Optional combined plot (all items)
p_all <- ggplot(att_long2, aes(x = forcats::fct_rev(factor(item)), y = pct, fill = response)) +
  geom_col(width = 0.85, color = "grey60", linewidth = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = likert_palette, drop = FALSE) +
  labs(title = "Attitudes (Likert 5) — All items",
       x = NULL, y = "Share of responses", fill = NULL) +
  theme_minimal()

save_and_show_plot(p_all, "plot_attitudes_all_items.png", w = 12, h = 10)

############################################################
# 4) SP PART — choice shares + attributes (x0_..x3_)
############################################################

if (!("choice_name" %in% names(df))) stop("choice_name not found in df.")
choice_share <- df %>%
  count(choice_name, name = "n") %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(n))

gt_choice <- choice_share %>%
  gt() %>%
  tab_header(title = "SP bundle choice shares (all tasks)") %>%
  fmt_percent(columns = share, decimals = 1)
save_gt(gt_choice, "table_sp_choice_shares.html")

p_choice <- ggplot(choice_share, aes(x = reorder(choice_name, n), y = share)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "SP bundle choice shares", x = NULL, y = "Share") +
  theme_minimal()

save_and_show_plot(p_choice, "plot_sp_choice_shares.png", w = 10, h = 6)

# ✅ IMPORTANT: your SP columns are x0_*, x1_*, x2_*, x3_*
alt_attr_cols <- names(df)[stringr::str_detect(names(df), "^x[0-3]_")]

if (length(alt_attr_cols) == 0) {
  message("No SP attribute columns like x0_*, x1_*, x2_*, x3_* detected. Skipping SP attribute plots.")
} else {
  
  sp_long <- df %>%
    select(response_id, choice_name, all_of(alt_attr_cols)) %>%
    pivot_longer(cols = all_of(alt_attr_cols), names_to = "alt_attr", values_to = "value") %>%
    tidyr::separate(alt_attr, into = c("alt_raw", "attr"), sep = "_", extra = "merge") %>%
    mutate(
      alt = as.integer(stringr::str_remove(alt_raw, "^x")),
      value_num = suppressWarnings(as.numeric(value))
    )
  
  write.csv(sort(unique(sp_long$attr)),
            file.path(out_dir, "sp_detected_attribute_names.csv"),
            row.names = FALSE)
  
  # Numeric distributions (cost/time)
  numeric_attrs <- c("cost", "time")
  present_num <- numeric_attrs[numeric_attrs %in% unique(sp_long$attr)]
  
  if (length(present_num) > 0) {
    p_num <- sp_long %>%
      filter(attr %in% present_num, !is.na(value_num)) %>%
      ggplot(aes(x = value_num)) +
      geom_histogram(bins = 30) +
      facet_grid(attr ~ alt, scales = "free_x") +
      labs(title = "SP attribute distributions by alternative (numeric)", x = NULL, y = "Count") +
      theme_minimal()
    
    save_and_show_plot(p_num, "plot_sp_attr_numeric_distributions.png", w = 12, h = 8)
  }
  
  # Categorical distributions (SAFE)
  cat_attrs <- setdiff(unique(sp_long$attr), numeric_attrs)
  sp_cat_data <- sp_long %>% filter(attr %in% cat_attrs, !is.na(value))
  
  if (nrow(sp_cat_data) > 0) {
    p_cat <- sp_cat_data %>%
      mutate(value = as.factor(value)) %>%
      count(attr, alt, value) %>%
      group_by(attr, alt) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = value, y = pct)) +
      geom_col() +
      facet_grid(attr ~ alt, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "SP attribute distributions by alternative (categorical)", x = NULL, y = "Share") +
      theme_minimal()
    
    save_and_show_plot(p_cat, "plot_sp_attr_categorical_distributions.png", w = 12, h = 10)
  } else {
    message("No categorical SP attributes with valid data — skipping categorical SP plot.")
  }
}

############################################################
# 5) DONE
############################################################
cat("\nDONE ✅\nAll outputs saved in the SAME folder as the CSV:\n", out_dir, "\n\n", sep = "")

# open the folder
browseURL(out_dir)
