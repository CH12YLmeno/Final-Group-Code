# ============================================================
# 0. Setup
# ============================================================

# Required packages are intentionally standard packages already used in the
# final clean-up file.
library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(forcats)
library(caret)
library(pROC)
library(randomForest)
library(broom)
library(knitr)
library(scales)

SEED <- 123
set.seed(SEED)

input_file <- "/Users/ch12yl/Downloads/2026T2/COMM3501/Assignment/A3 Group/A3_Dataset_2023.xlsx"
output_dir <- "model_outputs"

model_comparison_path <- file.path(output_dir, "model_comparison.csv")

if (!file.exists(model_comparison_path)) {
  stop("model_comparison.csv was not found. Please run Final Clean Up Code.Rmd first.")
}

model_comparison <- read_csv(
  model_comparison_path,
  show_col_types = FALSE
)

kable(
  model_comparison,
  caption = "Saved model comparison from Final Clean Up Code.Rmd"
)

complement_nb_rds <- file.path(output_dir, "complement_naive_bayes_model.rds")
logistic_rds <- file.path(output_dir, "logistic_regression_model.rds")
random_forest_rds <- file.path(output_dir, "random_forest_model.rds")

saved_model_check <- tibble(
  Model = c("Complement Naive Bayes", "Logistic Regression", "Random Forest"),
  File = c(complement_nb_rds, logistic_rds, random_forest_rds),
  Exists = file.exists(c(complement_nb_rds, logistic_rds, random_forest_rds))
)

kable(saved_model_check, caption = "Saved model availability check")

complement_naive_model <- readRDS(complement_nb_rds)
logistic_regression_model <- readRDS(logistic_rds)
random_forest_model <- readRDS(random_forest_rds)

data_raw <- read_excel(input_file)

data <- data_raw %>%
  clean_names() %>%
  mutate(
    across(where(is.character), ~ str_squish(.)),
    across(where(is.character), ~ na_if(., "")),
    across(where(is.character), ~ na_if(., "[Blank]"))
  )

data_clean <- data %>%
  dplyr::select(
    -recommendation_id,
    -request_id,
    -life_id,
    -external_ref
  ) %>%
  mutate(
    neos_flag = factor(
      if_else(underwriter == "NEOS Life", "Yes", "No"),
      levels = c("Yes", "No")
    ),
    life_bin = case_when(life == "Yes" ~ 1, life == "No" ~ 0, TRUE ~ NA_real_),
    tpd_bin = case_when(tpd == "Yes" ~ 1, tpd == "No" ~ 0, TRUE ~ NA_real_),
    trauma_bin = case_when(trauma == "Yes" ~ 1, trauma == "No" ~ 0, TRUE ~ NA_real_),
    ip_bin = case_when(ip == "Yes" ~ 1, ip == "No" ~ 0, TRUE ~ NA_real_),
    annual_income = if_else(annual_income < 0, NA_real_, annual_income),
    premium = if_else(premium < 0, NA_real_, premium),
    annualised_premium = if_else(annualised_premium < 0, NA_real_, annualised_premium),
    inside_super_premium = if_else(inside_super_premium < 0, NA_real_, inside_super_premium),
    outside_super_premium = if_else(outside_super_premium < 0, NA_real_, outside_super_premium),
    product_count = life_bin + tpd_bin + trauma_bin + ip_bin,
    has_alternative = if_else(is.na(alternative), 0, 1),
    total_cover = life_cover_amount + tpd_cover_amount +
      trauma_cover_amount + ip_cover_amount,
    log_annual_income = log1p(annual_income),
    log_premium = log1p(premium),
    log_annualised_premium = log1p(annualised_premium),
    log_total_cover = log1p(total_cover),
    product_bundle = case_when(
      life_bin == 1 & tpd_bin == 0 & trauma_bin == 0 & ip_bin == 0 ~ "Life only",
      life_bin == 0 & tpd_bin == 1 & trauma_bin == 0 & ip_bin == 0 ~ "TPD only",
      life_bin == 0 & tpd_bin == 0 & trauma_bin == 1 & ip_bin == 0 ~ "Trauma only",
      life_bin == 0 & tpd_bin == 0 & trauma_bin == 0 & ip_bin == 1 ~ "IP only",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 0 & ip_bin == 0 ~ "Life + TPD",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 1 & ip_bin == 0 ~ "Life + TPD + Trauma",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 1 & ip_bin == 1 ~ "Life + TPD + Trauma + IP",
      product_count >= 2 ~ "Other bundle",
      TRUE ~ "Other"
    )
  )

# Commission structure is organised consistently with the final clean-up file.
commission_text <- data_clean$commission_structure %>%
  as.character() %>%
  str_squish() %>%
  str_to_lower() %>%
  replace_na("unknown")

c_rate <- suppressWarnings(
  as.numeric(str_match(commission_text, "c(\\d+(?:\\.\\d+)?)")[, 2])
)

pair_rate <- str_match(
  commission_text,
  "(\\d+(?:\\.\\d+)?)\\s*%?\\s*/\\s*(\\d+(?:\\.\\d+)?)\\s*%?"
)

pair_initial <- suppressWarnings(as.numeric(pair_rate[, 2]))
pair_renewal <- suppressWarnings(as.numeric(pair_rate[, 3]))

init_modifier <- suppressWarnings(
  as.numeric(
    coalesce(
      str_match(commission_text, "(\\d+(?:\\.\\d+)?)\\s*%?\\s*(init|initial|year1|yr1)")[, 2],
      str_match(commission_text, "(init|initial|year1|yr1)\\s*(\\d+(?:\\.\\d+)?)\\s*%?")[, 3]
    )
  )
)

renew_modifier <- suppressWarnings(
  as.numeric(
    coalesce(
      str_match(commission_text, "(\\d+(?:\\.\\d+)?)\\s*%?\\s*(renew|renewal|year2|yr2)")[, 2],
      str_match(commission_text, "(renew|renewal|year2|yr2)\\s*(\\d+(?:\\.\\d+)?)\\s*%?")[, 3]
    )
  )
)

commission_type <- case_when(
  commission_text == "unknown" ~ "Unknown",
  str_detect(commission_text, "nil|nill|no commission|nil commission") ~ "No commission",
  str_detect(commission_text, "level|l2evel|0% level") ~ "Level",
  str_detect(commission_text, "upfront|higher initial|standard - upfront|initial \\(") ~ "Upfront / Higher initial",
  str_detect(commission_text, "hybrid|h4ybrid|h3ybrid|h2ybrid|hybrid60|hybrid70|hybrid80") ~ "Hybrid",
  TRUE ~ "Other specified"
)

base_initial <- case_when(
  commission_type == "Unknown" ~ NA_real_,
  commission_type == "No commission" ~ 0,
  commission_type == "Level" ~ 33,
  str_detect(commission_text, "2018|hybrid80|88\\s*/\\s*22") ~ 88,
  str_detect(commission_text, "2019|hybrid70|77\\s*/\\s*22") ~ 77,
  commission_type %in% c("Hybrid", "Upfront / Higher initial") ~ 66,
  TRUE ~ NA_real_
)

base_renewal <- case_when(
  commission_type == "Unknown" ~ NA_real_,
  commission_type == "No commission" ~ 0,
  commission_type == "Level" ~ 33,
  commission_type %in% c("Hybrid", "Upfront / Higher initial") ~ 22,
  TRUE ~ NA_real_
)

has_modifier_words <- str_detect(
  commission_text,
  "init|initial|renew|renewal|year1|year2"
)

use_pair_as_modifier <- has_modifier_words &
  !str_detect(commission_text, "yr1\\s*66|year1\\s*66|yr2\\s*22|year2\\s*22") &
  !is.na(pair_initial) &
  !is.na(pair_renewal) &
  pair_initial <= 100 &
  pair_renewal <= 100

initial_rate <- case_when(
  commission_type == "Unknown" ~ NA_real_,
  commission_type == "No commission" ~ 0,
  !is.na(c_rate) ~ base_initial * c_rate / 100,
  !is.na(init_modifier) ~ base_initial * init_modifier / 100,
  use_pair_as_modifier ~ base_initial * pair_initial / 100,
  !is.na(pair_initial) ~ pair_initial,
  commission_type %in% c("Hybrid", "Upfront / Higher initial", "Level") ~ base_initial,
  TRUE ~ NA_real_
)

renewal_rate <- case_when(
  commission_type == "Unknown" ~ NA_real_,
  commission_type == "No commission" ~ 0,
  !is.na(c_rate) ~ base_renewal * c_rate / 100,
  !is.na(renew_modifier) ~ base_renewal * renew_modifier / 100,
  use_pair_as_modifier ~ base_renewal * pair_renewal / 100,
  !is.na(pair_renewal) ~ pair_renewal,
  commission_type %in% c("Hybrid", "Upfront / Higher initial", "Level") ~ base_renewal,
  TRUE ~ NA_real_
)

data_clean <- data_clean %>%
  mutate(
    commission_type = factor(commission_type),
    initial_rate_numeric = initial_rate,
    renewal_rate_numeric = renewal_rate,
    initial_commission_rate_gst = factor(case_when(
      commission_text == "unknown" ~ "Unknown",
      is.na(initial_rate) ~ "Not stated in label",
      TRUE ~ paste0(round(initial_rate, 2), "%")
    )),
    renewal_commission_rate_gst = factor(case_when(
      commission_text == "unknown" ~ "Unknown",
      is.na(renewal_rate) ~ "Not stated in label",
      TRUE ~ paste0(round(renewal_rate, 2), "%")
    )),
    occupation_text = str_to_lower(as.character(occupation)),
    occupation_category = case_when(
      str_detect(occupation_text, "home duties|retired|student|unemployed") ~ "Home / retired / student",
      str_detect(occupation_text, "doctor|medical|nurse|dentist|physio|psychologist|pharmacist|veterinary|surgeon|therapist|paramedic") ~ "Healthcare professional",
      str_detect(occupation_text, "teacher|education|lecturer|academic|child care|teachers aide") ~ "Education / childcare",
      str_detect(occupation_text, "lawyer|solicitor|legal|accountant|architect|engineer|computer|programmer|analyst|consultant|actuary|scientist") ~ "Professional / technical",
      str_detect(occupation_text, "manager|management|chief executive|project manager|business development") ~ "Management",
      str_detect(occupation_text, "clerical|administration|clerk|receptionist|bookkeeper|office|bank") ~ "Clerical / administration",
      str_detect(occupation_text, "sales|real estate|retail|marketing") ~ "Sales / marketing",
      str_detect(occupation_text, "electrician|plumber|carpenter|builder|mechanic|fitter|cabinet|chef|hairdresser|trade|construction|foreman") ~ "Trade / skilled manual",
      str_detect(occupation_text, "driver|truck|mining|farming|police|fire|security|plant operator|manual|blue collar|heavy") ~ "Manual / field / higher risk",
      str_detect(occupation_text, "1a|1b|white collar|1p|1l|1m") ~ "Professional / technical",
      str_detect(occupation_text, "2a|2b|2c") ~ "White collar / clerical",
      str_detect(occupation_text, "3a|3b|3m|\\b4\\b|\\b5\\b") ~ "Manual / field / higher risk",
      TRUE ~ "Other"
    ),
    across(
      c(gender, smoker_status, home_state, self_employed,
        product_bundle, commission_type, occupation_category,
        premium_frequency, super, rollover_tax_rebate),
      as.factor
    )
  )

selected_models <- model_comparison %>%
  filter(
    Model %in% c(
      "Complement Naive Bayes",
      "Logistic Regression",
      "Random Forest"
    )
  ) %>%
  arrange(desc(AUC))

kable(
  selected_models,
  caption = "Models selected for detailed interpretation"
)

complement_nb_performance <- model_comparison %>%
  filter(Model == "Complement Naive Bayes")

kable(
  complement_nb_performance,
  caption = "Complement Naive Bayes performance"
)

complement_nb_top_features <- complement_naive_model$log_theta %>%
  group_by(Class) %>%
  slice_max(order_by = abs(Log_Theta), n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(Class, desc(abs(Log_Theta)))

kable(
  complement_nb_top_features,
  caption = "Complement Naive Bayes high-influence encoded features"
)

logistic_terms <- broom::tidy(
  logistic_regression_model,
  conf.int = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Odds_Ratio = exp(estimate),
    Direction = case_when(
      Odds_Ratio > 1 ~ "Increases odds of NEOS",
      Odds_Ratio < 1 ~ "Decreases odds of NEOS",
      TRUE ~ "Neutral"
    )
  ) %>%
  arrange(p.value)

logistic_top_terms <- logistic_terms %>%
  slice_head(n = 15) %>%
  dplyr::select(
    term,
    estimate,
    Odds_Ratio,
    p.value,
    Direction
  )

kable(
  logistic_top_terms,
  digits = 4,
  caption = "Top logistic regression terms by p-value"
)

logistic_direction_summary <- logistic_terms %>%
  count(Direction)

kable(
  logistic_direction_summary,
  caption = "Logistic regression direction summary"
)

logistic_top_terms %>%
  mutate(
    term = fct_reorder(term, Odds_Ratio)
  ) %>%
  ggplot(aes(x = term, y = Odds_Ratio, fill = Direction)) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey45") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Increases odds of NEOS" = "#5D3FD3",
      "Decreases odds of NEOS" = "#FFD700",
      "Neutral" = "grey70"
    )
  ) +
  labs(
    title = "Logistic Regression: Key Odds Ratios",
    x = "Predictor",
    y = "Odds ratio",
    fill = "Direction"
  ) +
  theme_minimal()

rf_importance <- importance(random_forest_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable")

importance_column <- if ("MeanDecreaseGini" %in% names(rf_importance)) {
  "MeanDecreaseGini"
} else {
  names(rf_importance)[ncol(rf_importance)]
}

rf_top_importance <- rf_importance %>%
  arrange(desc(.data[[importance_column]])) %>%
  slice_head(n = 15)

kable(
  rf_top_importance,
  digits = 4,
  caption = "Random Forest top variable importance"
)

rf_top_importance %>%
  mutate(
    Variable = fct_reorder(Variable, .data[[importance_column]])
  ) %>%
  ggplot(aes(x = Variable, y = .data[[importance_column]])) +
  geom_col(fill = "#483248", alpha = 0.9) +
  coord_flip() +
  labs(
    title = "Random Forest: Most Important Predictors",
    x = "Predictor",
    y = importance_column
  ) +
  theme_minimal()

cluster_base <- data_clean %>%
  dplyr::select(
    neos_flag,
    age_next,
    annual_income,
    log_annual_income,
    log_annualised_premium,
    log_total_cover,
    product_count,
    life_bin,
    tpd_bin,
    trauma_bin,
    ip_bin,
    has_alternative,
    initial_rate_numeric,
    renewal_rate_numeric,
    gender,
    smoker_status,
    home_state,
    self_employed,
    occupation_category,
    product_bundle,
    commission_type,
    premium_frequency,
    super
  ) %>%
  drop_na(
    age_next,
    log_annual_income,
    log_annualised_premium,
    log_total_cover,
    product_count,
    life_bin,
    tpd_bin,
    trauma_bin,
    ip_bin,
    has_alternative
  )

# Cap the clustering sample for speed while preserving class mix.
set.seed(SEED)
cluster_sample <- cluster_base %>%
  group_by(neos_flag) %>%
  slice_sample(prop = min(1, 30000 / nrow(cluster_base))) %>%
  ungroup()

cluster_numeric <- cluster_sample %>%
  dplyr::select(
    age_next,
    log_annual_income,
    log_annualised_premium,
    log_total_cover,
    product_count,
    life_bin,
    tpd_bin,
    trauma_bin,
    ip_bin,
    has_alternative
  )

cluster_scaled <- scale(cluster_numeric)

set.seed(SEED)
cluster_k_sample_size <- min(5000, nrow(cluster_scaled))
cluster_k_index <- sample(seq_len(nrow(cluster_scaled)), cluster_k_sample_size)
cluster_scaled_k_search <- cluster_scaled[cluster_k_index, , drop = FALSE]

k_search_table <- tibble(k = 2:6) %>%
  mutate(
    kmeans_model = map(
      k,
      ~ kmeans(
        cluster_scaled_k_search,
        centers = .x,
        nstart = 10,
        iter.max = 50
      )
    ),
    total_withinss = map_dbl(kmeans_model, "tot.withinss"),
    avg_silhouette = map2_dbl(
      kmeans_model,
      k,
      ~ mean(
        cluster::silhouette(
          .x$cluster,
          dist(cluster_scaled_k_search)
        )[, "sil_width"]
      )
    )
  ) %>%
  dplyr::select(
    k,
    total_withinss,
    avg_silhouette
  )

best_cluster_k <- k_search_table %>%
  arrange(desc(avg_silhouette)) %>%
  slice_head(n = 1) %>%
  pull(k)

kable(
  k_search_table,
  digits = 4,
  caption = "Best cluster number check using within-cluster sum of squares and silhouette score"
)

best_cluster_k

k_search_table %>%
  ggplot(aes(x = k, y = avg_silhouette)) +
  geom_line(colour = "#483248", linewidth = 1.1) +
  geom_point(colour = "#483248", fill = "#FFD700", size = 3, shape = 21) +
  scale_x_continuous(breaks = k_search_table$k) +
  labs(
    title = "Choosing The Number Of Customer Persona Clusters",
    subtitle = "Higher average silhouette indicates clearer separation between clusters",
    x = "Number of clusters (k)",
    y = "Average silhouette score"
  ) +
  theme_minimal()

# The final number of clusters is selected by the silhouette check above.
set.seed(SEED)
cluster_model <- kmeans(
  cluster_scaled,
  centers = best_cluster_k,
  nstart = 25,
  iter.max = 100
)

cluster_results <- cluster_sample %>%
  mutate(
    cluster = factor(cluster_model$cluster)
  )

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

cluster_personas <- cluster_results %>%
  group_by(cluster) %>%
  summarise(
    Records = n(),
    NEOS_Rate = mean(neos_flag == "Yes"),
    Median_Age = median(age_next, na.rm = TRUE),
    Median_Income = median(annual_income, na.rm = TRUE),
    Median_Annualised_Premium = median(expm1(log_annualised_premium), na.rm = TRUE),
    Median_Total_Cover = median(expm1(log_total_cover), na.rm = TRUE),
    Average_Product_Count = mean(product_count, na.rm = TRUE),
    Life_Rate = mean(life_bin == 1, na.rm = TRUE),
    TPD_Rate = mean(tpd_bin == 1, na.rm = TRUE),
    Trauma_Rate = mean(trauma_bin == 1, na.rm = TRUE),
    IP_Rate = mean(ip_bin == 1, na.rm = TRUE),
    Top_Gender = mode_value(gender),
    Top_Smoker_Status = mode_value(smoker_status),
    Top_Occupation = mode_value(occupation_category),
    Top_Product_Bundle = mode_value(product_bundle),
    Top_Commission_Type = mode_value(commission_type),
    .groups = "drop"
  ) %>%
  mutate(
    Persona_Label = case_when(
      Average_Product_Count >= 3 ~ "Multi-product comprehensive cover",
      IP_Rate == max(IP_Rate) ~ "Income protection focused",
      Median_Annualised_Premium == max(Median_Annualised_Premium) ~ "High-premium / high-cover profile",
      TRUE ~ "Core life-insurance profile"
    )
  ) %>%
  relocate(Persona_Label, .after = cluster)

kable(
  cluster_personas,
  digits = 3,
  caption = "Customer persona clusters"
)

# Clustering is unsupervised, so this is not a true supervised model test.
# The following maps each cluster to its majority class to check whether the
# personas align with NEOS vs non-NEOS recommendation behaviour.
cluster_class_map <- cluster_results %>%
  count(cluster, neos_flag) %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(
    cluster,
    cluster_predicted_class = neos_flag
  )

cluster_classification <- cluster_results %>%
  left_join(cluster_class_map, by = "cluster") %>%
  mutate(
    cluster_predicted_class = factor(
      cluster_predicted_class,
      levels = c("Yes", "No")
    ),
    neos_flag = factor(
      neos_flag,
      levels = c("Yes", "No")
    )
  )

cluster_confusion_matrix <- confusionMatrix(
  data = cluster_classification$cluster_predicted_class,
  reference = cluster_classification$neos_flag,
  positive = "Yes"
)

cluster_confusion_matrix

cluster_confusion_summary <- tibble(
  Accuracy = as.numeric(cluster_confusion_matrix$overall["Accuracy"]),
  Sensitivity = as.numeric(cluster_confusion_matrix$byClass["Sensitivity"]),
  Specificity = as.numeric(cluster_confusion_matrix$byClass["Specificity"]),
  Precision = as.numeric(cluster_confusion_matrix$byClass["Pos Pred Value"]),
  F1 = as.numeric(cluster_confusion_matrix$byClass["F1"]),
  Balanced_Accuracy = as.numeric(cluster_confusion_matrix$byClass["Balanced Accuracy"])
)

kable(
  cluster_confusion_summary,
  digits = 4,
  caption = "Cluster-to-class confusion matrix summary"
)

cluster_personas %>%
  ggplot(aes(x = cluster, y = NEOS_Rate, fill = Persona_Label)) +
  geom_col(alpha = 0.9) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Multi-product comprehensive cover" = "#483248",
      "Income protection focused" = "#5D3FD3",
      "High-premium / high-cover profile" = "#FFD700",
      "Core life-insurance profile" = "#B57EDC"
    )
  ) +
  labs(
    title = "NEOS Recommendation Rate By Customer Persona Cluster",
    x = "Cluster",
    y = "NEOS recommendation rate",
    fill = "Persona"
  ) +
  theme_minimal()

best_classification_model <- model_comparison %>%
  arrange(desc(AUC)) %>%
  slice_head(n = 1) %>%
  pull(Model)

highest_neos_cluster <- cluster_personas %>%
  arrange(desc(NEOS_Rate)) %>%
  slice_head(n = 1)

interpretation_summary <- tibble(
  Area = c(
    "Complement Naive Bayes",
    "Logistic Regression",
    "Random Forest",
    "Clustering"
  ),
  Role_In_Project = c(
    "Benchmark model designed to be more robust for imbalanced classification",
    "Explainable model for direction and odds of NEOS recommendation",
    "Strong predictive model for non-linear customer/product/adviser patterns",
    "Customer persona model for segment interpretation"
  ),
  Business_Use = c(
    "Broad screening of possible NEOS opportunities",
    "Explains which variables increase or decrease NEOS likelihood",
    "Ranks important drivers for targeting and adviser strategy",
    paste0(
      "Identifies high-value persona groups; highest observed NEOS-rate cluster is Cluster ",
      highest_neos_cluster$cluster
    )
  )
)

kable(
  interpretation_summary,
  caption = "Interpretation summary for final presentation"
)
