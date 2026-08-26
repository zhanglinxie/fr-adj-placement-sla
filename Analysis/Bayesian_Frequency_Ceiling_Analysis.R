# ==============================================================================
# Bayesian analysis of adjective placement accuracy
# Group x Adjective Type x Continuous Frequency
# ==============================================================================

library(tidyverse)
library(readxl)
library(lme4)
library(brms)
library(bayesplot)
library(posterior)
library(bayestestR)
library(emmeans)
library(sjPlot)
library(scales)


# ==============================================================================
# 1. Paths and analysis options
# ==============================================================================

project_dir <- "/Users/lucie/Desktop/Project1_Overgeneralization/Data"
setwd(project_dir)

# ==============================================================================
# 2. Load and combine datasets
# ==============================================================================

# ----- 2.1 L2 instructed participants -----

Data1 <- read_excel("expCh_5_sujet_2.xlsx")[-c(32, 37), ]
Data2 <- read_excel("expCh_5.xlsx")

Data_instructed <- merge(Data1, Data2, by = "ID") %>%
  select(ID, Group, Item, Condition, Accuracy) %>%
  mutate(
    LangEnvironment = "Instructed",
    # IDs are globally unique; convert to a common type before bind_rows().
    ID = as.character(ID),
    # Excel may infer different column types in different workbooks. Normalize
    # them before bind_rows() so, for example, Item is not character in one
    # dataset and double in another.
    Group = as.character(Group),
    Item = as.character(Item),
    Condition = as.character(Condition),
    Accuracy = suppressWarnings(as.integer(as.character(Accuracy)))
  )

# ----- 2.2 L2 immersed participants -----

expCh_immersion <- read_excel("expCh_immersionAdv.xlsx")
expCh_immersion_subject <- read_excel("expCh_immersion_subject.xlsx")

Data_immersed <- merge(
  expCh_immersion,
  expCh_immersion_subject,
  by = "ID"
) %>%
  select(ID, Group, Item, Condition, Accuracy) %>%
  mutate(
    LangEnvironment = "Immersed",
    ID = as.character(ID),
    Group = as.character(Group),
    Item = as.character(Item),
    Condition = as.character(Condition),
    Accuracy = suppressWarnings(as.integer(as.character(Accuracy)))
  )

# ----- 2.3 L1 French participants -----

Data_L1 <- read_excel("expFr_5.xlsx") %>%
  select(ID, Group, Item, Condition, Accuracy) %>%
  mutate(
    LangEnvironment = "Native",
    ID = as.character(ID),
    Group = as.character(Group),
    Item = as.character(Item),
    Condition = as.character(Condition),
    Accuracy = suppressWarnings(as.integer(as.character(Accuracy)))
  )

# ----- 2.4 Combine all datasets -----

Data <- bind_rows(Data_instructed, Data_immersed, Data_L1) %>%
  mutate(
    ID = factor(ID),
    Item = as.character(Item),
    Condition = factor(Condition),
    Group = factor(
      Group,
      levels = c("Beginner", "Intermediate", "Advanced", "L1French")
    ),
    LangEnvironment = factor(
      LangEnvironment,
      levels = c("Instructed", "Immersed", "Native")
    ),
    Accuracy = suppressWarnings(as.integer(as.character(Accuracy)))
  )

# Basic data checks.
if (anyNA(Data$Accuracy)) {
  stop("Accuracy contains missing or non-numeric values after conversion.")
}

if (!all(Data$Accuracy %in% c(0L, 1L))) {
  stop("Accuracy must contain only 0 and 1.")
}

if (anyNA(Data$Group)) {
  stop("Some Group values do not match the expected four group labels.")
}

# Each participant ID should belong to exactly one group/environment. This
# catches accidental reuse of an ID across source files before model fitting.
ID_check <- Data %>%
  distinct(ID, Group, LangEnvironment) %>%
  count(ID, name = "N_memberships")

if (any(ID_check$N_memberships != 1)) {
  stop("At least one participant ID belongs to multiple groups or environments.")
}

# ==============================================================================
# 3. Create adjective-type and frequency predictors
# ==============================================================================

# Explicit effect coding:
#   PreN  = -0.5
#   PostN = +0.5
# With this coding, the Adj_Type_c coefficient represents the full PostN-PreN
# contrast for the reference group at mean frequency.

Data <- Data %>%
  mutate(
    Adj_Type = if_else(
      Condition %in% c("PostN_Unknown", "PostN_Known"),
      "PostN_Adj",
      "PreN_Adj"
    ),
    Adj_Type = factor(
      Adj_Type,
      levels = c("PreN_Adj", "PostN_Adj")
    ),
    Adj_Type_c = if_else(Adj_Type == "PostN_Adj", 0.5, -0.5)
  )

# Item-frequency dictionary.
freq_dictionary <- tibble(
  Item = as.character(1:24),
  FrequencyNb = c(
    671.86, 226.45, 1106.8, 209.45, 252.69, 76.43,
    27.51, 2.65, 2.65, 9.46, 29.36, 44.64,
    52.59, 12.14, 12.31, 69.17, 23.09, 17.07,
    1.23, 1.11, 0.70, 0.78, 1.73, 4.67
  )
)

Data <- Data %>%
  left_join(freq_dictionary, by = "Item")

if (anyNA(Data$FrequencyNb)) {
  missing_items <- Data %>%
    filter(is.na(FrequencyNb)) %>%
    distinct(Item) %>%
    pull(Item)

  stop(
    "Frequency is missing for item(s): ",
    paste(missing_items, collapse = ", ")
  )
}

Data <- Data %>%
  mutate(
    Log_Freq = log10(FrequencyNb),
    # centering and scaling the continuous frequency predictor
    FrequencyCont_c = (Log_Freq - mean(Log_Freq, na.rm = TRUE)) / sd(Log_Freq, na.rm = TRUE),
    Item = factor(Item)
  )

View(Data)

write_csv(
  Data,
  file.path(project_dir, "data_Aug2026.csv")
)

# ==============================================================================
# 4. Descriptive accuracy and near-ceiling diagnostics
# ==============================================================================

accuracy_by_cell <- Data %>%
  group_by(Group, Adj_Type) %>%
  summarise(
    N = n(),
    Correct = sum(Accuracy == 1),
    Errors = sum(Accuracy == 0),
    Accuracy_rate = mean(Accuracy),
    .groups = "drop"
  )

cat("\nAccuracy by Group x Adjective Type:\n")
print(accuracy_by_cell)

write.csv(
  accuracy_by_cell,
  "accuracy_by_group_and_adjective_type.csv",
  row.names = FALSE
)

# Participant-level distribution: identifies how many participants are perfect
# in each Group x Adjective Type cell.
accuracy_by_participant <- Data %>%
  group_by(Group, Adj_Type, ID) %>%
  summarise(
    N = n(),
    Errors = sum(Accuracy == 0),
    Accuracy_rate = mean(Accuracy),
    .groups = "drop"
  )

ceiling_by_cell <- accuracy_by_participant %>%
  group_by(Group, Adj_Type) %>%
  summarise(
    N_participants = n(),
    N_perfect_participants = sum(Accuracy_rate == 1),
    Proportion_perfect = mean(Accuracy_rate == 1),
    Mean_participant_accuracy = mean(Accuracy_rate),
    .groups = "drop"
  )

cat("\nParticipant-level ceiling diagnostics:\n")
print(ceiling_by_cell)

write.csv(
  ceiling_by_cell,
  "ceiling_diagnostics_by_cell.csv",
  row.names = FALSE
)

# Inspect where the five Advanced PostN errors occur.
advanced_postn_errors_by_ID <- Data %>%
  filter(Group == "Advanced", Adj_Type == "PostN_Adj") %>%
  group_by(ID) %>%
  summarise(
    N = n(),
    Errors = sum(Accuracy == 0),
    Accuracy_rate = mean(Accuracy),
    .groups = "drop"
  ) %>%
  arrange(desc(Errors), ID)

advanced_postn_errors_by_Item <- Data %>%
  filter(Group == "Advanced", Adj_Type == "PostN_Adj") %>%
  group_by(Item) %>%
  summarise(
    N = n(),
    Errors = sum(Accuracy == 0),
    Accuracy_rate = mean(Accuracy),
    FrequencyNb = first(FrequencyNb),
    .groups = "drop"
  ) %>%
  arrange(desc(Errors), Item)

cat("\nAdvanced PostN errors by participant:\n")
print(advanced_postn_errors_by_ID)

cat("\nAdvanced PostN errors by item:\n")
print(advanced_postn_errors_by_Item)

# ==============================================================================
# 5. Descriptive plots
# ==============================================================================

p_frequency_raw <- ggplot(
  Data,
  aes(
    x = FrequencyNb,
    y = Accuracy,
    color = Adj_Type,
    fill = Adj_Type
  )
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    linewidth = 1.1,
    alpha = 0.20
  ) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000")
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-0.05, 1.05)
  ) +
  scale_color_manual(
    values = c("PreN_Adj" = "#E64B35FF", "PostN_Adj" = "#4DBBD5FF"),
    labels = c("Prenominal", "Postnominal")
  ) +
  scale_fill_manual(
    values = c("PreN_Adj" = "#E64B35FF", "PostN_Adj" = "#4DBBD5FF"),
    labels = c("Prenominal", "Postnominal")
  ) +
  facet_wrap(~ Group, nrow = 2)+
  theme_bw(base_size = 14) +
  labs(
    x = "Word Frequency (log10 scale)",
    y = "Accuracy",
    color = "Adjective Type",
    fill = "Adjective Type" ) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_frequency_raw)

ggsave(
  "descriptive_frequency_by_group.png",
  p_frequency_raw,
  dpi = 300
)

# ----- Item-level descriptive accuracy -----

lexical_dictionary <- tibble(
  Item = factor(as.character(1:24), levels = levels(Data$Item)),
  item_label = c(
    "beau", "joli", "petit", "nouveau", "mauvais", "excellent",
    "fichu", "piètre", "fieffé", "soi-disant", "véritable", "nombreux",
    "vert", "grec", "rond", "intéressant", "professionnel",
    "scientifique", "bis", "suave", "mauve", "sabbatique",
    "folklorique", "domestique"
  )
)

item_accuracy <- Data %>%
  mutate(
    AnalysisGroup = if_else(Group == "L1French", "L1 French", "L2 learners"),
    AnalysisGroup = factor(
      AnalysisGroup,
      levels = c("L2 learners", "L1 French")
    )
  ) %>%
  group_by(Item, AnalysisGroup) %>%
  summarise(
    Accuracy_rate = mean(Accuracy),
    SD = sd(Accuracy),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  ) %>%
  left_join(lexical_dictionary, by = "Item")

l2_item_order <- item_accuracy %>%
  filter(AnalysisGroup == "L2 learners") %>%
  arrange(Accuracy_rate) %>%
  pull(item_label)

item_accuracy <- item_accuracy %>%
  mutate(item_label = factor(item_label, levels = l2_item_order))

p_item_accuracy <- ggplot(
  item_accuracy,
  aes(x = item_label, y = Accuracy_rate, fill = AnalysisGroup)
) +
  geom_col(position = "dodge", width = 0.70) +
  geom_text(
    aes(label = scales::percent(Accuracy_rate, accuracy = 0.1)),
    hjust = -0.05,
    size = 2.7
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  coord_flip() +
  facet_grid(. ~ AnalysisGroup) +
  labs(x = "Item", y = "Accuracy") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

print(p_item_accuracy)
ggsave(
  "item_accuracy_L2_vs_L1.png",
  p_item_accuracy,
  width = 10,
  height = 8,
  dpi = 300
)

# ==============================================================================
# 6. Bayesian model: all four groups
# ==============================================================================

priors_maximal <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(lkj(2), class = "cor")
)

bayes_formula_maximal <- bf(
  Accuracy ~ Group * Adj_Type_c * FrequencyCont_c +
    (FrequencyCont_c * Adj_Type_c | ID) +
    (Group | Item)
)

Bays_model_maximal <- brm(
  formula = bayes_formula_maximal,
  data = Data,
  family = bernoulli(link = "logit"),
  prior = priors_maximal,
  iter = 8000,
  warmup = 4000,
  chains = 4,
  cores = 4,
  seed = 1234,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  ),
  save_pars = save_pars(all = TRUE)
)

saveRDS(Bays_model_maximal, "Bays_model_maximal.rds")

print(summary(Bays_model_maximal))
print(prior_summary(Bays_model_maximal))
print(p_direction(Bays_model_maximal))


# ----- Sampler diagnostics -----

nuts_full <- nuts_params(Bays_model_maximal)

n_divergent <- sum(
  nuts_full$Parameter == "divergent__" & nuts_full$Value == 1
)

treedepth_values <- nuts_full$Value[
  nuts_full$Parameter == "treedepth__"
]

max_treedepth_observed <- if (length(treedepth_values) > 0) {
  max(treedepth_values)
} else {
  NA_real_
}

cat("\nNumber of divergent transitions:", n_divergent, "\n")
cat("Maximum observed tree depth:", max_treedepth_observed, "\n")

print(plot(Bays_model_maximal))
print(pp_check(Bays_model_maximal, type = "bars", ndraws = 100))


# ----- Posterior plots -----

post_mat_freq <- as.matrix(as_draws_df(Bays_model_maximal))

requested_parameters <- c(
  "b_GroupAdvanced",
  "b_GroupIntermediate",
  "b_GroupL1French",
  "b_Adj_Type_c",
  "b_FrequencyCont_c",
  "b_GroupAdvanced:Adj_Type_c",
  "b_GroupIntermediate:Adj_Type_c",
  "b_GroupL1French:Adj_Type_c",
  "b_GroupAdvanced:FrequencyCont_c",
  "b_GroupIntermediate:FrequencyCont_c",
  "b_GroupL1French:FrequencyCont_c",
  "b_Adj_Type_c:FrequencyCont_c",
  "b_GroupAdvanced:Adj_Type_c:FrequencyCont_c",
  "b_GroupIntermediate:Adj_Type_c:FrequencyCont_c",
  "b_GroupL1French:Adj_Type_c:FrequencyCont_c"
)

available_parameters <- intersect(
  requested_parameters,
  colnames(post_mat_freq)
)

missing_parameters <- setdiff(
  requested_parameters,
  colnames(post_mat_freq)
)

if (length(missing_parameters) > 0) {
  warning(
    "The following posterior parameters were not found: ",
    paste(missing_parameters, collapse = ", ")
  )
}

p_fixed_effects <- mcmc_areas(
  post_mat_freq,
  pars = available_parameters,
  prob = 0.95,
  prob_outer = 0.99,
  point_est = "mean"
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#D62728",
    linewidth = 0.8
  ) +
  labs(
    title = "Posterior Distributions of Fixed Effects",
    subtitle = "Group, Adjective Type, and Continuous Frequency",
    x = "Posterior estimate (log-odds)",
    y = "Parameter"
  ) +
  theme_minimal(base_size = 13)

print(p_fixed_effects)
ggsave(
  "posterior_fixed_effects.png",
  p_fixed_effects,
  width = 10,
  height = 9,
  dpi = 300
)

p_model_predictions <- plot_model(
  Bays_model_maximal,
  type = "pred",
  terms = c(
    "FrequencyCont_c",
    "Adj_Type_c [-0.5,0.5]",
    "Group"
  ),
  title = "Frequency and Adjective Type across Groups",
  legend.title = "Adjective Type"
) +
  scale_color_discrete(labels = c("Prenominal", "Postnominal")) +
  scale_fill_discrete(labels = c("Prenominal", "Postnominal")) +
  theme_bw(base_size = 13)

print(p_model_predictions)
ggsave(
  "bayesian_predicted_frequency_interaction.png",
  p_model_predictions,
  width = 11,
  height = 7,
  dpi = 300
)



# ==============================================================================
# 7. Bayesian model: L2 learners only
# Beginner, Intermediate, and Advanced
# ==============================================================================

# ----- 7.1 Create L2-only dataset -----

Data_L2 <- Data %>%
  filter(Group != "L1French") %>%
  droplevels() %>%
  mutate(
    Group = factor(
      Group,
      levels = c("Beginner", "Intermediate", "Advanced")
    ),
    ID = droplevels(ID),
    Item = droplevels(Item)
  )

# Basic checks
cat("\nL2 observations:", nrow(Data_L2), "\n")
cat("L2 participants:", nlevels(Data_L2$ID), "\n")
cat("Items:", nlevels(Data_L2$Item), "\n")

cat("\nParticipants by L2 group:\n")
print(
  Data_L2 %>%
    distinct(ID, Group) %>%
    count(Group)
)

if (anyNA(Data_L2$Group)) {
  stop("Unexpected Group value in Data_L2.")
}

if (nlevels(Data_L2$Group) != 3) {
  stop("Data_L2 should contain exactly three Group levels.")
}

# ----- 7.2 Priors -----

priors_L2_maximal <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(lkj(2), class = "cor")
)

# ----- 7.3 Maximal model formula -----

bayes_formula_L2_maximal <- bf(
  Accuracy ~ Group * Adj_Type_c * FrequencyCont_c +
    (FrequencyCont_c * Adj_Type_c | ID) +
    (Group | Item)
)

cat("\nPriors available for the L2 maximal model:\n")
print(
  get_prior(
    formula = bayes_formula_L2_maximal,
    data = Data_L2,
    family = bernoulli(link = "logit")
  )
)

# ----- 7.4 Fit model -----

Bays_model_L2_maximal <- brm(
  formula = bayes_formula_L2_maximal,
  data = Data_L2,
  family = bernoulli(link = "logit"),
  prior = priors_L2_maximal,
  iter = 8000,
  warmup = 4000,
  chains = 4,
  cores = 4,
  seed = 1234,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  ),
  save_pars = save_pars(all = TRUE)
)

saveRDS(
  Bays_model_L2_maximal,
  "Bays_model_L2_maximal.rds"
)

print(summary(Bays_model_L2_maximal))
print(prior_summary(Bays_model_L2_maximal))
print(p_direction(Bays_model_L2_maximal))


# ----- 7.5 Sampler diagnostics -----

nuts_L2 <- nuts_params(Bays_model_L2_maximal)

n_divergent_L2 <- sum(
  nuts_L2$Parameter == "divergent__" &
    nuts_L2$Value == 1
)

treedepth_values_L2 <- nuts_L2$Value[
  nuts_L2$Parameter == "treedepth__"
]

max_treedepth_observed_L2 <- if (
  length(treedepth_values_L2) > 0
) {
  max(treedepth_values_L2)
} else {
  NA_real_
}

n_treedepth_hits_L2 <- sum(
  nuts_L2$Parameter == "treedepth__" &
    nuts_L2$Value >= 12
)

cat(
  "\nNumber of divergent transitions:",
  n_divergent_L2,
  "\n"
)

cat(
  "Maximum observed tree depth:",
  max_treedepth_observed_L2,
  "\n"
)

cat(
  "Transitions reaching max_treedepth:",
  n_treedepth_hits_L2,
  "\n"
)

print(plot(Bays_model_L2_maximal))

print(
  pp_check(
    Bays_model_L2_maximal,
    type = "bars",
    ndraws = 100
  )
)


# Posterior plots: L2 learners only

post_mat_L2 <- as.matrix(
  as_draws_df(Bays_model_L2_maximal)
)

# L2 model has only Intermediate and Advanced contrasts
# because Beginner is the reference group.

requested_parameters_L2 <- c(
  "b_GroupIntermediate",
  "b_GroupAdvanced",
  "b_Adj_Type_c",
  "b_FrequencyCont_c",
  "b_GroupIntermediate:Adj_Type_c",
  "b_GroupAdvanced:Adj_Type_c",
  "b_GroupIntermediate:FrequencyCont_c",
  "b_GroupAdvanced:FrequencyCont_c",
  "b_Adj_Type_c:FrequencyCont_c",
  "b_GroupIntermediate:Adj_Type_c:FrequencyCont_c",
  "b_GroupAdvanced:Adj_Type_c:FrequencyCont_c"
)

available_parameters_L2 <- intersect(
  requested_parameters_L2,
  colnames(post_mat_L2)
)

missing_parameters_L2 <- setdiff(
  requested_parameters_L2,
  colnames(post_mat_L2)
)

if (length(missing_parameters_L2) > 0) {
  warning(
    "The following L2 posterior parameters were not found: ",
    paste(
      missing_parameters_L2,
      collapse = ", "
    )
  )
}

# ----- 7.1 Fixed-effect posterior distributions -----

p_fixed_effects_L2 <- mcmc_areas(
  post_mat_L2,
  pars = available_parameters_L2,
  prob = 0.95,
  prob_outer = 0.99,
  point_est = "mean"
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#D62728",
    linewidth = 0.8
  ) +
  labs(
    title = "Posterior Distributions of Fixed Effects",
    subtitle = paste(
      "L2 learners:",
      "Group, Adjective Type, and Continuous Frequency"
    ),
    x = "Posterior estimate (log-odds)",
    y = "Parameter"
  ) +
  theme_minimal(base_size = 13)

print(p_fixed_effects_L2)

ggsave(
  "L2_posterior_fixed_effects.png",
  p_fixed_effects_L2,
  width = 10,
  height = 8,
  dpi = 300
)

# ----- 7.2 Predicted probabilities -----

p_model_predictions_L2 <- plot_model(
  Bays_model_L2_maximal,
  type = "pred",
  terms = c(
    "FrequencyCont_c",
    "Adj_Type_c [-0.5,0.5]",
    "Group"
  ),
  title = paste(
    "Frequency and Adjective Type",
    "across L2 Proficiency Groups"
  ),
  legend.title = "Adjective Type"
) +
  scale_color_discrete(
    labels = c("Prenominal", "Postnominal")
  ) +
  scale_fill_discrete(
    labels = c("Prenominal", "Postnominal")
  ) +
  theme_bw(base_size = 13)

print(p_model_predictions_L2)

ggsave(
  "L2_bayesian_predicted_frequency_interaction.png",
  p_model_predictions_L2,
  width = 11,
  height = 7,
  dpi = 300
)



# ==============================================================================
# Export parameter tables for the L2 maximal model
# ==============================================================================

library(tidyverse)
library(brms)
library(posterior)
library(bayestestR)

# Select model
model_for_table <- Bays_model_L2_maximal

# Output directory
output_dir <- if (exists("project_dir")) {
  project_dir
} else {
  getwd()
}

# ==============================================================================
# 1. Fixed-effect estimates and diagnostics
# ==============================================================================

model_summary <- summary(model_for_table)

fixed_summary_matrix <- model_summary$fixed

fixed_parameter_table_raw <- data.frame(
  Parameter = rownames(fixed_summary_matrix),
  fixed_summary_matrix,
  row.names = NULL,
  check.names = FALSE
) %>%
  as_tibble() %>%
  rename(
    Posterior_SD = Est.Error,
    CrI_lower = `l-95% CI`,
    CrI_upper = `u-95% CI`
  )

cat("\nFixed effects before adding pd:\n")
print(fixed_parameter_table_raw)


# ==============================================================================
# 2. Calculate probability of direction directly
# ==============================================================================

all_draws <- posterior::as_draws_df(
  model_for_table
)

# Remove pd columns that may have been created by previous attempts
fixed_parameter_table_raw <-
  fixed_parameter_table_raw %>%
  select(
    -any_of(
      c(
        "pd",
        "Direction",
        "pd.x",
        "pd.y",
        "Direction.x",
        "Direction.y"
      )
    )
  )

# Standardize parameter names
clean_parameter_name <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_trim(x)
  
  # Remove possible brms prefix
  x <- stringr::str_remove(
    x,
    "^b_"
  )
  
  # Convert (Intercept) to Intercept
  x <- stringr::str_replace(
    x,
    "^\\(Intercept\\)$",
    "Intercept"
  )
  
  # Remove possible backticks
  x <- stringr::str_replace_all(
    x,
    "`",
    ""
  )
  
  x
}

# Parameter names in the fixed-effect table
clean_fixed_names <- clean_parameter_name(
  fixed_parameter_table_raw$Parameter
)

# Corresponding posterior variable names
posterior_variable_names <- paste0(
  "b_",
  clean_fixed_names
)

cat("\nParameters in fixed-effect table:\n")
print(fixed_parameter_table_raw$Parameter)

cat("\nExpected posterior variable names:\n")
print(posterior_variable_names)

# Check that all variables exist
missing_draw_variables <- setdiff(
  posterior_variable_names,
  posterior::variables(all_draws)
)

if (length(missing_draw_variables) > 0) {
  
  cat("\nAvailable b parameters in posterior draws:\n")
  
  print(
    posterior::variables(all_draws) %>%
      stringr::str_subset("^b_")
  )
  
  stop(
    "The following posterior variables were not found: ",
    paste(
      missing_draw_variables,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 3. Calculate pd and direction in fixed-table order
# ==============================================================================

pd_results <- purrr::map_dfr(
  posterior_variable_names,
  function(variable_name) {
    
    parameter_draws <- as.numeric(
      all_draws[[variable_name]]
    )
    
    probability_positive <- mean(
      parameter_draws > 0,
      na.rm = TRUE
    )
    
    probability_negative <- mean(
      parameter_draws < 0,
      na.rm = TRUE
    )
    
    tibble(
      Direction = if_else(
        probability_positive >=
          probability_negative,
        "Positive",
        "Negative"
      ),
      
      pd = max(
        probability_positive,
        probability_negative
      )
    )
  }
)

# Add pd directly by row order
fixed_parameter_table_raw <-
  bind_cols(
    fixed_parameter_table_raw,
    pd_results
  ) %>%
  select(
    Parameter,
    Estimate,
    Posterior_SD,
    CrI_lower,
    CrI_upper,
    Direction,
    pd,
    Rhat,
    Bulk_ESS,
    Tail_ESS
  )

cat("\nComplete fixed-effect table with pd:\n")
print(
  fixed_parameter_table_raw,
  n = Inf
)
# ==============================================================================
# 4. Format fixed-effect table
# ==============================================================================

fixed_parameter_table_formatted <-
  fixed_parameter_table_raw %>%
  mutate(
    Estimate = round(Estimate, 2),
    Posterior_SD = round(Posterior_SD, 2),
    CrI_lower = round(CrI_lower, 2),
    CrI_upper = round(CrI_upper, 2),
    
    pd = paste0(
      sprintf("%.2f", pd * 100),
      "%"
    ),
    
    Rhat = round(Rhat, 2),
    Bulk_ESS = round(Bulk_ESS),
    Tail_ESS = round(Tail_ESS)
  ) %>%
  rename(
    Predictor = Parameter,
    `Posterior SD` = Posterior_SD,
    `95% CrI lower` = CrI_lower,
    `95% CrI upper` = CrI_upper,
    `Posterior direction` = Direction,
    `Probability of direction` = pd,
    `Bulk ESS` = Bulk_ESS,
    `Tail ESS` = Tail_ESS
  )

print(
  fixed_parameter_table_formatted,
  n = Inf
)
View(fixed_parameter_table_formatted)


readr::write_csv(
  fixed_parameter_table_formatted,
  file.path(
    output_dir,
    "L2_maximal_fixed_effects_formatted.csv"
  )
)
