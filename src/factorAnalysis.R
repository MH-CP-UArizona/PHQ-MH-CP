# Load required libraries
library(tidyverse)
library(psych)  # For performing factor analysis
library(UpSetR) # For performing upset plots
library(here)
library(lavaan)
library(semPlot)
library(gt)

# Load the data (update the file path accordingly)
data <- read.csv(here("analysis", "data", "derivedData", "GC_20241021_NHISadult2019_data_newvars.csv"))

# Filter for depression-positive individuals
depression_positive_data <- data %>% filter(depression == 1)

# Select symptom severity variables and ChronicPain_any
symptom_vars <- c("anhedonia", "sadness", "sleep", "energy", "appetite", "guilt", "concentration", "psychomotor")
cp_indicator <- "ChronicPain_any"
symptom_data <- depression_positive_data %>% select(all_of(symptom_vars), all_of(cp_indicator))

# Subset the data into groups with and without chronic pain
with_cp <- symptom_data %>% filter(!!sym(cp_indicator) == 1) %>% select(all_of(symptom_vars))
without_cp <- symptom_data %>% filter(!!sym(cp_indicator) == 0) %>% select(all_of(symptom_vars))

# Perform factor analysis for individuals with chronic pain
fa_with_cp <- fa(with_cp, nfactors = 1, rotate = "varimax")
print("Factor Analysis for Individuals WITH Chronic Pain")
print(fa_with_cp)
fa.diagram(fa_with_cp)
??fa
# Perform factor analysis for individuals without chronic pain
fa_without_cp <- fa(without_cp, nfactors = 1, rotate = "varimax")
print("Factor Analysis for Individuals WITHOUT Chronic Pain")
print(fa_without_cp)
fa.diagram(fa_without_cp)

# Compare Factor Scores Between Subgroups
# Calculate factor scores for the entire dataset
fa_full <- fa(symptom_data %>% select(all_of(symptom_vars)), nfactors = 1, rotate = "varimax")
factor_scores <- factor.scores(symptom_data %>% select(all_of(symptom_vars)), fa_full)$scores

# Add factor scores to the original dataset
symptom_data <- symptom_data %>% mutate(Factor_Score = factor_scores[, 1])

# Perform t-test to compare factor scores between individuals with and without chronic pain
t_test <- t.test(Factor_Score ~ ChronicPain_any, data = symptom_data)
print("T-test Results for Factor Scores Between Subgroups")
print(t_test)

# Convert symptom severity to binary (e.g., 1 for severity > 0, 0 otherwise)
symptom_data_binary <- symptom_data %>%
  mutate(across(all_of(symptom_vars), ~ ifelse(. > 0, 1, 0)))

# Transform symptom variable names to title case
symptom_vars_title <- str_to_title(symptom_vars)

# Rename columns in the dataset
colnames(symptom_data_binary)[1:length(symptom_vars)] <- symptom_vars_title

# Separate individuals with and without chronic pain
with_cp <- symptom_data_binary %>% filter(!!sym(cp_indicator) == 1) %>% select(all_of(symptom_vars_title))
without_cp <- symptom_data_binary %>% filter(!!sym(cp_indicator) == 0) %>% select(all_of(symptom_vars_title))

# Create UpSet plots for each group
par(mfrow = c(1, 2))  # Set up side-by-side layout for plots

## MAKE ANHEDONIA MOD-SEVERE AND ALSO SEVERE
# Plot for individuals with chronic pain
upset(
  with_cp,
  sets = symptom_vars_title,
  nsets = length(symptom_vars_title),
  nintersects = 30,
  order.by = "freq",
  main.bar.color = "darkblue",
  sets.bar.color = "darkred",
  text.scale = 1.75,
  point.size = 3
)

# Plot for individuals without chronic pain
upset(
  without_cp,
  sets = symptom_vars_title,
  nsets = length(symptom_vars_title),
  nintersects = 30,
  order.by = "freq",
  main.bar.color = "darkgreen",
  sets.bar.color = "darkorange",
  text.scale = 1.75,
  point.size = 3
) 

# Prepare the data for CFA
# Ensure no missing values and subset only symptom severity variables
cfa_data <- symptom_data %>% select(all_of(symptom_vars)) %>% na.omit()

# Define the CFA model
# Hypothesize that a single latent variable (Chronic Pain) explains the observed symptoms
cfa_model <- '
  ChronicPain =~ anhedonia + sadness + sleep + energy + appetite + guilt + concentration
'

# Fit the CFA model
fit <- cfa(cfa_model, data = cfa_data, std.lv = TRUE)

# Summarize the model results
# Extract parameter estimates as a data frame
cfa_results <- as.data.frame(parameterEstimates(fit, standardized = TRUE))

# Filter and clean the table to keep only factor loadings (latent =~ observed)
clean_table <- cfa_results %>%
  filter(op == "=~") %>%  # Keep only factor loadings
  select(rhs, est, se, z, pvalue, std.all) %>%
  rename(
    Observed_Variable = rhs,
    Estimate = est,
    Std_Error = se,
    Z_Value = z,
    P_Value = pvalue,
    Std_Loading = std.all
  )

# Extract model fit information
fit_measures <- fitMeasures(fit, c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))

# Format model fit into a string
fit_text <- paste(
  "**Model Fit:**", "<br>",
  "CFI =", round(fit_measures["cfi"], 2), "<br>",
  "TLI =", round(fit_measures["tli"], 2), "<br>",
  "RMSEA =", round(fit_measures["rmsea"], 2), " (90% CI: ", round(fit_measures["rmsea.ci.lower"], 2), "-", round(fit_measures["rmsea.ci.upper"], 2), ")<br>",
  "SRMR =", round(fit_measures["srmr"], 2), "<br>",
  "AIC =", round(fit_measures["aic"], 2), "<br>",
  "BIC =", round(fit_measures["bic"], 2)
)

# Create a gt table with the model fit in the footer
gt_table <- clean_table %>%
  gt() %>%
  tab_header(
    title = "CFA Results",
    subtitle = "Confirmatory Factor Analysis with Model Fit Information"
  ) %>%
  cols_label(
    Observed_Variable = "Observed Variable",
    Estimate = "Estimate",
    Std_Error = "Standard Error",
    Z_Value = "Z Value",
    P_Value = "P Value",
    Std_Loading = "Standardized Loading"
  ) %>%
  fmt_number(columns = c(Estimate, Std_Error, Z_Value, Std_Loading), decimals = 2) %>%
  fmt_number(columns = P_Value, decimals = 3) %>%
  tab_source_note(
    source_note = md(fit_text)  # Include model fit information with line breaks
  ) %>%
  tab_options(table.font.size = "small")

# Save the gt table as an image
gtsave(gt_table, "analysis/figures/cfa_table.png")

# Visualize the model (optional)
semPaths(fit, what = "std", edge.label.cex = 1.2, layout = "tree", title = TRUE)

## CORRESPONDENSE ANALYSIS


