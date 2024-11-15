####################################################################################################################
## Author: Greg Chism
## Date: November 2024
## email: gchism@arizona.edu
## Project: MH & CP MS3 Depression Symptoms
## Title: Descriptive analyses
####################################################################################################################

# Required Libraries
if(!require(pacman))  # Check if the `pacman` package is installed
  install.packages("pacman")  # If not installed, install `pacman` package

# Use `pacman` to load required libraries, installing any missing ones automatically
pacman::p_load(
  cowplot,          # Plot arrangement and combining
  ggthemes,         # Additional themes for ggplot2
  ggpubr,           # Publication-ready ggplot2 visualizations
  gt,               # Create tables with advanced formatting
  here,             # Manage file paths in a project
  nnet,             # Neural networks and multinomial models
  patchwork,        # Arrange ggplots into layouts
  psych,            # Various tools for psychological research
  rcompanion,       # Helper functions for data analysis
  scales,           # Scale functions for ggplot2
  survey,           # Analyze survey data
  tidymodels,       # Collection of packages for modeling and machine learning
  tidyverse,        # Collection of R packages for data science (ggplot2, dplyr, etc.)
  UpSetR,           # Create UpSet plots to visualize intersections
  waffle,           # Create waffle charts
  webshot           # Take web screenshots
)

# Load additional GitHub package for waffle plots
pacman::p_load_gh("liamgilbey/ggwaffle")

# Read in and glimpse data
data <- read_csv(here("analysis", "data", "rawData", "20241021_NHISadult2019_data_newvars.csv"))  # Load data from CSV file located in "data" folder using `here()`

# Mutually Exclusive Subpopulations: Create categorical variables for specific subpopulations based on conditions
df <- data %>%
  mutate(
    subpopulation = factor(case_when(
      LowerImpactCP_largedenom == 1 & PHQ8_count >= 10 ~ "Depression and Low Impact CP",  # Low impact chronic pain and depression (PHQ score >= 10)
      ChronicPain_any == 0 & PHQ8_count >= 10 ~ "Depression No CP",                       # Depression but no chronic pain
      HighImpactCP_largedenom == 1 & PHQ8_count >= 10 ~ "Depression and High Impact CP",  # High impact chronic pain and depression
      HighFAMImpactCP_largedenom == 1 & PHQ8_count >= 10 ~ "Depression and Family Impact CP" # Family impact chronic pain and depression
    ))
  ) |>
  filter(PHQ8_count >= 10)

# 1. Box Plot Figure
# Visualize PHQ Scores by Subpopulation using a box plot
fig1 <- df %>%
  mutate(subpopulation = fct_relevel(subpopulation, c("Depression No CP", "Depression and Low Impact CP", "Depression and High Impact CP"))) %>%   # Reorder levels of subpopulation factor
  drop_na(subpopulation) %>%  # Drop rows where subpopulation is NA
  ggplot(aes(x = PHQ8_count, y = subpopulation, fill = subpopulation)) +
  geom_boxplot(width = 0.5) +  # Create a box plot
  scale_fill_colorblind() +  # Apply colorblind-friendly colors
  labs(title = "PHQ Scores by Subpopulation", x = "PHQ Scores", y = "Subpopulation") +  # Set axis labels and title
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # Hide legend

print(fig1)

### TO DO Add pairwise testing (Dunn's test w/ post hoc)

# 2. Table with Population Proportions with 95% CI for Symptoms
symptoms <- c("NOanxiety", "NOdepression", "NOanhedonia", "NOblues", "NOsleep_probs", "NOenergy_probs", "NOeating_probs", "NOself_blame_probs", "NOconcentration_probs", "NOmoving_speed",
              "SOMEanxiety", "SOMEdepression", "SOMEanhedonia", "SOMEblues", "SOMEsleep_probs", "SOMEenergy_probs", "SOMEeating_probs", "SOMEself_blame_probs", "SOMEconcentration_probs", "SOMEmoving_speed",
              "MOSTLYanxiety", "MOSTLYdepression", "MOSTLYanhedonia", "MOSTLYblues", "MOSTLYsleep_probs", "MOSTLYenergy_probs", "MOSTLYeating_probs", "MOSTLYself_blame_probs", "MOSTLYconcentration_probs", "MOSTLYmoving_speed",
              "ALWAYSanxiety", "ALWAYSdepression", "ALWAYSanhedonia", "ALWAYSblues", "ALWAYSsleep_probs", "ALWAYSenergy_probs", "ALWAYSeating_probs", "ALWAYSself_blame_probs", "ALWAYSconcentration_probs", "ALWAYSmoving_speed")

# Calculate proportions and counts of different symptoms by subpopulation
prop_data <- df %>%
  group_by(subpopulation) %>%
  summarize(across(all_of(contains(symptoms)), list(
    prop = ~ mean(. == 1, na.rm = TRUE),  # Proportion of individuals with the symptom
    count = ~ sum(. == 1, na.rm = TRUE),  # Count of individuals with the symptom
    total = ~ n()                         # Total number of individuals in the subpopulation
  ), .names = "{col}_{fn}"),
  .groups = "drop")

# Prepare the data for visualization in a table format
prop_table <- prop_data %>%
  select(subpopulation, contains("_prop")) %>%
  pivot_longer(
    cols = -subpopulation,
    names_to = c("severity", "symptom"),
    names_pattern = "(NO|SOME|MOSTLY|ALWAYS)([A-Za-z]+)",
    values_to = "n"
  ) %>%
  mutate(n_count = round(n * 100),  # Convert proportion to count as a percentage
         symptom_labels = case_when(
           symptom == "anhedonia" ~ "Anhedonia",
           symptom == "blues" ~ "Blues",
           symptom == "eating" ~ "Eating Problems",
           symptom == "energy" ~ "Energy Problems",
           symptom == "self" ~ "Self Blame Problems",
           symptom == "sleep" ~ "Sleep Problems",
           symptom == "concentration" ~ "Concentration Problems",
           symptom == "moving" ~ "Moving Problems",
           TRUE ~ NA
         ),
         symptom_labels = factor(symptom_labels, levels = c("Anhedonia", "Blues", "Eating Problems", "Energy Problems", "Self Blame Problems", "Sleep Problems", "Concentration Problems", "Moving Problems")),
         severity = str_to_title(severity)) %>%  # Capitalize the severity levels
  drop_na(subpopulation)

prop_table_summary <- prop_table %>%
  mutate(
    severity = factor(severity, levels = c("Always", "Mostly", "Some", "No")),  # Order the severity levels
    fill_value = interaction(symptom_labels, severity)  # Combine symptom and severity for easier plotting
  ) %>%
  arrange(symptom_labels, severity)  # Sort data for plotting

# Sample size of individuals with depression for further calculations
sample_size <- 12670

# Calculate proportions across entire sample size
prop_data1 <- df %>%
  group_by(subpopulation) %>%
  summarize(
    across(
      all_of(contains(symptoms)),
      list(
        prop = ~ mean(. == 1, na.rm = TRUE),  # Proportion of individuals with the symptom
        count = ~ sum(. == 1, na.rm = TRUE),  # Count of individuals with the symptom
        total = ~ n(),                        # Total number of individuals in the subpopulation
        overall_prop = ~ sum(. == 1, na.rm = TRUE) / sample_size  # Proportion across the whole sample
      ),
      .names = "{col}_{fn}"
    ),
    .groups = "drop"
  )

# Pivot the proportions for easier visualization and summarization
prop_table_full <- prop_data1 %>%
  select(subpopulation, contains("_overall_prop")) %>%
  pivot_longer(
    cols = -subpopulation,
    names_to = c("severity", "symptom"),
    names_pattern = "(NO|SOME|MOSTLY|ALWAYS)([a-zA-Z_]+)_prop",
    values_to = "proportion"
  ) %>%
  mutate(
    prop_within_subpop = proportion,  # Proportion within subpopulation
    total_prop = proportion * sample_size,  # Total number of individuals across sample size
    n_count = round(proportion * 100),  # Convert proportions to count as percentage
    severity = str_to_title(severity)  # Capitalize the severity levels
  ) %>%
  drop_na(severity, symptom)

prop_table_summary_overall <- prop_table_full %>%
  filter(str_detect(symptom, "_overall")) %>%
  mutate(
    symptom = str_extract(symptom, "^[^_]+"),
    symptom_labels = case_when(
      symptom == "anhedonia" ~ "Anhedonia",
      symptom == "blues" ~ "Blues",
      symptom == "eating" ~ "Eating Problems",
      symptom == "energy" ~ "Energy Problems",
      symptom == "self" ~ "Self Blame Problems",
      symptom == "sleep" ~ "Sleep Problems",
      symptom == "concentration" ~ "Concentration Problems",
      symptom == "moving" ~ "Moving Problems",
      TRUE ~ NA
    ),
    symptom_labels = factor(symptom_labels, levels = c("Anhedonia", "Blues", "Eating Problems", "Energy Problems", "Self Blame Problems", "Sleep Problems", "Concentration Problems", "Moving Problems")),
    severity = factor(severity, levels = c("Always", "Mostly", "Some", "No")),  # Order severity levels
    fill_value = interaction(symptom_labels, severity)  # Combine symptom and severity for easier plotting
  ) %>%
  arrange(symptom_labels, severity)  # Sort data for plotting

# Waffle chart
# Define severity levels in order from light to dark
severity_levels <- c("Always", "Mostly", "Some", "No")  # Define the severity levels

# Generate base colors for each symptom
symptom_labels <- unique(prop_table_summary$symptom_labels)  # Extract unique symptom labels from prop_table_summary\ n
n_symptoms <- length(symptom_labels)  # Get the number of unique symptoms
base_colors <- hue_pal()(n_symptoms)  # Generates distinct base colors for each symptom

# Generate the color mapping
color_mapping <- c()

for (i in seq_along(symptom_labels)) {  # Loop through each symptom label
  symptom <- symptom_labels[i]  # Get the current symptom label
  symptom_color <- base_colors[i]  # Get the base color for the current symptom
  
  # Generate a gradient of colors for severity levels from dark (Always) to light (Some)
  severity_colors <- scales::seq_gradient_pal(symptom_color, "white")(seq(0, 0.75, length.out = length(severity_levels) - 1))
  
  # Set "NO" severity to be explicitly white
  severity_colors <- c(severity_colors, "white")
  
  # Assign names to severity_colors, ensuring they follow the levels from Always to No
  names(severity_colors) <- paste(symptom, severity_levels, sep = ".")
  
  # Append to the color mapping
  color_mapping <- c(color_mapping, severity_colors)
}

# Generate the waffle plot to visualize the data by subpopulation and severity levels
wafflePlot <- ggplot(
  prop_table_summary,
  aes(fill = fct_rev(fill_value), values = n_count)  # Use fill_value to represent the fill color, reverse factor levels
) +
  waffle::geom_waffle(
    n_rows = 10,         # Number of squares in each row of the waffle plot
    color = "gray",      # Border color of each square
    flip = TRUE,         # Disable flipping to change stacking order to bottom-to-top
    na.rm = TRUE         # Handle NA values if present
  ) +
  facet_grid(
    subpopulation ~ symptom_labels,  # Create a grid layout of subpopulation by symptom labels
    switch = "both",
    labeller = labeller(
      subpopulation = label_wrap_gen(width = 10),
      symptom_labels = label_wrap_gen(width = 10)
    )
  ) +
  coord_equal() +  # Ensure that the waffle tiles are square
  theme_enhance_waffle() +  # Enhance theme specifically for waffle plots
  scale_fill_manual(
    values = color_mapping                # Use custom color mapping to reflect severity order
  ) +
  labs(
    title = "Waffle Plot by Subpopulation and Severity",  # Title of the plot
    fill = "Severity"  # Legend title for the color scale
  ) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "none",                       # Hide legend
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white", color = "white"),
    strip.background.y = element_rect(fill = "white", color = "white"),
    strip.text.x = element_text(color = "black", size = 9),
    strip.text.y.left = element_text(color = "black", size = 9, angle = 0, hjust = 1)  # Adjust strip text rotation
  )

# Custom legend
# Define custom colors for severity levels in black and white
bw_colors <- c(
  "Always" = "#000000",  # Black
  "Mostly" = "#555555",  # Dark Gray
  "Some" = "#AAAAAA",    # Light Gray
  "No" = "#FFFFFF"       # White
)

# Create a simplified data frame for the custom legend
legend_data <- data.frame(
  severity = factor(severity_levels, levels = severity_levels),
  fill_value = severity_levels,  # Using severity levels directly for the legend
  n_count = 1  # Dummy values for counts to create legend only
)

# Create the custom legend plot
legend_plot <- ggplot(
  legend_data,
  aes(fill = severity, values = n_count)
) +
  waffle::geom_waffle(
    n_rows = 1,         # One row, since we're only creating a legend
    color = "gray",     # Border color
    flip = TRUE
  ) +
  scale_fill_manual(
    values = bw_colors,   # Use black and white colors for the legend
    guide = guide_legend(title = "Severity", direction = "horizontal")
  ) +
  theme_void()  # Simplify theme for the legend

# Extract the legend using cowplot::get_legend
legend <- cowplot::get_legend(legend_plot)  # Extract legend from the custom legend plot

# Combine waffle plot with legend, stacking vertically
combined_plot <- plot_grid(wafflePlot, legend, ncol = 1, rel_heights = c(4, 0.3))  # Combine main plot and legend, adjust heights

# Print the combined plot
print(combined_plot)  # Display the combined waffle plot

# Generate the waffle plot with all depressed individuals on denominator
wafflePlot <- prop_table_summary_overall %>% 
  drop_na(subpopulation) %>%  # Remove rows with NA in subpopulation
  ggplot(
    aes(fill = fct_rev(fill_value), values = n_count)  # Use fill_value for fill color, reverse factor levels
  ) +
  waffle::geom_waffle(
    n_rows = 10,         # Number of squares in each row of the waffle plot
    color = "gray",      # Border color of each square
    flip = TRUE,         # Disable flipping to change stacking order to bottom-to-top
    na.rm = TRUE         # Handle NA values if present
  ) +
  facet_grid(
    subpopulation ~ symptom_labels,  # Create a grid layout of subpopulation by symptom labels
    switch = "both",
    labeller = labeller(
      subpopulation = label_wrap_gen(width = 20),
      symptom_labels = label_wrap_gen(width = 10)
    )
  ) +
  coord_equal(clip = "off") +  # Ensure that waffle tiles are square, disable clipping
  theme_enhance_waffle() +  # Enhance theme specifically for waffle plots
  scale_fill_manual(
    values = color_mapping                # Use custom color mapping to reflect severity order
  ) +
  labs(
    title = "Waffle Plot by Subpopulation and Severity",  # Title of the plot
    fill = "Severity"  # Legend title for the color scale
  ) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "none",                       # Hide legend
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white", color = "white"),
    strip.background.y = element_rect(fill = "white", color = "white"),
    strip.text.x = element_text(color = "black", size = 9),
    strip.text.y.left = element_text(color = "black", size = 8, angle = 0, hjust = 1, margin = margin(r = 0, l = 0)),  # Adjust strip text rotation
    strip.placement = "outside"                     # Place strips outside of panels
  ) 

# Custom legend
# Define custom colors for severity levels in black and white
bw_colors <- c(
  "Always" = "#000000",  # Black
  "Mostly" = "#555555",  # Dark Gray
  "Some" = "#AAAAAA",    # Light Gray
  "No" = "#FFFFFF"       # White
)

# Create a simplified data frame for the custom legend
legend_data <- data.frame(
  severity = factor(severity_levels, levels = severity_levels),
  fill_value = severity_levels,  # Using severity levels directly for the legend
  n_count = 1  # Dummy values for counts to create legend only
)

# Create the custom legend plot
legend_plot <- ggplot(
  legend_data,
  aes(fill = severity, values = n_count)
) +
  waffle::geom_waffle(
    n_rows = 1,         # One row, since we're only creating a legend
    color = "gray",     # Border color
    flip = TRUE
  ) +
  scale_fill_manual(
    values = bw_colors,   # Use black and white colors for the legend
    guide = guide_legend(title = "Severity", direction = "horizontal")
  ) +
  theme_void()  # Simplify theme for the legend

# Extract the legend using cowplot::get_legend
legend <- cowplot::get_legend(legend_plot)  # Extract legend from the custom legend plot

# Combine waffle plot with legend, adjusting space between them
combined_plot <- plot_grid(
  wafflePlot, 
  legend, 
  nrow = 2, 
  rel_heights = c(1, 0.1),  # Reduce height of the legend to minimize space
  align = "v",               # Align vertically
  axis = "tb"                # Align top and bottom axis
)

# Print the combined plot
print(combined_plot)  # Display the combined waffle plot

# Import required libraries
# This code relies on libraries such as dplyr, ggplot2, and gt.

# Statistics: 
## CMH Test
# Prepare data for the Cochran-Mantel-Haenszel (CMH) Test
cmh_data <- prop_table %>%
  mutate(
    # Convert 'severity' to a factor with levels specified in a particular order
    severity = factor(severity, levels = c("No", "Some", "Mostly", "Always")),
    # Convert 'subpopulation' and 'symptom' to factors for further analysis
    subpopulation = as.factor(subpopulation),
    symptom = as.factor(symptom)
  )

# Create a contingency table for the CMH Test
# Use the xtabs function to create a contingency table of counts (n_count) categorized by subpopulation, severity, and symptom
cmh_table <- xtabs(n_count ~ subpopulation + severity + symptom, data = cmh_data)

# Perform the Cochran-Mantel-Haenszel (CMH) Test
# This test is used to determine if there is an association between variables, controlling for other factors
cmh_result <- mantelhaen.test(cmh_table)

# Display the CMH test result
print(cmh_result)

## Pairwise Chi-Square Tests with FDR Correction
# Extract unique levels of 'subpopulation'
unique_subpopulations <- levels(cmh_data$subpopulation)

# Create an empty list to store p-values obtained from pairwise comparisons
p_values <- c()

# Loop over all unique pairs of subpopulations to perform pairwise comparisons
for (i in 1:(length(unique_subpopulations) - 1)) {
  for (j in (i + 1):length(unique_subpopulations)) {
    # Subset data for the two subpopulations being compared
    sub_data <- cmh_data %>% 
      filter(subpopulation %in% c(unique_subpopulations[i], unique_subpopulations[j]))
    
    # Create a contingency table for severity vs subpopulation for this pair
    pairwise_table <- xtabs(n_count ~ subpopulation + severity, data = sub_data)
    
    # Perform Fisher's exact test on the pairwise contingency table
    fisher_result <- fisher.test(pairwise_table)
    
    # Store the p-value from Fisher's test
    p_values <- c(p_values, fisher_result$p.value)
  }
}

# Display the raw p-values obtained from pairwise Fisher tests
print(p_values)

# Adjust the p-values using the False Discovery Rate (FDR) correction
# FDR correction is used to control the Type I error rate when multiple comparisons are made
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Display the adjusted p-values
print(p_values_fdr)

# Create a combined table with subpopulation names and corresponding adjusted p-values
rbind(unique_subpopulations, p_values_fdr)

## Heatmap
# Create a summary table with proportions for heatmap visualization
heatmap_data <- cmh_data %>%
  group_by(subpopulation, symptom_labels, severity) %>%
  # Sum counts by subpopulation, symptom, and severity
  summarise(n_count = sum(n_count)) %>%
  ungroup() %>%
  group_by(subpopulation, symptom_labels) %>%
  # Calculate the proportion of each severity level within each subpopulation and symptom combination
  mutate(proportion = n_count / sum(n_count)) %>%
  ungroup()

# Create the heatmap visualization
# The heatmap visualizes the proportion of different severity levels by subpopulation and symptom
# ggplot2 is used for visualization
heatmap_plot <- ggplot(heatmap_data, aes(x = subpopulation, y = fct_rev(symptom_labels), fill = proportion)) +
  geom_tile(color = "white") +
  facet_wrap(~ severity, ncol = 2) +
  scale_fill_gradient(low = "white", high = "red", name = "Proportion") +
  labs(
    title = "Heatmap of Proportions by Subpopulation, Symptom, and Severity",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.key.height = unit(10, "lines"),
    legend.ticks = element_line(color = "gray15", linewidth = 0.5),
    legend.ticks.length = unit(0.5, "lines")))) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Display the heatmap
print(heatmap_plot)

# Create a table version of the heatmap data for better interpretability
# Arrange the data by subpopulation, symptom, and severity
heatmap_data_table <- heatmap_data %>%
  arrange(subpopulation, symptom_labels, severity)

# Create a wide-format table suitable for display using gt
heatmap_data_wide <- heatmap_data %>%
  pivot_wider(
    names_from = severity,
    values_from = c(n_count, proportion),
    names_glue = "{severity}_{.value}"
  )

# Create the GT table with a heatmap color scheme and grouped by subpopulation
heatmap_gt_table_wide <- heatmap_data_wide %>%
  group_by(subpopulation) %>% 
  gt() %>%
  tab_header(
    title = "Subpopulation, Symptom, and Severity with N and Proportion"
  ) %>%
  cols_label(
    subpopulation = "Subpopulation",
    symptom_labels = "Symptom",
    No_n_count = "N",
    No_proportion = "Proportion",
    Some_n_count = "N",
    Some_proportion = "Proportion",
    Mostly_n_count = "N",
    Mostly_proportion = "Proportion",
    Always_n_count = "N",
    Always_proportion = "Proportion"
  ) %>%
  tab_spanner(
    label = "No",
    columns = c(No_n_count, No_proportion)
  ) %>%
  tab_spanner(
    label = "Some",
    columns = c(Some_n_count, Some_proportion)
  ) %>%
  tab_spanner(
    label = "Mostly",
    columns = c(Mostly_n_count, Mostly_proportion)
  ) %>%
  tab_spanner(
    label = "Always",
    columns = c(Always_n_count, Always_proportion)
  ) %>%
  fmt_number(
    columns = matches("_proportion"),
    decimals = 3
  ) %>%
  data_color(
    columns = matches("_proportion"),
    fn = scales::col_numeric(
      palette = c("white", "red"),
      domain = c(0, 1)
    )
  ) %>%
  tab_options(
    table.font.size = "small",
    table.align = "center"
  )

# Print the GT table with heatmap color scheme
print(heatmap_gt_table_wide)

# Save the GT table as an image (e.g., PNG format)
gtsave(heatmap_gt_table_wide, filename = "analysis/figures/heatmap_gt_table.png")
