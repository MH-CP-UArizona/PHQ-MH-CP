####################################################################################################################
## Author: Greg Chism
## Date: November 2024
## email: gchism@arizona.edu
## Project: MH & CP MS3 Depression Symptoms
## Title: Descriptive analyses
####################################################################################################################

# Required Libraries Test
if(!require(pacman))  # Check if the `pacman` package is installed
  install.packages("pacman")  # If not installed, install `pacman` package

# Use `pacman` to load required libraries, installing any missing ones automatically
pacman::p_load(
  cowplot,          # Plot arrangement and combining
  ggthemes,         # Additional themes for ggplot2
  ggpubr,           # Publication-ready ggplot2 visualizations
  gt,               # Create tables with advanced formatting
  here,             # Manage file paths in a project
  patchwork,        # Arrange ggplots into layouts
  psych,            # Various tools for psychological research
  rcompanion,       # Helper functions for data analysis
  scales,           # Scale functions for ggplot2
  survey,           # Analyze survey data
  tidymodels,       # Collection of packages for modeling and machine learning
  tidyverse,        # Collection of R packages for data science (ggplot2, dplyr, etc.)
  waffle,           # Create waffle charts
  webshot           # Take web screenshots
)

# Load additional GitHub package for waffle plots
pacman::p_load_gh("liamgilbey/ggwaffle")

# Read in and glimpse data
data <- read_csv(here("analysis", "data", "derivedData", "GC_20241021_NHISadult2019_data_newvars.csv"))  # Load data from CSV file located in "data" folder using `here()`

# Generating frequency tables for selected columns (Exploratory data analysis)
## Definition of depression (N = 2196 or `2188`, either based on `depression` variable or `mh_categorical`)
## High impact pain (N = 2666)
## ChronicPain_any (N = 7184)
## Lower impact pain (N = 4508)

severity_table <- data %>%
  select(depression) %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))

# Rename the columns for clarity
colnames(severity_table) <- c("Severity Level", "Frequency")

# Print the table
print(severity_table)

# Mutually Exclusive Subpopulations: Create categorical variables for specific subpopulations based on conditions
data$subpopulation <- with(data, factor(ifelse(
  LowerImpactCP_largedenom == 1 & depression == 1, "Depression and Low Impact CP",
  ifelse(ChronicPain_any == 0 & depression == 1, "Depression No CP",
         ifelse(HighImpactCP_largedenom == 1 & LowerImpactCP_largedenom == 0 & depression == 1, "Depression and High Impact CP", NA)
         )
  )
))

# Filter rows where depression == 1 and subpopulation is not NA
df <- subset(data, depression == 1 & !is.na(subpopulation))

# Box Plot Figure
# Visualize PHQ Scores by Subpopulation using a box plot
fig1 <- df %>%
  mutate(subpopulation = fct_relevel(subpopulation, c("Depression No CP", "Depression and Low Impact CP", "Depression and High Impact CP"))) %>%   # Reorder levels of subpopulation factor
  drop_na(subpopulation) %>%  # Drop rows where subpopulation is NA
  ggplot(aes(x = PHQ8_count, y = subpopulation, fill = subpopulation)) +
  geom_boxplot(width = 0.5) +  # Create a box plot
  scale_fill_colorblind() +  # Apply colorblind-friendly colors
  labs(title = "PHQ Scores by Subpopulation", x = "PHQ Scores", y = NULL) +  # Set axis labels and title
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # Hide legend

print(fig1)

### TO DO Add pairwise testing (Dunn's test w/ post hoc)

## Data wrangling for plotting / analysis
# Define the symptoms
symptoms <- c("NOanxiety", "NOdepression", "NOanhedonia", "NOsadness", "NOsleep", "NOenergy", "NOappetite", "NOguilt", "NOconcentration", "NOpsychomotor",
              "SOMEanxiety", "SOMEdepression", "SOMEanhedonia", "SOMEsadness", "SOMEsleep", "SOMEenergy", "SOMEappetite", "SOMEguilt", "SOMEconcentration", "SOMEpsychomotor",
              "MOSTLYanxiety", "MOSTLYdepression", "MOSTLYanhedonia", "MOSTLYsadness", "MOSTLYsleep", "MOSTLYenergy", "MOSTLYappetite", "MOSTLYguilt", "MOSTLYconcentration", "MOSTLYpsychomotor",
              "ALWAYSanxiety", "ALWAYSdepression", "ALWAYSanhedonia", "ALWAYSsadness", "ALWAYSsleep", "ALWAYSenergy", "ALWAYSappetite", "ALWAYSguilt", "ALWAYSconcentration", "ALWAYSpsychomotor")

# Extract symptom columns dynamically
symptom_cols <- grep(paste(symptoms, collapse = "|"), colnames(df), value = TRUE)
colnames(df)
# Initialize a list to store results
result_list <- list()

# Iterate through each subpopulation
for (subpop in unique(df$subpopulation)) {
  # Subset the data for the current subpopulation
  subpop_data <- df[df$subpopulation == subpop, symptom_cols, drop = FALSE]
  
  # Initialize a data frame to store results for the current subpopulation
  subpop_results <- data.frame(
    symptom = character(),
    total = numeric(),
    always = numeric(),
    always_prop = numeric(),
    always_n_count = numeric(),
    mostly = numeric(),
    mostly_prop = numeric(),
    mostly_n_count = numeric(),
    some = numeric(),
    some_prop = numeric(),
    some_n_count = numeric(),
    no_count = numeric(),
    no_prop = numeric(),
    no_n_count = numeric(),
    did_not_answer = numeric(),
    did_not_answer_prop = numeric(),
    did_not_answer_n_count = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract unique symptom names (without the prefixes)
  unique_symptoms <- unique(gsub("^(NO|SOME|MOSTLY|ALWAYS)", "", colnames(subpop_data)))
  
  for (symptom in unique_symptoms) {
    # Safely calculate the totals for each category
    total <- nrow(subpop_data)
    always <- if (paste0("ALWAYS", symptom) %in% colnames(subpop_data)) {
      sum(subpop_data[[paste0("ALWAYS", symptom)]] == 1, na.rm = TRUE)
    } else 0
    
    mostly <- if (paste0("MOSTLY", symptom) %in% colnames(subpop_data)) {
      sum(subpop_data[[paste0("MOSTLY", symptom)]] == 1, na.rm = TRUE)
    } else 0
    
    some <- if (paste0("SOME", symptom) %in% colnames(subpop_data)) {
      sum(subpop_data[[paste0("SOME", symptom)]] == 1, na.rm = TRUE)
    } else 0
    
    # Calculate the NO category
    no_count <- total - (always + mostly + some)
    
    # Calculate the "Did Not Answer" category
    did_not_answer <- sum(is.na(subpop_data[paste0(c("ALWAYS", "MOSTLY", "SOME"), symptom)]), na.rm = TRUE)
    
    # Calculate proportions and n_count for each category
    always_prop <- always / total
    mostly_prop <- mostly / total
    some_prop <- some / total
    no_prop <- no_count / total
    did_not_answer_prop <- did_not_answer / total
    
    always_n_count <- round(always_prop * 100)
    mostly_n_count <- round(mostly_prop * 100)
    some_n_count <- round(some_prop * 100)
    no_n_count <- round(no_prop * 100)
    did_not_answer_n_count <- round(did_not_answer_prop * 100)
    
    # Create a temporary data frame for this symptom
    temp_result <- data.frame(
      symptom = symptom,
      total = total,
      always = always,
      always_prop = always_prop,
      always_n_count = always_n_count,
      mostly = mostly,
      mostly_prop = mostly_prop,
      mostly_n_count = mostly_n_count,
      some = some,
      some_prop = some_prop,
      some_n_count = some_n_count,
      no_count = no_count,
      no_prop = no_prop,
      no_n_count = no_n_count,
      did_not_answer = did_not_answer,
      did_not_answer_prop = did_not_answer_prop,
      did_not_answer_n_count = did_not_answer_n_count,
      stringsAsFactors = FALSE
    )
    
    # Ensure consistent column structure before binding
    subpop_results <- rbind(subpop_results, temp_result)
  }
  
  # Add results for this subpopulation to the list
  result_list[[subpop]] <- subpop_results
}


# Combine all subpopulation results into a single data frame
final_results <- do.call(rbind, 
                         lapply(names(result_list), function(subpop) {
                           cbind(subpopulation = subpop, result_list[[subpop]])
                         }))

# Reset row names
rownames(final_results) <- NULL


# View the final results
print(final_results)


# Convert final_results to long form
long_results <- do.call(rbind, lapply(split(final_results, final_results$subpopulation), function(subpop_df) {
  # Pivot for each severity type
  severities <- c("Always", "Mostly", "Some", "No")
  symptom_names <- unique(subpop_df$symptom)
  
  # Create long-form rows
  long_rows <- do.call(rbind, lapply(symptom_names, function(symptom) {
    symptom_data <- subpop_df[subpop_df$symptom == symptom, ]
    rows <- data.frame(
      subpopulation = rep(symptom_data$subpopulation, 4),
      symptom = rep(symptom, 4),
      severity = severities,
      count = c(symptom_data$always, symptom_data$mostly, symptom_data$some, symptom_data$no_count),
      prop = c(symptom_data$always_prop, symptom_data$mostly_prop, symptom_data$some_prop, symptom_data$no_prop),
      n_count = c(symptom_data$always_n_count, symptom_data$mostly_n_count, symptom_data$some_n_count, symptom_data$no_n_count),
      sum = rep(symptom_data$total, 4)
    )
    return(rows)
  }))
  return(long_rows)
}))

# Reset row names
rownames(long_results) <- NULL

# Add symptom_labels using case_when
long_results$symptom_labels <- stringr::str_to_title(long_results$symptom)

# Order symptom_labels and severity
long_results$symptom_labels <- factor(long_results$symptom_labels, levels = c(
  "Anhedonia", "Sadness", "Appetite", "Energy",
  "Guilt", "Sleep", "Concentration", "Psychomotor"
))

long_results$severity <- factor(long_results$severity, levels = c("Always", "Mostly", "Some", "No"))  # Order severity levels

# Add fill_value by combining symptom_labels and severity
long_results$fill_value <- interaction(long_results$symptom_labels, long_results$severity)

# Sort the data by symptom_labels and severity
long_results <- long_results |> arrange(symptom_labels, severity)

# View the long-format data
print(long_results)
 

# Waffle chart
# Define severity levels in order from light to dark
severity_levels <- c("Always", "Mostly", "Some", "No")  # Define the severity levels

# Generate base colors for each symptom
symptom_labels <- unique(long_results$symptom_labels)  # Extract unique symptom labels from prop_table_summary\ n
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
df |> group_by(subpopulation) |> summarise(n = n())

wafflePlot <- ggplot(
  long_results,
  aes(fill = fill_value, values = n_count)
) +
  waffle::geom_waffle(
    n_rows = 10,
    color = "gray",
    flip = TRUE,
    na.rm = TRUE
  ) +
  facet_grid(
    subpopulation ~ symptom_labels,
    switch = "both",
    labeller = labeller(
      subpopulation = as_labeller(
        c(
          "Depression and High Impact CP" = "Depression and High Impact CP\n(n=848)",
          "Depression and Low Impact CP" = "Depression and Low Impact CP\n(n=466)",
          "Depression No CP" = "Depression No CP\n(n=839)"
        ),
        default = label_wrap_gen(width = 10)
      ),
      symptom_labels = label_wrap_gen(width = 10)
    )
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  scale_fill_manual(
    values = color_mapping
  ) +
  labs(
    title = "Waffle Plot by Subpopulation and Severity",
    fill = "Severity"
  ) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white", color = "white"),
    strip.background.y = element_rect(fill = "white", color = "white"),
    strip.text.x = element_text(color = "black", size = 8.4),
    strip.text.y.left = element_text(color = "black", size = 8.4, angle = 0, hjust = 1)
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


# Dumbbell plot of differences
dumbbell <- long_results |> 
  ggplot(aes(x = fct_rev(subpopulation), y =  prop, group = severity)) + 
  geom_line() +
  geom_point(size = 2, shape = 21, color = "black", aes(fill = interaction(symptom_labels, severity))) + 
  facet_wrap(~ symptom_labels) +
  labs(x = NULL, y = "Proportion") + 
  scale_fill_manual(
    values = color_mapping,    # Ensure that color_mapping aligns with severity levels
    guide = guide_legend(reverse = TRUE)  # Reverse legend order if desired
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))


legend_plot_point <- ggplot(
  legend_data,
  aes(x = severity, y = n_count, fill = severity)
) +
  geom_point(size = 3, shape = 21, color = "black") +  # Use points to represent each severity level
  scale_fill_manual(
    values = bw_colors,   # Use black and white colors for the legend
    guide = guide_legend(title = "Severity", direction = "horizontal")
  ) +
  theme_void()   # Simplify theme for the legend

# Extract the legend using cowplot::get_legend
legend <- cowplot::get_legend(legend_plot_point)  # Extract legend from the custom legend plot

# Combine waffle plot with legend, adjusting space between them
combined_plot <- plot_grid(
  legend,
  dumbbell, 
  nrow = 2, 
  rel_heights = c(0.1, 1),  # Reduce height of the legend to minimize space
  align = "v",               # Align vertically
  axis = "tb"                # Align top and bottom axis
)

print(combined_plot)

# Import required libraries
# This code relies on libraries such as dplyr, ggplot2, and gt.
# Statistics: 
## CMH Test
# Prepare data for the Cochran-Mantel-Haenszel (CMH) Test
cmh_data <- long_results %>%
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


#> Set analysis (case oriented)
#> Profiles like before
#> Mean severity (bar)
#> size possibly correlation to CP
#> severity of symptom is color gradient
