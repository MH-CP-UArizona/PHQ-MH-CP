####################################################################################################################
## Author: Greg Chism + Jennifer De La Rosa
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
  ggridges,         # Ridgeline plots
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
data$subpopulation <- with(data, factor(
  ifelse(depression == 1 & LowerImpactCP_largedenom == 1, "Depression and Low Impact CP",
         ifelse(depression == 1 & HighImpactCP_largedenom == 1 & LowerImpactCP_largedenom == 0, "Depression and High Impact CP",
                ifelse(depression == 1 & ChronicPain_any == 0, "Depression No CP",
                       ifelse(depression == 0 & LowerImpactCP_largedenom == 1, "No Depression and Low Impact CP",
                              ifelse(depression == 0 & HighImpactCP_largedenom == 1 & LowerImpactCP_largedenom == 0, "No Depression and High Impact CP",
                                     ifelse(depression == 0 & ChronicPain_any == 0, "No Depression and No CP", NA
                                     )))))))
)

# Filter rows where depression == 1 and subpopulation is not NA
df <- subset(data, !is.na(subpopulation))

## Data wrangling for plotting / analysis
# Define the symptoms
symptoms <- c("NOanxiety", "NOdepression", "NOanhedonia", "NOsadness", "NOsleep", "NOenergy", "NOappetite", "NOguilt", "NOconcentration", "NOpsychomotor",
              "SOMEanxiety", "SOMEdepression", "SOMEanhedonia", "SOMEsadness", "SOMEsleep", "SOMEenergy", "SOMEappetite", "SOMEguilt", "SOMEconcentration", "SOMEpsychomotor",
              "MOSTLYanxiety", "MOSTLYdepression", "MOSTLYanhedonia", "MOSTLYsadness", "MOSTLYsleep", "MOSTLYenergy", "MOSTLYappetite", "MOSTLYguilt", "MOSTLYconcentration", "MOSTLYpsychomotor",
              "ALWAYSanxiety", "ALWAYSdepression", "ALWAYSanhedonia", "ALWAYSsadness", "ALWAYSsleep", "ALWAYSenergy", "ALWAYSappetite", "ALWAYSguilt", "ALWAYSconcentration", "ALWAYSpsychomotor")

# Extract symptom columns dynamically
symptom_cols <- grep(paste(symptoms, collapse = "|"), colnames(df), value = TRUE)

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
long_results <- long_results %>%
  mutate(
    symptom_labels = case_when(
      symptom == "anhedonia" ~ "PHQ-1\nAnhedonia",
      symptom == "sadness" ~ "PHQ-2\nSadness/Blues",
      symptom == "energy" ~ "PHQ-3\nFatigue/Energy",
      symptom == "sleep" ~ "PHQ-4\nSleep",
      symptom == "appetite" ~ "PHQ-5\nAppetite",
      symptom == "guilt" ~ "PHQ-6\nSelf-Blame/Guilt",
      symptom == "concentration" ~ "PHQ-7\nConcentration",
      symptom == "psychomotor" ~ "PHQ-8\nPsychomotor"
    ),
    symptom_labels = factor(symptom_labels, levels = c(
      "PHQ-1\nAnhedonia", "PHQ-2\nSadness/Blues", "PHQ-3\nFatigue/Energy", "PHQ-4\nSleep",
      "PHQ-5\nAppetite", "PHQ-6\nSelf-Blame/Guilt", "PHQ-7\nConcentration", "PHQ-8\nPsychomotor"
    ))
  )

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

wafflePlot <- 
  long_results %>%
  filter(grepl("^Depression", subpopulation)) |>
  ggplot(aes(fill = fill_value, values = n_count)
) +
  waffle::geom_waffle(
    n_rows = 10,
    color = "gray",
    flip = TRUE,
    na.rm = TRUE
  ) +
  facet_grid(
    symptom_labels ~ subpopulation, 
    labeller = labeller(
      subpopulation = as_labeller(
        c(
          "Depression and High Impact CP" = "HICP\n(n=848)",
          "Depression and Low Impact CP" = "LICP\n(n=466)",
          "Depression No CP" = "No CP\n(n=839)"
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
    title = "Positive Depression Screen (n=2153)",
    fill = "Severity"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    
    strip.text = element_text(size = 10),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white", color = "white"),
    strip.background.y = element_rect(fill = "white", color = "white"),
    strip.text.x = element_text(hjust = 0.5, color = "black", size = 10),
    strip.text.y.right = element_text(color = "black", size = 10),
    strip.placement = "outside",
    strip.placement.x = "top",
    strip.placement.y = "left"
  )

# PNG
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_depression.png",
  plot = wafflePlot,
  width = 4, height = 10,
  dpi = 400
)

# PDF
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_depression.pdf",
  plot = wafflePlot,
  width = 4, height = 10,
  device = cairo_pdf  # better text rendering
)

# SVG
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_depression.svg",
  plot = wafflePlot,
  width = 4, height = 10,
  device = "svg"
)

wafflePlotNoDP <- 
  long_results %>%
  filter(grepl("^No Depression", subpopulation)) %>% 
  ggplot(aes(fill = fill_value, values = n_count)
  ) +
  waffle::geom_waffle(
    n_rows = 10,
    color = "gray",
    flip = TRUE,
    na.rm = TRUE
  ) +
  facet_grid(
    symptom_labels ~ subpopulation, 
    labeller = labeller(
      subpopulation = as_labeller(
        c(
          "No Depression and High Impact CP" = "HICP\n(n=1763)",
          "No Depression and Low Impact CP" = "LICP\n(n=3981)",
          "No Depression and No CP" = "No CP\n(n=23086)"
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
    title = "Negative Depression Screen (n=28830)",
    fill = "Severity"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    strip.text = element_text(size = 10),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white", color = "white"),
    strip.background.y = element_rect(fill = "white", color = "white"),
    strip.text.x = element_text(hjust = 0.5, color = "black", size = 10),
    strip.text.y.right = element_text(color = "black", size = 10),
    strip.placement = "outside",
    strip.placement.x = "top",
    strip.placement.y = "left"
  )

#PNG
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_Nodepression.png",
  plot = wafflePlotNoDP,
  width = 4, height = 10,  # Adjust dimensions as needed
  dpi = 400
)

# PDF
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_Nodepression.pdf",
  plot = wafflePlotNoDP,
  width = 4, height = 10,
  device = cairo_pdf  # better text rendering
)

# SVG
ggsave(
  filename = "analysis/figures/fig1_waffle_plot_Nodepression.svg",
  plot = wafflePlotNoDP,
  width = 4, height = 10,
  device = "svg"
)

###########################FIGURE 1 ################################
# Author Jennifer De La Rosa
####both density plots need numeric PHQ-8 and factor Chronic Pain Ordinal
##############################################################
data$ChronicPainOrdinal <- as.factor(data$ChronicPainOrdinal)
depressed$ChronicPainOrdinal <- as.factor(depressed$ChronicPainOrdinal)
not_depressed$ChronicPainOrdinal <- as.factor(not_depressed$ChronicPainOrdinal)

data$PHQ8_score <- as.double(data$PHQ8_score)
not_depressed$PHQ8_score <- as.double(not_depressed$PHQ8_score)
depressed$PHQ8_score <- as.double(depressed$PHQ8_score)
####DENSITY PLOTS - Panel B
####Distribution of PHQ-8 scores among those who screened positive, grouped by CP
###########################################################################################
density_depressed  <-
  ggplot(data=depressed, aes(x=PHQ8_score, y=ChronicPainOrdinal, fill = ChronicPainOrdinal)) +
  geom_density_ridges(
    aes(point_shape = ChronicPainOrdinal, point_fill = ChronicPainOrdinal),
    alpha=.7, scale =.8) +
  
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(0,24), expand = c(0,0), breaks = seq(from=0, to=24, by=1)) +
  scale_fill_paletteer_d("fishualize::Semicossyphus_pulcher", direction = -1)+
  theme(legend.position = "none")+
  geom_boxplot(width = .1, alpha = 1.0, lwd=.65, outlier.shape=NA)
density_depressed
###########################FIGURE 1 ########################
###################################################################
####DENSITY PLOTS Panel D - distribution of PHQ-8 scores  for people who screened negative, grouped by CP 
###################################################################
density_not_depressed  <-
  ggplot(data=not_depressed, aes(x=PHQ8_score, y=ChronicPainOrdinal, fill = ChronicPainOrdinal)) +
  geom_density_ridges(
    aes(point_shape = ChronicPainOrdinal, point_fill = ChronicPainOrdinal),
    alpha=.7, scale =.8) +
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(0,24), expand = c(0,0), breaks = seq(from=0, to=24, by=1)) +
  scale_fill_paletteer_d("fishualize::Semicossyphus_pulcher", direction = -1)+
  theme(legend.position = "none")+
  geom_boxplot(width = .1, alpha = 1.0, lwd=.65, outlier.shape=NA)
density_not_depressed

###########################FIGURE 2 ########################
#### RIDGELINE PLOTS - Panel A
p1<- 
  ggplot(data, aes(x = somaticsymptoms_proportion, y = PHQ8_score,fill = after_stat(x))) +
  geom_density_ridges_gradient()+ facet_wrap(~ChronicPainOrdinal)+
  
  scale_fill_viridis_c(option = "E", expand = c(0,0),limits=c(0,1))+   
  coord_cartesian (clip = "off") +
  theme(panel.spacing = unit(.5,"cm",data = NULL), legend.direction = "horizontal", legend.position = "bottom")+
  xlim(0,1)

p1 +
  xlab("") + 
  ylab("PHQ-8 Score") 


############################FIGURE 2 ########################
#### SCATTERPLOTS - Panel A

p2<-
  ggplot(data, aes(x = ChronicPainOrdinal, y = PHQ8_score, color=somaticsymptoms_proportion)) +
  geom_jitter(alpha=8/20, size=.8) +
  scale_color_viridis_c( option = "E", limits=c(0,1),direction =1, breaks = seq(from=0, to=1, by=.25))
p2 + 
  theme(panel.spacing = unit(.5,"cm",data = NULL), legend.direction = "horizontal", legend.position = "bottom", axis.ticks = element_blank())+
  xlab("") + 
  ylab("PHQ-8 Score")

# PNG
ggsave(
  filename = "analysis/figures/fig1_density_plot_depression.png",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  dpi = 400
)

# PDF
ggsave(
  filename = "analysis/figures/fig1_density_plot_depression.pdf",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  device = cairo_pdf  # better text rendering
)

# SVG
ggsave(
  filename = "analysis/figures/fig1_density_plot_depression.svg",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  device = "svg"
)

ggsave(
  filename = "analysis/figures/fig1_density_plot_Nodepression.png",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  dpi = 400
)

# PDF
ggsave(
  filename = "analysis/figures/fig1_density_plot_Nodepression.pdf",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  device = cairo_pdf  # better text rendering
)

# SVG
ggsave(
  filename = "analysis/figures/fig1_density_plot_Nodepression.svg",
  plot = last_plot(),
  width = 4, height = 6,  # Adjust dimensions as needed
  device = "svg"
)

