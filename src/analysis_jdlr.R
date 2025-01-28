library(tidyverse)
library(here)
data <- read_csv(here("analysis", "data", "rawData", "NHIS_Adult2019_20241127.csv"))


# Recode variables for mental health symptoms
data$GADCAT_A <- ifelse(data$GADCAT_A == 8, NA, data$GADCAT_A)
data$anxiety <- ifelse(data$GADCAT_A %in% c(3, 4), 1, ifelse(data$GADCAT_A %in% c(1, 2), 0, NA))
data$depression <- ifelse(data$PHQCAT_A %in% c(3, 4), 1, ifelse(data$PHQCAT_A %in% c(1, 2), 0, NA))

# Binary variables for PHQ symptoms
data$PHQ81_A <- ifelse(data$PHQ81_A %in% c(7, 8, 9), NA, data$PHQ81_A)
data$PHQ82_A <- ifelse(data$PHQ82_A %in% c(7, 8, 9), NA, data$PHQ82_A)
data$PHQ83_A <- ifelse(data$PHQ83_A %in% c(7, 8, 9), NA, data$PHQ83_A)
data$PHQ84_A <- ifelse(data$PHQ84_A %in% c(7, 8, 9), NA, data$PHQ84_A)
data$PHQ85_A <- ifelse(data$PHQ85_A %in% c(7, 8, 9), NA, data$PHQ85_A)
data$PHQ86_A <- ifelse(data$PHQ86_A %in% c(7, 8, 9), NA, data$PHQ86_A)
data$PHQ87_A <- ifelse(data$PHQ87_A %in% c(7, 8, 9), NA, data$PHQ87_A)
data$PHQ88_A <- ifelse(data$PHQ88_A %in% c(7, 8, 9), NA, data$PHQ88_A)

symptom_vars <- c("PHQ81_A", "PHQ82_A", "PHQ83_A", "PHQ84_A", "PHQ85_A", "PHQ86_A", "PHQ87_A", "PHQ88_A")
symptom_names <- c("anhedonia", "sadness", "sleep", "energy", "appetite", "guilt", "concentration", "psychomotor")

for (i in seq_along(symptom_vars)) {
  data[[symptom_names[i]]] <- ifelse(is.na(data[[symptom_vars[i]]]), NA, data[[symptom_vars[i]]] - 1)
}

# Verify the adjustment
summary(data[symptom_names])


# Filter out rows with NA in any of the symptoms columns
data <- data[complete.cases(data[symptom_names]), ]

# Binary variables for GAD symptoms
data$GAD71_A <- ifelse(data$GAD71_A %in% c(7, 8, 9), NA, data$GAD71_A)
data$GAD72_A <- ifelse(data$GAD72_A %in% c(7, 8, 9), NA, data$GAD72_A)
data$GAD73_A <- ifelse(data$GAD73_A %in% c(7, 8, 9), NA, data$GAD73_A)
data$GAD74_A <- ifelse(data$GAD74_A %in% c(7, 8, 9), NA, data$GAD74_A)
data$GAD75_A <- ifelse(data$GAD75_A %in% c(7, 8, 9), NA, data$GAD75_A)
data$GAD76_A <- ifelse(data$GAD76_A %in% c(7, 8, 9), NA, data$GAD76_A)
data$GAD77_A <- ifelse(data$GAD77_A %in% c(7, 8, 9), NA, data$GAD77_A)


# Compute PHQ8_score
data$PHQ8_count <- rowSums(data[symptom_names], na.rm = TRUE)

# Create frequency tables for variables
# You can use the table() function for frequencies
frequencies <- function(x) {
  table(x, useNA = "ifany")
}


# Example: frequencies for depression and anxiety
frequencies(data$depression)
frequencies(data$PHQ8_count)
frequencies(data$anxiety)



#ridgeline plot
# library
library(ggridges)
library(ggplot2)


# basic example
ggplot(data, aes(x = PHQ8_count, y = Chronicpain_any, fill = anhedonia)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")




