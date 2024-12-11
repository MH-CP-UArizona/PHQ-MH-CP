####################################################################################################################
## Author: Greg Chism
## Date: December 2024
## email: gchism@arizona.edu
## Project: MH & CP MS3 Depression Symptoms
## Title: Reproducing SPSS encoding by Jenn De La Rosa into R
####################################################################################################################

library(tidyverse)
library(here)

# Read in raw data
data <- read_csv(here("analysis", "data", "rawData", "NHIS_Adult2019_20241127.csv"))

# Define a function to recode variables
recode <- function(x, old, new, na.rm = FALSE) {
  if (na.rm) {
    x[is.na(x)] <- NA
  }
  recoded <- x
  for (i in seq_along(old)) {
    recoded[x == old[i]] <- new[i]
  }
  recoded
}

# Define a function to recode variables
recode <- function(x, old, new, na.rm = FALSE) {
  if (na.rm) {
    x[is.na(x)] <- NA
  }
  recoded <- x
  for (i in seq_along(old)) {
    recoded[x == old[i]] <- new[i]
  }
  recoded
}

# Define a function to calculate frequencies
frequencies <- function(var) {
  table(var, useNA = "ifany")
}

# Label variables using attributes
label_variable <- function(var, label) {
  attr(var, "label") <- label
}

# Recoding anxiety and depression variables
data$anxiety <- recode(data$GADCAT_A, old = c(3, 4, 1, 2), new = c(1, 1, 0, 0), na.rm = TRUE)
data$depression <- recode(data$PHQCAT_A, old = c(3, 4, 1, 2), new = c(1, 1, 0, 0), na.rm = TRUE)
