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
  data[[symptom_names[i]]] <- ifelse(is.na(data[[symptom_vars[i]]]), NA, data[[symptom_vars[i]]])
}

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

gad_vars <- c("GAD71_A", "GAD72_A", "GAD73_A", "GAD74_A", "GAD75_A", "GAD76_A", "GAD77_A")
gad_names <- c("nervous_on_edge", "cant_stop_worry", "worry_differentThings", "relax_probs", 
               "cant_sit_still", "too_irritable", "fear_future_dread")

for (i in seq_along(gad_vars)) {
  data[[gad_names[i]]] <- ifelse(is.na(data[[gad_vars[i]]]), NA, data[[gad_vars[i]]])
}

# Compute PHQ8_count
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

# Expanded recoding for all categories and variables
# NO category recoding
data$NOanhedonia <- ifelse(is.na(data$anhedonia), NA,
                            ifelse(data$anhedonia == 0, 1, 0))
data$NOsadness <- ifelse(is.na(data$sadness), NA,
                          ifelse(data$sadness == 0, 1, 0))
data$NOsleep <- ifelse(is.na(data$sleep), NA,
                              ifelse(data$sleep == 0, 1, 0))
data$NOenergy <- ifelse(is.na(data$energy), NA,
                               ifelse(data$energy == 0, 1, 0))
data$NOappetite <- ifelse(is.na(data$appetite), NA,
                                 ifelse(data$appetite == 0, 1, 0))
data$NOguilt <- ifelse(is.na(data$guilt), NA,
                        ifelse(data$guilt == 0, 1, 0))
data$NOconcentration <- ifelse(is.na(data$concentration), NA,
                                      ifelse(data$concentration == 0, 1, 0))
data$NOpsychomotor <- ifelse(is.na(data$psychomotor), NA,
                              ifelse(data$psychomotor == 0, 1, 0))

# SOME category recoding
data$SOMEanhedonia <- ifelse(is.na(data$anhedonia), NA,
                              ifelse(data$anhedonia == 1, 1, 0))
data$SOMEsadness <- ifelse(is.na(data$sadness), NA,
                            ifelse(data$sadness == 1, 1, 0))
data$SOMEsleep <- ifelse(is.na(data$sleep), NA,
                                ifelse(data$sleep == 1, 1, 0))
data$SOMEenergy <- ifelse(is.na(data$energy), NA,
                                 ifelse(data$energy == 1, 1, 0))
data$SOMEappetite <- ifelse(is.na(data$appetite), NA,
                                   ifelse(data$appetite == 1, 1, 0))
data$SOMEguilt <- ifelse(is.na(data$guilt), NA,
                          ifelse(data$guilt == 1, 1, 0))
data$SOMEconcentration <- ifelse(is.na(data$concentration), NA,
                                        ifelse(data$concentration == 1, 1, 0))
data$SOMEpsychomotor <- ifelse(is.na(data$psychomotor), NA,
                                ifelse(data$psychomotor == 1, 1, 0))

# MOSTLY category recoding
data$MOSTLYanhedonia <- ifelse(is.na(data$anhedonia), NA,
                                ifelse(data$anhedonia == 2, 1, 0))
data$MOSTLYsadness <- ifelse(is.na(data$sadness), NA,
                              ifelse(data$sadness == 2, 1, 0))
data$MOSTLYsleep <- ifelse(is.na(data$sleep), NA,
                                  ifelse(data$sleep == 2, 1, 0))
data$MOSTLYenergy <- ifelse(is.na(data$energy), NA,
                                   ifelse(data$energy == 2, 1, 0))
data$MOSTLYappetite <- ifelse(is.na(data$appetite), NA,
                                     ifelse(data$appetite == 2, 1, 0))
data$MOSTLYguilt <- ifelse(is.na(data$guilt), NA,
                            ifelse(data$guilt == 2, 1, 0))
data$MOSTLYconcentration <- ifelse(is.na(data$concentration), NA,
                                          ifelse(data$concentration == 2, 1, 0))
data$MOSTLYpsychomotor <- ifelse(is.na(data$psychomotor), NA,
                                  ifelse(data$psychomotor == 2, 1, 0))

# ALWAYS category recoding
data$ALWAYSanhedonia <- ifelse(is.na(data$anhedonia), NA,
                                ifelse(data$anhedonia == 3, 1, 0))
data$ALWAYSsadness <- ifelse(is.na(data$sadness), NA,
                              ifelse(data$sadness == 3, 1, 0))
data$ALWAYSsleep <- ifelse(is.na(data$sleep), NA,
                                  ifelse(data$sleep == 3, 1, 0))
data$ALWAYSenergy <- ifelse(is.na(data$energy), NA,
                                   ifelse(data$energy == 3, 1, 0))
data$ALWAYSappetite <- ifelse(is.na(data$appetite), NA,
                                     ifelse(data$appetite == 3, 1, 0))
data$ALWAYSguilt <- ifelse(is.na(data$guilt), NA,
                            ifelse(data$guilt == 3, 1, 0))
data$ALWAYSconcentration <- ifelse(is.na(data$concentration), NA,
                                          ifelse(data$concentration == 3, 1, 0))
data$ALWAYSpsychomotor <- ifelse(is.na(data$psychomotor), NA,
                                  ifelse(data$psychomotor == 3, 1, 0))

# ModSev category recoding
data$ModSevanhedonia <- ifelse(is.na(data$anhedonia), NA,
                                ifelse(data$anhedonia %in% c(2, 3), 1, 0))
data$ModSevsadness <- ifelse(is.na(data$sadness), NA,
                              ifelse(data$sadness %in% c(2, 3), 1, 0))
data$ModSevsleep <- ifelse(is.na(data$sleep), NA,
                                  ifelse(data$sleep %in% c(2, 3), 1, 0))
data$ModSevenergy <- ifelse(is.na(data$energy), NA,
                                   ifelse(data$energy %in% c(2, 3), 1, 0))
data$ModSevappetite <- ifelse(is.na(data$appetite), NA,
                                     ifelse(data$appetite %in% c(2, 3), 1, 0))
data$ModSevguilt <- ifelse(is.na(data$guilt), NA,
                            ifelse(data$guilt %in% c(2, 3), 1, 0))
data$ModSevconcentration <- ifelse(is.na(data$concentration), NA,
                                          ifelse(data$concentration %in% c(2, 3), 1, 0))
data$ModSevpsychomotor <- ifelse(is.na(data$psychomotor), NA,
                                  ifelse(data$psychomotor %in% c(2, 3), 1, 0))


# Create mental health symptoms variable
data$mh_symptoms <- ifelse(data$depression == 1 | data$anxiety == 1, 1, 
                      ifelse(data$depression == 0 & data$anxiety == 0, 0, NA))

# Create mental health categorical variable
data$mh_categorical <- ifelse(data$depression == 1 & data$anxiety == 0, 1, 
                         ifelse(data$depression == 0 & data$anxiety == 1, 2, 
                                ifelse(data$depression == 1 & data$anxiety == 1, 3, 
                                       ifelse(data$depression == 0 & data$anxiety == 0, 4, NA))))

# Label values
mh_categorical_labels <- c("Depression Only", "Anxiety Only", "Both depression and anxiety", "Neither depression nor anxiety")

# Frequency tables for mental health variables
frequencies(data$mh_symptoms)
frequencies(data$mh_categorical)

# Recode variables for chronic pain symptoms
data$PAIFRQ3M_A <- ifelse(data$PAIFRQ3M_A %in% c(7, 8, 9), NA, data$PAIFRQ3M_A)
data$PAIULMB3M_A <- ifelse(data$PAIULMB3M_A %in% c(7, 8, 9), NA, data$PAIULMB3M_A)
data$PAILLMB3M_A <- ifelse(data$PAILLMB3M_A %in% c(7, 8, 9), NA, data$PAILLMB3M_A)
data$PAIHDFC3M_A <- ifelse(data$PAIHDFC3M_A %in% c(7, 8, 9), NA, data$PAIHDFC3M_A)
data$PAIAPG3M_A <- ifelse(data$PAIAPG3M_A %in% c(7, 8, 9), NA, data$PAIAPG3M_A)
data$PAITOOTH3M_A <- ifelse(data$PAITOOTH3M_A %in% c(7, 8, 9), NA, data$PAITOOTH3M_A)
data$PAIWKLM3M_A <- ifelse(data$PAIWKLM3M_A %in% c(7, 8, 9), NA, data$PAIWKLM3M_A)
data$PAIAFFM3M_A <- ifelse(data$PAIAFFM3M_A %in% c(7, 8, 9), NA, data$PAIAFFM3M_A)
data$PAIAMNT_A <- ifelse(data$PAIAMNT_A %in% c(7, 8, 9), NA, data$PAIAMNT_A)

data$ChronicPain_any <- NA
data$pain_any <- NA
data$NO_ChronicPain_any <- NA
data$HighImpactCP <- NA
data$LowerImpactCP <- NA
data$HighImpactCP_largedenom <- NA
data$LowerImpactCP_largedenom <- NA
data$HighFAMImpactCP <- NA
data$LowerFAMImpactCP <- NA
data$HighFAMImpactCP_largedenom <- NA
data$LowerFAMImpactCP_largedenom <- NA
data$HighFAMImpactCP_largedenomNESTED <- NA
data$LowerFAMImpactCP_largedenomNESTED <- NA
data$ChronicPainOrdinal <- NA
data$anyChronicMigraine <- NA
data$high_impact_ChronicMigraine <- NA
data$high_FAMimpact_ChronicMigraine <- NA
data$cooccurence <- NA
data$chronic_migraine_cooccurence <- NA

# Recode ChronicPain_any
data$ChronicPain_any <- ifelse(is.na(data$PAIFRQ3M_A), NA,
                                      ifelse(data$PAIFRQ3M_A %in% c(3, 4), 1, 0))


# Recode pain_any
data$pain_any <- ifelse(is.na(data$PAIFRQ3M_A), NA,
                        ifelse(data$PAIFRQ3M_A %in% c(2, 3, 4), 1, 0))

# Recode NO_ChronicPain_any
data$NO_ChronicPain_any <- ifelse(is.na(data$ChronicPain_any), NA,
                                  ifelse(data$ChronicPain_any == 0, 1, 0))


# Recode pain location/type variables
pain_vars <- c("PAIBACK3M_A", "PAIULMB3M_A", "PAILLMB3M_A", "PAIHDFC3M_A", "PAIAPG3M_A", "PAITOOTH3M_A")
pain_names <- c("ModSevBackPain", "ModSevHandPain", "ModSevHipPain", "ModSevMigraine", "ModSevAbdominalPain", "ModSevToothPain")

# ModSev pain variables
for (i in seq_along(pain_vars)) {
  data[[pain_names[i]]] <- ifelse(is.na(data[[pain_vars[i]]]), NA,
                                  ifelse(data[[pain_vars[i]]] %in% c(3, 4), 1, 0))
}

# Any pain variables
any_pain_names <- c("anyBackPain", "anyHandPain", "anyHipPain", "anyMigraine", "anyAbdominalPain", "anyToothPain")
for (i in seq_along(pain_vars)) {
  data[[any_pain_names[i]]] <- ifelse(is.na(data[[pain_vars[i]]]), NA,
                                      ifelse(data[[pain_vars[i]]] %in% c(2, 3, 4), 1, 0))
}

# Recode pain severity
data$pain_severity <- ifelse(is.na(data$PAIAMNT_A), NA,
                             ifelse(data$PAIAMNT_A == 1, 1,
                                    ifelse(data$PAIAMNT_A == 2, 3,
                                           ifelse(data$PAIAMNT_A == 3, 2, NA))))

# Create severity binary variables
data$aLittlePain <- ifelse(is.na(data$pain_severity), NA,
                           ifelse(data$pain_severity == 1, 1, 0))
data$betweenLittleandLot <- ifelse(is.na(data$pain_severity), NA,
                                   ifelse(data$pain_severity == 2, 1, 0))
data$aLotPain <- ifelse(is.na(data$pain_severity), NA,
                        ifelse(data$pain_severity == 3, 1, 0))

# Functional limitation variables
data$HighImpactCP <- ifelse(is.na(data$ChronicPain_any) | is.na(data$PAIWKLM3M_A), NA,
                            ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(3, 4), 1,
                                   ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2), 0, NA)))

data$LowerImpactCP <- ifelse(is.na(data$ChronicPain_any) | is.na(data$PAIWKLM3M_A), NA,
                             ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2), 1,
                                    ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(3, 4), 0, NA)))

# Functional limitation variables with larger denominator
# Compute HighImpactCP_largedenom
data$HighImpactCP_largedenom <- ifelse(
  data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(3, 4), 1, # Chronic pain and high impact
  ifelse(
    data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2), 0, # Chronic pain but low impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)

# Compute LowerImpactCP_largedenom
data$LowerImpactCP_largedenom <- ifelse(
  data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2), 1, # Chronic pain and low impact
  ifelse(
    data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(3, 4), 0, # Chronic pain but high impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)
# High and Lower Family Impact
data$HighFAMImpactCP <- ifelse(is.na(data$ChronicPain_any) | is.na(data$PAIAFFM3M_A), NA,
                               ifelse(data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(3, 4), 1,
                                      ifelse(data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(1, 2), 0, NA)))

data$LowerFAMImpactCP <- ifelse(is.na(data$ChronicPain_any) | is.na(data$PAIAFFM3M_A), NA,
                                ifelse(data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(1, 2), 1,
                                       ifelse(data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(3, 4), 0, NA)))

# Compute HighFAMImpactCP_largedenom
data$HighFAMImpactCP_largedenom <- ifelse(
  data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(3, 4), 1, # High family impact
  ifelse(
    data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(1, 2), 0, # Low family impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)

# Compute HighFAMImpactCP_largedenomNESTED
data$HighFAMImpactCP_largedenomNESTED <- ifelse(
  data$HighImpactCP_largedenom == 1 & data$PAIAFFM3M_A %in% c(3, 4), 1, # High impact and high family impact
  ifelse(
    (data$ChronicPain_any == 1 | data$HighImpactCP_largedenom == 1) & data$PAIAFFM3M_A %in% c(1, 2), 0, # Either chronic pain or high impact and low family impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)

# Compute LowerFAMImpactCP_largedenom
data$LowerFAMImpactCP_largedenom <- ifelse(
  data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(1, 2), 1, # Low family impact
  ifelse(
    data$ChronicPain_any == 1 & data$PAIAFFM3M_A %in% c(3, 4), 0, # High family impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)

# Compute LowerFAMImpactCP_largedenomNESTED
data$LowerFAMImpactCP_largedenomNESTED <- ifelse(
  data$HighImpactCP_largedenom == 1 & data$PAIAFFM3M_A %in% c(3, 4), 1, # High impact and high family impact
  ifelse(
    (data$ChronicPain_any == 1 | data$HighImpactCP_largedenom == 1) & data$PAIAFFM3M_A %in% c(1, 2), 0, # Either chronic pain or high impact and low family impact
    ifelse(
      data$ChronicPain_any == 0, 0, # No chronic pain
      NA # Missing values
    )
  )
)

# Compute Chronic Pain Ordinal variable
data$ChronicPainOrdinal <- ifelse(
  data$ChronicPain_any == 0, 1, # No chronic pain
  ifelse(
    data$ChronicPain_any == 1 & data$LowerImpactCP_largedenom == 1, 2, # Chronic pain with lower impact
    ifelse(
      data$ChronicPain_any == 1 & data$HighImpactCP_largedenom == 1, 3, # Chronic pain with high impact
      NA # Missing values
    )
  )
)

# Compute any Chronic Migraine
data$anyChronicMigraine <- ifelse(is.na(data$anyMigraine) | is.na(data$ChronicPain_any), NA,
                                  ifelse(data$anyMigraine == 1 & data$ChronicPain_any == 1, 1, 0))

# High-Impact Chronic Migraine
data$high_impact_ChronicMigraine <- ifelse(is.na(data$anyChronicMigraine) | is.na(data$HighImpactCP_largedenom), NA,
                                           ifelse(data$anyChronicMigraine == 1 & data$HighImpactCP_largedenom == 1, 1, 0))

# Compute High Family Impact Chronic Migraine
data$high_FAMimpact_ChronicMigraine <- ifelse(is.na(data$high_impact_ChronicMigraine) | is.na(data$HighFAMImpactCP_largedenom), NA, 
                                              ifelse(data$high_impact_ChronicMigraine == 1 & data$HighFAMImpactCP_largedenom == 1, 1, 0))

# Co-occurrence of Chronic Pain and MH Symptoms
data$cooccurence <- ifelse(is.na(data$ChronicPain_any) | is.na(data$mh_symptoms), NA,
                           ifelse(data$ChronicPain_any == 0 & data$mh_symptoms == 0, 1,
                                  ifelse(data$ChronicPain_any == 1 & data$mh_symptoms == 0, 2,
                                         ifelse(data$ChronicPain_any == 0 & data$mh_symptoms == 1, 3,
                                                ifelse(data$ChronicPain_any == 1 & data$mh_symptoms == 1, 4, NA)))))

# Label cooccurence values
cooccurence_labels <- c("No chronic pain or MH symptoms", "Chronic Pain (Only)", "Mental Health (Only)", "Co-Occuring Symptoms")
data$cooccurence <- factor(data$cooccurence, levels = 1:4, labels = cooccurence_labels)

# Frequency table for cooccurence
frequencies <- function(x) {
  table(x, useNA = "ifany")
}
frequencies(data$cooccurence)

# Subset and frequency analysis for cooccurence not equal to 1
subset_cooccurence <- subset(data, cooccurence != "No chronic pain or MH symptoms")
frequencies(subset_cooccurence$cooccurence)

# Compute chronic migraine cooccurrence variable
data$chronic_migraine_cooccurence <- ifelse(is.na(data$anyChronicMigraine) | is.na(data$mh_symptoms), NA, 
                                            ifelse(data$anyChronicMigraine == 0 & data$mh_symptoms == 0, 1, 
                                                   ifelse(data$anyChronicMigraine == 1 & data$mh_symptoms == 0, 2, 
                                                          ifelse(data$anyChronicMigraine == 0 & data$mh_symptoms == 1, 3, 
                                                                 ifelse(data$anyChronicMigraine == 1 & data$mh_symptoms == 1, 4, NA)))))

# Label chronic migraine cooccurrence values
chronic_migraine_cooccurence_labels <- c("No chronic migraine or MH symptoms", "Chronic migraine (Only)", "Mental Health (Only)", "Co-Occuring Symptoms")
data$chronic_migraine_cooccurence <- factor(data$chronic_migraine_cooccurence, levels = 1:4, labels = chronic_migraine_cooccurence_labels)

# Frequency table for chronic migraine cooccurrence
frequencies(data$chronic_migraine_cooccurence)

# Subset and frequency analysis for chronic migraine cooccurrence not equal to 1
subset_chronic_migraine_cooccurence <- subset(data, chronic_migraine_cooccurence != "No chronic migraine or MH symptoms")
frequencies(subset_chronic_migraine_cooccurence$chronic_migraine_cooccurence)

# Recode variables for treatment-related measures
data$anxiety_med_NOW <- ifelse(data$ANXMED_A == 1, 1, ifelse(data$ANXMED_A == 2, 0, NA))
data$depress_med_NOW <- ifelse(data$DEPMED_A == 1, 1, ifelse(data$DEPMED_A == 2, 0, NA))
data$other_mh_med_12m <- ifelse(data$MHRX_A == 1, 1, ifelse(data$MHRX_A == 2, 0, NA))

# Compute any mental health medication in past 12 months
data$any_mh_med_12m <- ifelse(is.na(data$anxiety_med_NOW) & is.na(data$depress_med_NOW) & is.na(data$other_mh_med_12m), NA,
                              ifelse(data$anxiety_med_NOW == 1 | data$depress_med_NOW == 1 | data$other_mh_med_12m == 1, 1, 0))

# Compute any anxiety or depression medication in past 12 months
data$any_ad_med_12m <- ifelse(is.na(data$anxiety_med_NOW) & is.na(data$depress_med_NOW), NA,
                              ifelse(data$anxiety_med_NOW == 1 | data$depress_med_NOW == 1, 1, 0))

# Recode mental health therapy in past 12 months
data$any_mh_therapy_12m <- ifelse(data$MHTHRPY_A == 1, 1, ifelse(data$MHTHRPY_A == 2, 0, NA))

# Recode current mental health therapy
data$MH_therapy_NOW <- ifelse(data$MHTPYNOW_A == 1, 1, ifelse(data$MHTPYNOW_A == 2, 0, NA))
data$MH_therapy_NOW <- ifelse(data$any_mh_therapy_12m == 0, 0, data$MH_therapy_NOW)

# Compute any mental health treatment in past 12 months
data$any_mh_tx_12m <- ifelse(is.na(data$any_mh_med_12m) & is.na(data$any_mh_therapy_12m), NA,
                             ifelse(data$any_mh_med_12m == 1 | data$any_mh_therapy_12m == 1, 1, 0))

# Compute current mental health treatment
data$any_mh_tx_NOW <- ifelse(is.na(data$anxiety_med_NOW) & is.na(data$depress_med_NOW) & is.na(data$MH_therapy_NOW), NA,
                             ifelse(data$anxiety_med_NOW == 1 | data$depress_med_NOW == 1 | data$MH_therapy_NOW == 1, 1, 0))

# Compute mental health treatment approach in past 12 months
data$mh_tx_approach_12m <- ifelse(is.na(data$any_mh_med_12m) & is.na(data$any_mh_therapy_12m), NA,
                                  ifelse(data$any_mh_med_12m == 1 & data$any_mh_therapy_12m == 0, 1,
                                         ifelse(data$any_mh_med_12m == 0 & data$any_mh_therapy_12m == 1, 2,
                                                ifelse(data$any_mh_med_12m == 1 & data$any_mh_therapy_12m == 1, 3,
                                                       ifelse(data$any_mh_med_12m == 0 & data$any_mh_therapy_12m == 0, 4, NA)))))

# Label treatment approach
mh_tx_approach_labels <- c("Medication only", "Therapy only", "Both medication and therapy", "No A/D treatment")
data$mh_tx_approach_12m <- factor(data$mh_tx_approach_12m, levels = 1:4, labels = mh_tx_approach_labels)

# Frequency tables for treatment-related variables
frequencies <- function(x) {
  table(x, useNA = "ifany")
}

# Example frequencies
frequencies(data$any_mh_med_12m)
frequencies(data$any_mh_therapy_12m)
frequencies(data$mh_tx_approach_12m)

# Recode pain management strategies into binary variables
data$pt <- ifelse(data$PAIPHYSTPY_A == 1, 1, ifelse(data$PAIPHYSTPY_A == 2, 0, NA))
data$chiro <- ifelse(data$PAICHIRO_A == 1, 1, ifelse(data$PAICHIRO_A == 2, 0, NA))
data$cbt_pain <- ifelse(data$PAITALKTPY_A == 1, 1, ifelse(data$PAITALKTPY_A == 2, 0, NA))
data$self_mgmt <- ifelse(data$PAIPROGRAM_A == 1, 1, ifelse(data$PAIPROGRAM_A == 2, 0, NA))
data$peer_sppt <- ifelse(data$PAIGROUP_A == 1, 1, ifelse(data$PAIGROUP_A == 2, 0, NA))
data$yoga <- ifelse(data$PAIYOGA_A == 1, 1, ifelse(data$PAIYOGA_A == 2, 0, NA))
data$massage <- ifelse(data$PAIMASSAGE_A == 1, 1, ifelse(data$PAIMASSAGE_A == 2, 0, NA))
data$meditation <- ifelse(data$PAIMEDITAT_A == 1, 1, ifelse(data$PAIMEDITAT_A == 2, 0, NA))
data$other_pain_tx <- ifelse(data$PAIMOTHER_A == 1, 1, ifelse(data$PAIMOTHER_A == 2, 0, NA))

# Recode opioid use variables
data$Any_Rx12m <- ifelse(data$RX12M_A == 1, 1, ifelse(data$RX12M_A == 2, 0, NA))
data$AnyOpioid12m <- ifelse(data$OPD12M_A == 1, 1, ifelse(data$OPD12M_A == 2, 0, NA))
data$AnyOpioid3m <- ifelse(data$OPD3M_A == 1, 1, ifelse(data$OPD3M_A == 2, 0, NA))
data$AcuteOpioid3m <- ifelse(data$OPDACUTE_A == 1, 1, ifelse(data$OPDACUTE_A == 2, 0, NA))
data$ChronicOpioid3m <- ifelse(data$OPDCHRONIC_A == 1, 1, ifelse(data$OPDCHRONIC_A == 2, 0, NA))
data$ChronicOpdFreq3m <- ifelse(data$OPDFREQ_A == 1, 1, ifelse(data$OPDFREQ_A == 2, 2, ifelse(data$OPDFREQ_A == 3, 3, NA)))

# Compute new opioid variables
data$AnyOpioid12m_NEW <- ifelse(data$Any_Rx12m == 7, 7,
                                ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 0, 0,
                                       ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1, 1, NA)))

data$AnyOpioid3m_NEW <- ifelse(data$Any_Rx12m == 0, 0,
                               ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 0, 0,
                                      ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 0, 0,
                                             ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 1, 1, NA))))

data$AcuteOpioid3m_NEW <- ifelse(data$Any_Rx12m == 0, 0,
                                 ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 0, 0,
                                        ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 0, 0,
                                               ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 1 & data$AcuteOpioid3m == 1, 1, 0))))

data$ChronicOpioid3m_NEW <- ifelse(data$Any_Rx12m == 0, 0,
                                   ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 0, 0,
                                          ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 0, 0,
                                                 ifelse(data$Any_Rx12m == 1 & data$AnyOpioid12m == 1 & data$AnyOpioid3m == 1 & data$ChronicOpioid3m == 1, 1, 0))))

# Compute any pain treatment variable
data$any_pain_treatment <- ifelse(
  data$ChronicOpioid3m_NEW == 1 | data$pt == 1 | data$chiro == 1 | data$cbt_pain == 1 |
    data$self_mgmt == 1 | data$peer_sppt == 1 | data$yoga == 1 | data$massage == 1 |
    data$meditation == 1 | data$other_pain_tx == 1, 1,
  ifelse(
    data$ChronicOpioid3m_NEW == 0 & data$pt == 0 & data$chiro == 0 & data$cbt_pain == 0 &
      data$self_mgmt == 0 & data$peer_sppt == 0 & data$yoga == 0 & data$massage == 0 &
      data$meditation == 0 & data$other_pain_tx == 0, 0, NA
  )
)

# Compute no pain treatment variable
data$NO_pain_treatment <- ifelse(data$any_pain_treatment == 1, 0, ifelse(data$any_pain_treatment == 0, 1, NA))

# Compute integrated treatment approach variable
data$Integrated_tx_approach <- ifelse(
  data$any_pain_treatment == 0 & data$any_mh_tx_NOW == 0, 1,
  ifelse(data$any_pain_treatment == 1 & data$any_mh_tx_NOW == 0, 2,
         ifelse(data$any_pain_treatment == 0 & data$any_mh_tx_NOW == 1, 3,
                ifelse(data$any_pain_treatment == 1 & data$any_mh_tx_NOW == 1, 4, NA))))

# Label integrated treatment approach
data$Integrated_tx_approach <- factor(
  data$Integrated_tx_approach,
  levels = 1:4,
  labels = c(
    "No pain treatment and no MH treatment",
    "Pain treatment only",
    "MH treatment only",
    "Both Pain and MH treatment"
  )
)

# Frequency tables for pain treatment variables
frequencies <- function(x) {
  table(x, useNA = "ifany")
}

frequencies(data$any_pain_treatment)
frequencies(data$Integrated_tx_approach)

# Create binaries for combinations of treatment and need
data$txANDsymptoms <- ifelse(data$any_mh_tx_12m == 1 & data$mh_symptoms == 1, 1,
                             ifelse(data$any_mh_tx_12m == 0 | data$mh_symptoms == 0, 0, NA))

data$symptomsNOtx <- ifelse(data$mh_symptoms == 1 & data$any_mh_tx_12m == 0, 1,
                            ifelse(data$mh_symptoms == 0 | data$any_mh_tx_12m == 1, 0, NA))

data$NoSymptomsNOtx <- ifelse(data$mh_symptoms == 0 & data$any_mh_tx_12m == 0, 1,
                              ifelse(data$mh_symptoms == 1 | data$any_mh_tx_12m == 1, 0, NA))

data$txNOsymptoms <- ifelse(data$any_mh_tx_12m == 1 & data$mh_symptoms == 0, 1,
                            ifelse(data$any_mh_tx_12m == 0 | data$mh_symptoms == 1, 0, NA))

# Create mh_utiliz_need variable
data$mh_utiliz_need <- ifelse(data$NoSymptomsNOtx == 1, 1,
                              ifelse(data$txNOsymptoms == 1, 2,
                                     ifelse(data$txANDsymptoms == 1, 3,
                                            ifelse(data$symptomsNOtx == 1, 4, NA))))

# Label mh_utiliz_need variable
data$mh_utiliz_need <- factor(data$mh_utiliz_need,
                              levels = 1:4,
                              labels = c(
                                "No MH symptoms and no MH treatment",
                                "MH symptoms controlled with MH treatment",
                                "MH symptoms poorly controlled despite MH Treatment",
                                "MH symptoms and not using MH Treatments"
                              ))

# Create MHrelevance variable
data$MHrelevance <- ifelse(data$NoSymptomsNOtx == 0, 1,
                           ifelse(data$NoSymptomsNOtx == 1, 0, NA))


# Create screening and referral-related variables
data$DocSaidDepress <- ifelse(data$DEPEV_A == 1, 1, ifelse(data$DEPEV_A == 2, 0, NA))
data$DocSaidAnxiety <- ifelse(data$ANXEV_A == 1, 1, ifelse(data$ANXEV_A == 2, 0, NA))
data$DocSaidAnyMH <- ifelse(data$DocSaidAnxiety == 1 | data$DocSaidDepress == 1, 1,
                            ifelse(data$DocSaidAnxiety == 0 & data$DocSaidDepress == 0, 0, NA))
data$DocNEVERsaidMH <- ifelse(data$DocSaidAnyMH == 0, 1, ifelse(data$DocSaidAnyMH == 1, 0, NA))
data$NOusualplace <- ifelse(data$USUALPL_A == 1, 0, ifelse(data$USUALPL_A == 2, 1, NA))
data$NOpaidSickLeave <- ifelse(data$EMPPDSKLV_A == 1, 0, ifelse(data$EMPPDSKLV_A == 2, 1, NA))
data$NOworkedLastWeek <- ifelse(data$EMPWRKLSWK_A == 1, 0, ifelse(data$EMPWRKLSWK_A == 2, 1, NA))

# Recode variable for general health status
data$healthstatus <- ifelse(data$PHSTAT_A == 1, 5,
                            ifelse(data$PHSTAT_A == 2, 4,
                                   ifelse(data$PHSTAT_A == 3, 3,
                                          ifelse(data$PHSTAT_A == 4, 2,
                                                 ifelse(data$PHSTAT_A == 5, 1, NA)))))

# Label healthstatus variable
data$healthstatus <- factor(data$healthstatus,
                            levels = 1:5,
                            labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))

# Recode functional limitations variables
data$soc_work_limit <- ifelse(data$SOCWRKLIM_A == 1, 1, ifelse(data$SOCWRKLIM_A == 2, 0, NA))
data$soc_errands_limit <- ifelse(data$SOCERRNDS_A == 1, 0, ifelse(data$SOCERRNDS_A %in% c(2, 3, 4), 1, NA))
data$soc_participate_limit <- ifelse(data$SOCSCLPAR_A == 1, 0, ifelse(data$SOCSCLPAR_A %in% c(2, 3, 4), 1, NA))

# Create any functional limitation variable
data$any_functional_limit <- ifelse(
  data$soc_work_limit == 1 | data$soc_errands_limit == 1 | data$soc_participate_limit == 1, 1,
  ifelse(data$soc_work_limit == 0 & data$soc_errands_limit == 0 & data$soc_participate_limit == 0, 0, NA))

# Label any_functional_limit variable
data$any_functional_limit <- factor(data$any_functional_limit,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

# Create functional limitations combinations variable
data$func_Limitations_combos <- ifelse(
  data$soc_work_limit == 0 & data$soc_errands_limit == 0 & data$soc_participate_limit == 0, 0,
  ifelse(data$soc_work_limit == 1 & data$soc_errands_limit == 0 & data$soc_participate_limit == 0, 1,
         ifelse(data$soc_work_limit == 0 & data$soc_errands_limit == 1 & data$soc_participate_limit == 0, 2,
                ifelse(data$soc_work_limit == 0 & data$soc_errands_limit == 0 & data$soc_participate_limit == 1, 3, 2))))

# Create binaries for combinations of treatment and functional limitation
data$txANDlimitation <- ifelse(data$any_mh_tx_12m == 1 & data$any_functional_limit == 1, 1,
                               ifelse(data$any_mh_tx_12m == 0 | data$any_functional_limit == 0, 0, NA))

data$limitationNOtx <- ifelse(data$any_functional_limit == 1 & data$any_mh_tx_12m == 0, 1,
                              ifelse(data$any_functional_limit == 0 | data$any_mh_tx_12m == 1, 0, NA))

data$NoLimitationNOtx <- ifelse(data$any_functional_limit == 0 & data$any_mh_tx_12m == 0, 1,
                                ifelse(data$any_functional_limit == 1 | data$any_mh_tx_12m == 1, 0, NA))

data$txNOlimitation <- ifelse(data$any_mh_tx_12m == 1 & data$any_functional_limit == 0, 1,
                              ifelse(data$any_mh_tx_12m == 0 | data$any_functional_limit == 1, 0, NA))

# Create mh_utiliz_function variable
data$mh_utiliz_function <- ifelse(data$NoLimitationNOtx == 1, 1,
                                  ifelse(data$txNOlimitation == 1, 2,
                                         ifelse(data$txANDlimitation == 1, 3,
                                                ifelse(data$limitationNOtx == 1, 4, NA))))

# Label mh_utiliz_function variable
data$mh_utiliz_function <- factor(data$mh_utiliz_function,
                                  levels = 1:4,
                                  labels = c(
                                    "No functional limitation and no MH treatment",
                                    "MH treatment and no functional limitation",
                                    "MH Treatment and functional limitation",
                                    "Functional limitation and not using MH Treatments"
                                  ))

# Recode pain efficacy variable
data$effective <- ifelse(data$PAINMEFF_A == 1, 1, ifelse(data$PAINMEFF_A == 2, 0, NA))

# Functional impact of pain binaries
data$CPNeverLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 1, 1,
                             ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% 2:4, 0, NA))

data$CPSometimesLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 2, 1,
                                 ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 3, 4), 0, NA))

data$CPMostDaysLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 3, 1,
                                ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2, 4), 0, NA))

data$CPEveryDayLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 4, 1,
                                ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2, 3), 0, NA))

# Functional limitation affecting families
data$FAMNeverHighImpact <- ifelse(data$PAIAFFM3M_A == 1, 1,
                                  ifelse(data$PAIAFFM3M_A %in% c(2, 3, 4), 0, NA))

data$FAMSometimesHiImpact <- ifelse(data$PAIAFFM3M_A == 2, 1,
                                    ifelse(data$PAIAFFM3M_A %in% c(1, 3, 4), 0, NA))

data$FAMMostDaysHighImpact <- ifelse(data$PAIAFFM3M_A == 3, 1,
                                     ifelse(data$PAIAFFM3M_A %in% c(1, 2, 4), 0, NA))

data$FAMEveryDayHighImpact <- ifelse(data$PAIAFFM3M_A == 4, 1,
                                     ifelse(data$PAIAFFM3M_A %in% c(1, 2, 3), 0, NA))

data$HighImpactFAM <- ifelse(data$PAIAFFM3M_A %in% c(3, 4), 1,
                             ifelse(data$PAIAFFM3M_A %in% c(1, 2), 0, NA))

# Frequency tables for variables
frequencies <- function(x) {
  table(x, useNA = "ifany")
}

frequencies(data$DocSaidAnxiety)
frequencies(data$DocSaidDepress)
frequencies(data$DocSaidAnyMH)
frequencies(data$DocNEVERsaidMH)
frequencies(data$NOusualplace)
frequencies(data$NOpaidSickLeave)
frequencies(data$NOworkedLastWeek)
frequencies(data$healthstatus)
frequencies(data$any_functional_limit)
frequencies(data$mh_utiliz_function)
frequencies(data$CPNeverLimits)
frequencies(data$CPSometimesLimits)
frequencies(data$CPMostDaysLimits)
frequencies(data$CPEveryDayLimits)
frequencies(data$FAMNeverHighImpact)
frequencies(data$FAMSometimesHiImpact)
frequencies(data$FAMMostDaysHighImpact)
frequencies(data$FAMEveryDayHighImpact)
frequencies(data$HighImpactFAM)


# Create MHscreeningAccurate variable
data$MHscreeningAccurate <- ifelse(data$MHrelevance == 1 & data$DocSaidAnyMH == 1, 1,
                                   ifelse(data$MHrelevance == 0 & data$DocSaidAnyMH == 1, 2,
                                          ifelse(data$MHrelevance == 0 & data$DocSaidAnyMH == 0, 3,
                                                 ifelse(data$MHrelevance == 1 & data$DocSaidAnyMH == 0, 4, NA))))

# Label MHscreeningAccurate variable
data$MHscreeningAccurate <- factor(data$MHscreeningAccurate,
                                   levels = 1:4,
                                   labels = c(
                                     "Correctly Advised",
                                     "Incorrectly Advised",
                                     "Correctly Did Not Advise",
                                     "Incorrectly Did Not Advise"
                                   ))

# Frequency tables for combinations of treatment and need variables
frequencies <- function(x) {
  table(x, useNA = "ifany")
}

frequencies(data$txANDsymptoms)
frequencies(data$symptomsNOtx)
frequencies(data$NoSymptomsNOtx)
frequencies(data$txNOsymptoms)
frequencies(data$mh_utiliz_need)
frequencies(data$MHrelevance)
frequencies(data$MHscreeningAccurate)

# Recode variable for general health status
data$healthstatus <- ifelse(data$PHSTAT_A == 1, 5,
                            ifelse(data$PHSTAT_A == 2, 4,
                                   ifelse(data$PHSTAT_A == 3, 3,
                                          ifelse(data$PHSTAT_A == 4, 2,
                                                 ifelse(data$PHSTAT_A == 5, 1, NA)))))

# Label general health status
data$healthstatus <- factor(data$healthstatus,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))

# Recode functional limitations variables
data$soc_work_limit <- ifelse(data$SOCWRKLIM_A == 1, 1, ifelse(data$SOCWRKLIM_A == 2, 0, NA))
data$soc_errands_limit <- ifelse(data$SOCERRNDS_A == 1, 0, ifelse(data$SOCERRNDS_A %in% c(2, 3, 4), 1, NA))
data$soc_participate_limit <- ifelse(data$SOCSCLPAR_A == 1, 0, ifelse(data$SOCSCLPAR_A %in% c(2, 3, 4), 1, NA))

# Create variable for functional limitations
data$any_functional_limit <- ifelse(data$soc_work_limit == 1 | data$soc_errands_limit == 1 | data$soc_participate_limit == 1, 1,
                                    ifelse(data$soc_work_limit == 0 & data$soc_errands_limit == 0 & data$soc_participate_limit == 0, 0, NA))

# Label functional limitations
data$any_functional_limit <- factor(data$any_functional_limit,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

# Create variable for combinations of functional limitations
data$func_Limitations_combos <- with(data, ifelse(
  soc_work_limit == 0 & soc_errands_limit == 0 & soc_participate_limit == 0, 0,
  ifelse(soc_work_limit == 1 & soc_errands_limit == 0 & soc_participate_limit == 0, 1,
         ifelse(soc_work_limit == 0 & soc_errands_limit == 1 & soc_participate_limit == 0, 2,
                ifelse(soc_work_limit == 0 & soc_errands_limit == 0 & soc_participate_limit == 1, 3, 2)))))

# Compute binaries for combinations of treatment and functional limitation
data$txANDlimitation <- ifelse(data$any_mh_tx_12m == 1 & data$any_functional_limit == 1, 1,
                               ifelse(data$any_mh_tx_12m == 0 | data$any_functional_limit == 0, 0, NA))

data$limitationNOtx <- ifelse(data$any_functional_limit == 1 & data$any_mh_tx_12m == 0, 1,
                              ifelse(data$any_functional_limit == 0 | data$any_mh_tx_12m == 1, 0, NA))

data$NoLimitationNOtx <- ifelse(data$any_functional_limit == 0 & data$any_mh_tx_12m == 0, 1,
                                ifelse(data$any_functional_limit == 1 | data$any_mh_tx_12m == 1, 0, NA))

data$txNOlimitation <- ifelse(data$any_mh_tx_12m == 1 & data$any_functional_limit == 0, 1,
                              ifelse(data$any_mh_tx_12m == 0 | data$any_functional_limit == 1, 0, NA))

# Create mh_utiliz_function variable
data$mh_utiliz_function <- ifelse(data$NoLimitationNOtx == 1, 1,
                                  ifelse(data$txNOlimitation == 1, 2,
                                         ifelse(data$txANDlimitation == 1, 3,
                                                ifelse(data$limitationNOtx == 1, 4, NA))))

# Label mh_utiliz_function variable
data$mh_utiliz_function <- factor(data$mh_utiliz_function,
                                  levels = c(1, 2, 3, 4),
                                  labels = c(
                                    "No functional limitation and no MH treatment",
                                    "MH treatment and no functional limitation",
                                    "MH Treatment and functional limitation",
                                    "Functional limitation and not using MH Treatments"
                                  ))

# Recode pain efficacy variable
data$effective <- ifelse(data$PAINMEFF_A == 1, 1, ifelse(data$PAINMEFF_A == 2, 0, NA))

# Functional impact of pain binaries
data$CPNeverLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 1, 1,
                             ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% 2:4, 0, NA))

data$CPSometimesLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 2, 1,
                                 ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 3, 4), 0, NA))

data$CPMostDaysLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 3, 1,
                                ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2, 4), 0, NA))

data$CPEveryDayLimits <- ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A == 4, 1,
                                ifelse(data$ChronicPain_any == 1 & data$PAIWKLM3M_A %in% c(1, 2, 3), 0, NA))

# Functional limitation affecting families
data$FAMNeverHighImpact <- ifelse(data$PAIAFFM3M_A == 1, 1,
                                  ifelse(data$PAIAFFM3M_A %in% c(2, 3, 4), 0, NA))

data$FAMSometimesHiImpact <- ifelse(data$PAIAFFM3M_A == 2, 1,
                                    ifelse(data$PAIAFFM3M_A %in% c(1, 3, 4), 0, NA))

data$FAMMostDaysHighImpact <- ifelse(data$PAIAFFM3M_A == 3, 1,
                                     ifelse(data$PAIAFFM3M_A %in% c(1, 2, 4), 0, NA))

data$FAMEveryDayHighImpact <- ifelse(data$PAIAFFM3M_A == 4, 1,
                                     ifelse(data$PAIAFFM3M_A %in% c(1, 2, 3), 0, NA))

data$HighImpactFAM <- ifelse(data$PAIAFFM3M_A %in% c(3, 4), 1,
                             ifelse(data$PAIAFFM3M_A %in% c(1, 2), 0, NA))

# Recode NOTCOV_A to Uninsured
data$Uninsured <- ifelse(data$NOTCOV_A == 1, 1, ifelse(data$NOTCOV_A == 2, 0, NA))

# Recode INCWELF_A to PublicAssistance
data$PublicAssistance <- ifelse(data$INCWELF_A == 1, 1, ifelse(data$INCWELF_A == 2, 0, NA))

# Recode URBRRL to Rural and Urban
data$Rural <- ifelse(data$URBRRL == 4, 1, 0)
data$Urban <- ifelse(data$URBRRL == 1, 1, 0)

# Recode SEX_A to Female and Male
data$Female <- ifelse(data$SEX_A == 2, 1, ifelse(data$SEX_A == 1, 0, NA))
data$Male <- ifelse(data$SEX_A == 1, 1, ifelse(data$SEX_A == 2, 0, NA))

# Create GenderPain variable
data$GenderPain <- ifelse(data$Male == 1 & data$ChronicPain_any == 1, 1,
                          ifelse(data$Male == 0 & data$ChronicPain_any == 1, 2,
                                 ifelse(data$Male == 1 & data$ChronicPain_any == 0, 3,
                                        ifelse(data$Male == 0 & data$ChronicPain_any == 0, 4, NA))))

# Recode CANEV_A to cancer
data$cancer <- ifelse(data$CANEV_A == 1, 1, ifelse(data$CANEV_A == 2, 0, NA))

# Recode HISPALLP_A to race/ethnicity variables
data$Hispanic <- ifelse(data$HISPALLP_A == 1, 1, 0)
data$NHW <- ifelse(data$HISPALLP_A == 2, 1, 0)
data$BlackAfAmer <- ifelse(data$HISPALLP_A == 3, 1, 0)
data$Asian <- ifelse(data$HISPALLP_A == 4, 1, 0)
data$AIANonly <- ifelse(data$HISPALLP_A == 5, 1, 0)
data$AIAN_multiracialonly <- ifelse(data$HISPALLP_A == 6, 1, 0)
data$AIAN_all <- ifelse(data$HISPALLP_A %in% c(5, 6), 1, 0)
data$Other <- ifelse(data$HISPALLP_A == 7, 1, 0)

# Recode MARITAL_A to CurrentlyHasPartner and NOcurrentPartner
data$CurrentlyHasPartner <- ifelse(data$MARITAL_A %in% c(1, 2), 1, ifelse(data$MARITAL_A == 3, 0, NA))
data$NOcurrentPartner <- ifelse(data$MARITAL_A == 3, 1, ifelse(data$MARITAL_A %in% c(1, 2), 0, NA))

# Recode MEDDL12M_A to DelayedMedicalCareDueToCost
data$DelayedMedicalCareDueToCost <- ifelse(data$MEDDL12M_A == 1, 1, ifelse(data$MEDDL12M_A == 2, 0, NA))

# Recode WRKHLTHFC_A to WorkInHealthCare
data$WorkInHealthCare <- ifelse(data$WRKHLTHFC_A == 1, 1, ifelse(data$WRKHLTHFC_A == 2, 0, NA))

# Recode PCNTLT18TC to ChildAtHome
data$ChildAtHome <- ifelse(data$PCNTLT18TC > 0, 1, 0)

# Create categorical and binary Age variables (CHECK WITH JENN)
data$AgeCat <- cut(data$AGEP_A,
                   breaks = c(-Inf, 24, 34, 44, 54, 64, 74, 84, Inf),
                   labels = c("1", "2", "3", "4", "5", "6", "7", "8"),
                   right = TRUE, include.lowest = TRUE)
data$age18_24 <- ifelse(data$AgeCat == "1", 1, 0)
data$age25_34 <- ifelse(data$AgeCat == "2", 1, 0)
data$age35_44 <- ifelse(data$AgeCat == "3", 1, 0)
data$age45_54 <- ifelse(data$AgeCat == "4", 1, 0)
data$age55_64 <- ifelse(data$AgeCat == "5", 1, 0)
data$age65_74 <- ifelse(data$AgeCat == "6", 1, 0)
data$age75_84 <- ifelse(data$AgeCat == "7", 1, 0)
data$age85plus <- ifelse(data$AgeCat == "8", 1, 0)

# Create categorical and binary Education variables
data$EducCat <- cut(data$EDUC_A,
                    breaks = c(-Inf, 2, 4, 7, 8, 11),
                    labels = c("1", "2", "3", "4", "5"),
                    right = TRUE, include.lowest = TRUE)
data$NoHighSchool <- ifelse(data$EducCat == "1", 1, 0)
data$HighSchoolGrad <- ifelse(data$EducCat == "2", 1, 0)
data$SomeCollege <- ifelse(data$EducCat == "3", 1, 0)
data$Bachelors <- ifelse(data$EducCat == "4", 1, 0)
data$GradSchool <- ifelse(data$EducCat == "5", 1, 0)

# Write final dataset
write.csv(data[535:732], here("analysis", "data", "derivedData", "GC_20241021_NHISadult2019_data_newvars.csv"))
