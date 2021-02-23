
# Background --------------------------------------------------------------

# The Brugada Syndrome Risk Stratification calculator has a few graphs. One of
# the graphs is a line graph thats outlines the risk of experiencing an event at
# 5 years based on the country. This graph is based on table 5 in the Supp.
# Material.

# This script is the cleaning script after copying it from a work doc to an
# excel/csv file.


# Housekeeping ------------------------------------------------------------

library(dplyr)

# Reading in the data -----------------------------------------------------

RF_tab <- read.csv(file = "rf_perm_table.csv", header = TRUE)
RF_tab_clean <- RF_tab %>% 
  select(SC:France) %>% 
  tidyr::pivot_longer(cols = AVE:France, names_to = "country") %>% 
  mutate(value = as.numeric(gsub(pattern = "%", replacement = "", x = .$value))) %>% 
  rename(Risk_Score = SC, Predicted_Risk = value) %>% 
  mutate(country = gsub(pattern = "\\.", replacement = " ", x = .$country)) %>% 
  mutate(country = gsub(pattern = "AVE", replacement = "Average", x = .$country))

write.csv(x = RF_tab_clean, file = "RF_Tab_Clean.csv", row.names = FALSE)

