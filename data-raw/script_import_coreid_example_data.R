# Load and save example data used in ThermalSampleN package

library(tidyverse)

# Import raw data
coreid_data <- readr::read_csv2("./data-raw/coreid_data_raw.csv") %>%
  dplyr::mutate(col = as.factor(col))
head(coreid_data)

# Save raw data as .Rdata file
usethis::use_data(coreid_data)


