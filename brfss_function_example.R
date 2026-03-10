library(haven)
library(tidyverse)
library(labelled)
library(survey)
library(srvyr)


# Read in raw 2024 BRFSS xpt file
# (https://www.cdc.gov/brfss/annual_data/2024/files/LLCP2024XPT.zip)
brfss_raw <- read_xpt("brfss_functions/LLCP2024XPT.zip",
                      .name_repair="universal")

# Make everything lowercase
brfss_raw <- rename_with(brfss_raw, tolower, .cols = everything())


