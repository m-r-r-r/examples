# Load and prep data  ----

library(tidyverse)
library(haven) # reads sas
library(srvyr)
library(survey)
library(purrr)

#Reading in multiyear SAS data/catalog file
# brfss_my <- read_sas("brfss_2014_2024.sas7bdat",
#                      "brfsmy.sas7bcat",
#                      .name_repair="universal")


# Make everything lowercase
brfss_my <- rename_with(brfss_my, tolower, .cols = everything())

## Filter multiyear file and tidy county names ----
# Period and space can't be in the county names when rendering final report
brfss_county <-
  brfss_my %>%
  filter(datayear %in% 2022:2024) 



# allow lonely PSUs - survey package
# remedies error when trying to have psu = 1
options(survey.lonely.psu = "certainty")


## Source file for profile sections ----
source("Reports/brfss/County BRFSS Reports/chronic_conditions.R")
source("Reports/brfss/County BRFSS Reports/demographics.R")
source("Reports/brfss/County BRFSS Reports/prevent_behaviors.R")
source("Reports/brfss/County BRFSS Reports/demographics.R")
source("Reports/brfss/County BRFSS Reports/risk_factors.R")
source("Reports/brfss/County BRFSS Reports/SDoH.R")


## Combine all results into single df for reports ----
# Easier than running functions each time
county_profile_df <-
  bind_rows(
    chronic_cond_result,
    demo_result,
    prevent_behav_result,
    risk_fact_result,
    sdoh_result
  ) %>%
  filter(RSE_Flag != "RSE High")


# Write combibed results df
# Since things take a few minutes to run this is faster at the moment
#write.csv(county_profile_df, "county_profile_df.csv", row.names=FALSE)



# Quarto report ----

## County list ----
# Create a list of the counties to loop
county_list <- expand.grid(
  county = c("Barre",
         "Burlington",
         "Morrisville",
         "Newport",
         "StJohnsbury",
         "Middlebury",
         "Rutland",
         "Bennington",
         "Springfield",
         "Brattleboro"),
  stringsAsFactors = FALSE)


## File rendering df ----
# Creates a df that is used to create the file names and indicates what variables (county) to loop through
rendering_data <- 
  county_list %>%
  mutate(
    output_format = "docx",       
    output_file = paste(          # Output file name
      county, "BRFSS_profile.docx",
      sep = "-"),
    execute_params = map( # Named list of parameters in YAML
      county,
      \(county) list(county = county))) %>%
  dplyr::select(-c(county))

rendering_data


## Render brfss_county_profiles.qmd by county ----
# Map the rendering of the word doc by county using the qmd identified
pwalk(
  .l = rendering_data,          # Dataframe to map over
  .f = quarto_render,   # Quarto render function
  input = "brfss_county_profiles.qmd",    # Named arguments of .f
  .progress = TRUE              
)



