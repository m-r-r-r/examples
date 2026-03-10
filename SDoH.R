
# Social Determinants of Health ----

## Data prep ----
sdoh_data <- 
  brfss_county %>%
  mutate(
    # Dissatisfied or Very dissatisfied w/life
    life_satisfied = case_when(
      lsatisfy %in% 3:4 ~ "Dissatisfied or Very dissatisfied with life",
      lsatisfy %in% 1:2 ~ "0"),
    # Rarely/never receive social/emotional support
    emot_support = case_when(
      emtsuprt %in% 4:5 ~ "Rarely or never receive social or emotional support",
      emtsuprt %in% 1:3 ~ "0"),
    # Lost job or had hours reduced
    job_security = case_when(
      sdhemply == 1 ~ "Lost job or had hours reduced in past year",
      sdhemply == 2 ~ "0"),
    # Always, usually, or sometimes food insecure
    food_insecure = case_when(
      sdhfood1 %in% 1:3 ~ "Always, usually or sometimes bought food that did not last in past year",
      sdhfood1 %in% 4:5 ~ "0"),
    # Unable to pay rent or mortgage on time
    housing_security = case_when(
      houssec == 1 ~ "Unable to pay rent or mortgage on time in past year",
      houssec == 2 ~ "0"),
    # Lack of reliable transportation
    transporation = case_when(
      sdhtrnsp == 1 ~ "Lack of reliable transportation in past year",
      sdhtrnsp == 2 ~ "0"),
    # Considered attempting suicide
    suicide = case_when(
      suiccons == 1 ~ "Seriously considered suicide in past year",
      suiccons == 2 ~ "0")
  )

## Design object ----
sdoh_design <- 
  sdoh_data %>%
  as_survey_design(strata = newstrat, 
                   weight =  ._llcpwt, 
                   ids = ._psu, 
                   nest = T)

## List of variables ----
# Note the var names must match the names assigned in data prep. step 
# Variables need to be contained in a list for purrr::map() 
# "county_var" is the identifying the district offices
sdoh_vars <-
  list(
    list(var = "life_satisfied",
         county_var = "county"),
    list(var = "emot_support",
         county_var = "county"),
    list(var = "job_security",
         county_var = "county"),
    list(var = "food_insecure",
         county_var = "county"),
    list(var = "housing_security",
         county_var = "county"),
    list(var = "transporation",
         county_var = "county"),
    list(var = "suicide", 
         county_var = "county")
  )

## Map the data ----

# The results are stored in a list

# State
sdoh_result_state <- 
  purrr::map(
    .x = sdoh_vars,
    .f = ~ brfss_one_var(
      dat = sdoh_design,
      group_var = .x$var
    )
  ) 

# county
sdoh_result_do <- 
  purrr::map(
    .x = sdoh_vars,
    .f = ~ brfss_two_var(
      dat = sdoh_design,
      group_var1 = .x$county_var,
      group_var2 = .x$var
    )
  )

# Convert the State results to dataframe 
sdoh_result_state_df <- 
  list_rbind(sdoh_result_state) %>%
  mutate(county = "State",
         years = "2022/2024")

# Convert the county results to dataframe 
sdoh_result_county_df <- 
  list_rbind(sdoh_result_do) %>%
  mutate(years = "2022/2024")

# Combine State and county dataframes
sdoh_result <-
  rbind(sdoh_result_county_df,
        sdoh_result_state_df) %>%
  mutate(group = "sdoh")











