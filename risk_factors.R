
# Risk Factors ----

# Data prep ----
## 2022 & 2024 ----
risk_fact_data_22_24 <- 
  brfss_county %>%
  filter(datayear == 2022 | datayear == 2024) %>%
  mutate(
    # Engaged in high risk behaviors - HIV
    hiv_risk = case_when(
      hivrisk5 == 1 ~ "Engaged in high-risk HIV transmission behaviors in last year",
      hivrisk5 == 2 ~ "0"))

## 2023 & 2024 ----
risk_fact_data_23_24 <- 
  brfss_county %>%
  filter(datayear %in% 2023:2024) %>%
  mutate(
    # At least one drink in last month
    any_drink = case_when(
      drnkany5 == 1 ~ "At least one drink in last month",
      drnkany5 == 2 ~ "0"),
    # At risk for binge drinking
    binge_drink = case_when(
      ._rfbing6 == 1 ~ "0",
      ._rfbing6 == 2 ~ "Reported binge drinking"),
    # At risk for heavy drinking
    heavy_drink = case_when(
      ._rfdrhv8 == 1 | ._rfdrhv9 == 1 ~ "0",
      ._rfdrhv8 == 2 | ._rfdrhv9 == 2 ~ "Reported heavy drinking"),
    # Used marijuana in last month
    use_mj = case_when(
      mja301 == 88 ~ "0",
      mja301 %in% 1:30 ~ "Used cannabis in last month"),
    # Drove after using marijuana in last month
    use_mj_drive = case_when(
      mjadrive1 == 88 ~ "0",
      mjadrive1 %in% 1:30 ~ "Drove after using cannabis in last month"),
    # Current smoker
    smoker = case_when(
      ._rfsmok3 == 1 ~ "0",
      ._rfsmok3 == 2 ~ "Currently smokes cigarettes"),
    # Current ecig user 
    ecig = case_when(
      ._cureci2 == 1 | ._cureci3 == 1 ~ "0",
      ._cureci2 == 2 | ._cureci3 == 2 ~ "Currently uses e-cigarettes"),
    # Smoking quit attempt last year
    quit_smoke = case_when(
      stopsmk2 == 1 ~ "Made attempt to quit smoking cigarettes in past year",
      stopsmk2 == 2 ~ "0"),
    # No leisure time physical activity or exercise 
    leisure_time = case_when(
      ._totinda == 1 ~ "0",
      ._totinda == 2 ~ "No leisure time physical activity or exercise  in past month")
  ) 

## Obese ----
risk_fact_data_obese <-
  brfss_county %>%
  filter(age >= 20 & datayear %in% 2023:2024) %>%
  mutate(
    # Obese, age 20+ 
    obese_20 = case_when(
      ._bmi5cat %in% 1:3 ~ "0",
      ._bmi5cat == 4 ~ "BMI-defined obesity, adults 20+")) 


# Design objects ----

## 2022 & 2024 ----
risk_fact_22_24_design <- 
  risk_fact_data_22_24 %>%
  as_survey_design(strata = newstrat, 
                   weight =  ._llcpwt, 
                   ids = ._psu, 
                   nest = T)

## 2023 & 2024 ----
risk_fact_23_24_design <- 
  risk_fact_data_23_24 %>%
  as_survey_design(strata = newstrat, 
                   weight =  ._llcpwt, 
                   ids = ._psu, 
                   nest = T)

## Obese ----
risk_fact_obese_design <- 
  risk_fact_data_obese %>%
  as_survey_design(strata = newstrat, 
                   weight =  ._llcpwt, 
                   ids = ._psu, 
                   nest = T)

# List of variables ----
# Note the var names must match the names assigned in data prep. step 
# Variables need to be contained in a list for purrr::map() 
# "county_var" is the identifying the district offices

## 2022 & 2024 ----
risk_fact_22_24_vars <-
  list(
    list(var = "hiv_risk", 
         county_var = "county")
  )

## 2023 & 2024 ----
risk_fact_23_24_vars <-
  list(
    list(var = "any_drink", 
         county_var = "county"),
    list(var = "binge_drink",
         county_var = "county"),
    list(var = "heavy_drink",
         county_var = "county"),
    list(var = "use_mj",
         county_var = "county"),
    list(var = "use_mj_drive",
         county_var = "county"),
    list(var = "smoker",
         county_var = "county"),
    list(var = "ecig",
         county_var = "county"),
    list(var = "quit_smoke",
         county_var = "county"),
    list(var = "leisure_time",
         county_var = "county")
  )

## Obese ----
risk_fact_obese_vars <-
  list(
    list(var = "obese_20",
         county_var = "county")
  )

# Mapping the data  ----

## 2022 & 2024 ----

# State
risk_fact_22_24_result_state <- 
  purrr::map(
    .x = risk_fact_22_24_vars,
    .f = ~ brfss_one_var(
      dat = risk_fact_22_24_design,
      group_var = .x$var
    )
  ) 

# county
risk_fact_22_24_result_county <- 
  purrr::map(
    .x = risk_fact_22_24_vars,
    .f = ~ brfss_two_var(
      dat = risk_fact_22_24_design,
      group_var1 = .x$county_var,
      group_var2 = .x$var
    )
  )

# Convert the State results to dataframe 
risk_fact_22_24_result_state_df <- 
  list_rbind(risk_fact_22_24_result_state) %>%
  mutate(county = "State",
         years = "2022/2024")

# Convert the county results to dataframe 
risk_fact_22_24_result_county_df <- 
  list_rbind(risk_fact_22_24_result_county) %>%
  mutate(years = "2022/2024")



## 2023 & 2024 ----

# State
risk_fact_23_24_result_state <- 
  purrr::map(
    .x = risk_fact_23_24_vars,
    .f = ~ brfss_one_var(
      dat = risk_fact_23_24_design,
      group_var = .x$var
    )
  ) 

# county
risk_fact_23_24_result_county <- 
  purrr::map(
    .x = risk_fact_23_24_vars,
    .f = ~ brfss_two_var(
      dat = risk_fact_23_24_design,
      group_var1 = .x$county_var,
      group_var2 = .x$var
    )
  )

# Convert the State results to dataframe 
risk_fact_23_24_result_state_df <- 
  list_rbind(risk_fact_23_24_result_state) %>%
  mutate(county = "State",
         years = "2023/2024")

# Convert the county results to dataframe 
risk_fact_23_24_result_county_df <- 
  list_rbind(risk_fact_23_24_result_county) %>%
  mutate(years = "2023/2024")


## Obese ----

# State
risk_fact_obese_result_state <- 
  purrr::map(
    .x = risk_fact_obese_vars,
    .f = ~ brfss_one_var(
      dat = risk_fact_obese_design,
      group_var = .x$var
    )
  ) 

# county
risk_fact_obese_result_county <- 
  purrr::map(
    .x = risk_fact_obese_vars,
    .f = ~ brfss_two_var(
      dat = risk_fact_obese_design,
      group_var1 = .x$county_var,
      group_var2 = .x$var
    )
  )

# Convert the State results to dataframe 
risk_fact_obese_result_state_df <- 
  list_rbind(risk_fact_obese_result_state) %>%
  mutate(county = "State",
         years = "2023/2024")

# Convert the county results to dataframe 
risk_fact_obese_result_county_df <- 
  list_rbind(risk_fact_obese_result_county) %>%
  mutate(years = "2023/2024")


# Combine dataframes for preventative behaviors into one
risk_fact_result <-
  rbind(risk_fact_22_24_result_county_df,
        risk_fact_22_24_result_state_df,
        risk_fact_23_24_result_county_df,
        risk_fact_23_24_result_state_df,
        risk_fact_obese_result_state_df,
        risk_fact_obese_result_county_df) %>%
  mutate(group = "risk_fact")












