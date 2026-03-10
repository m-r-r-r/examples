
# Prepared by M. Rhodes-Reese


# The functions below run BRFSS analysis
# brfss_one_var only allows for one grouping variable like when running single state values (eg one indicator)
# brfss_two_var only allows for the grouping of two variables like performing demographic or county-level analysis (eg geographic breakdown and indicator)





# BRFSS One Variable

# dat -> dataset ; in this case it would be your design object
# group_var -> ; is the list of variables to be passed through for analysis (eg indicator)


brfss_one_var <- function(dat, group_var) {
  
  group_var_sym <- sym(group_var)  # create grouping variable to be mapped
  
  dat %>%  # design object (survery/srvyr package)
    
    filter(!is.na(!!group_var_sym)) %>% # remove any NAs prior to analysis 
     
    group_by(!!group_var_sym) %>% # grouping variable
    
    summarize((
      percent =  # percent
        survey_mean(
          vartype = c("ci", "cv"),
          na.rm = TRUE)) * 100,
      n = n(),   # numerator
      pop_estimate = survey_total()) %>%  # total population estimate (weighted)
    
    na.omit() %>%  # remove any NAs that were created above
    
    filter(!!group_var_sym != "0") %>%  # remove any observations that are not of interest (eg percent of people who don't binge drink when looking at percent of adults who do)
    
    rename("lower_CI" = "_low", # rename variables
           "upper_CI" = "_upp",
           "percent" = "coef",
           "RSE" = "_cv",
           "variable" = !!group_var_sym) %>%
    
    mutate(across(c("percent", "RSE"), round, 0),  # round percent and RSE error
           across(c("lower_CI", "upper_CI"), round, 3),  # round lower and upper confidence interval to three decimals places
           across("pop_estimate", round, -3),  # round populate estimate to nearest thousand
           RSE_Flag = if_else(RSE >= 30, "RSE High" , NA)  # create an RSE suppression flag for any RSE greater or equal to 30
    ) %>%
    
    select((-pop_estimate_se)). # remove pop_estimate_se from output
  
}












# BRFSS Two Variables

# dat -> dataset ; in this case it would be your design object
# group_var1 &  group_var2 -> ; the variables to be passed through for analysis from list 
# order does matter when assigning variables to group_var1 & group_var2 - assign accordingly

brfss_two_var <- function(dat, group_var1, group_var2) {
  
  
  group_var_sym1 <- sym(group_var1)  # create grouping variable 1 to be mapped
  group_var_sym2 <- sym(group_var2). # create grouping variable 2 to be mapped
  
  
  dat %>%.  # design object (survery/srvyr package)
    
    filter(!is.na(!!group_var_sym2)) %>%  # remove any NAs prior to analysis 
    
    group_by(!!group_var_sym1, !!group_var_sym2) %>%. # grouping variables
    
    summarize((
      percent =  # percent
        survey_mean(
          vartype = c("ci", "cv"),
          na.rm = TRUE)) * 100,
      n = n(),   # numerator
      pop_estimate = survey_total()) %>%  # total population estimate (weighted)
    
    na.omit() %>%  # remove any NAs that were created above
      
    filter(!!group_var_sym2 != "0") %>%. # remove any observations that are not of interest (eg percent of people who don't binge drink when looking at percent of adults who do)
    
                                 # rename variables
    rename("Lower_CI" = "_low",  # lower confidence interval
           "Upper_CI" = "_upp",  # upper confidence interval
           "Percent" = "coef",  # percent
           "RSE" = "_cv",  # relative standard error
           "Variable" = !!group_var_sym2) %>% # rename variable to grouping variable name
    
      mutate(across(c("percent", "RSE"), round, 0),  # round percent and RSE error
             across(c("lower_CI", "upper_CI"), round, 3),  # round lower and upper confidence interval to three decimals places
             across("pop_estimate", round, -3),  # round populate estimate to nearest thousand
             RSE_Flag = if_else(RSE >= 30, "RSE High" , NA)  # create an RSE suppression flag for any RSE greater or equal to 30
      ) %>%
    
    select((-Pop_Estimate_se)). # remove pop_estimate_se from output
  
  
}





