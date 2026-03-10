---
  title: "`{r} params$do` 2024 BRFSS District Profile – Adult Health"
subtitle: "February 2026"
format: 
  docx:
  reference-doc: county_profile_template.docx
execute:
  echo: false
warning: false
params:
  do: ""
---
  
For more information on the indicators below refer to the 2024 BRFSS Annual Report and see the Methodology at the end of this document for Definitions and Data Notes.


```{r data_prep, echo=FALSE}

library(tidyverse)
library(gt)
library(gtExtras)
library(extrafontdb)
library(quarto)



# Read in data from county_profile_prep.R
county_result_raw <- read.csv("county_profile_df.csv", header = TRUE)


# Create new column with state values
state_result <-
  county_result_raw %>%
  filter(do == "State") %>%
  pivot_wider(values_from = Percent, names_from = do) %>%
  select(Variable, 
         State)

county_result <-
  county_result_raw %>%
  filter(do == params$do) %>%
  select(do,
         Variable, 
         Percent, 
         Pop_Estimate,
         years, 
         group)


# Combine transformed state result and rename columns for final output
county_result_tidy <-
  county_result %>%
  right_join(state_result) %>%
  rename("Vermont Most Recent %" = "State",
         "District Most Recent %" = "Percent",
         "Estimated # of Adults in District" = "Pop_Estimate",
         "Indicator" = "Variable",
         "Data Years" = "years")


```
## Risk Factor
```{r risk_fact, echo=FALSE}

# Create risk factor subset
risk_fact_result <-
  county_result_tidy %>%
  filter(group == "risk_fact" & do == params$do) %>%
  select("Indicator",
         "District Most Recent %",
         "Vermont Most Recent %",
         "Estimated # of Adults in District",
         "Data Years") 


# Formatted table
risk_fact_table <-
  
  risk_fact_result %>%
  
  gt() %>%
  
  fmt_number(columns = "Estimated # of Adults in District", decimals = 0) %>%
  
  opt_table_font(
    font = list(
      google_font(
        name = "Franklin Gothic Book"),
      "sans-serif")) %>%
  
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2") %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_body()) %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_title()) %>%
  
  tab_style(
    style = cell_text(font = "Franklin Gothic Book"),
    locations = cells_body(columns = everything())) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#0071a1"),
      cell_text(color = "white", font = "Franklin Gothic Book", v_align = "middle")),
    locations = cells_column_labels(columns = everything())) %>%
  
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "center", weight = "bold"), 
    locations = cells_column_labels(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_body(columns = "Indicator")) %>%
  
  tab_style(
    style = cell_fill(color = '#F2F2F2'),
    locations = cells_body(rows = seq(1, nrow(risk_fact_result), 2))) 

risk_fact_table

```


## Preventative Behaviors
```{r prevent_behav, echo=FALSE}

# Preventative behavior subset ----
prevent_behav_result <-
  county_result_tidy %>%
  filter(group == "prevent_behav" & do == params$do) %>%
  select("Indicator",
         "District Most Recent %",
         "Vermont Most Recent %",
         "Estimated # of Adults in District",
         "Data Years") 


# Preventative behavior formatted table ----
prevent_behav_table <-
  
  prevent_behav_result %>%
  
  gt() %>%
  
  fmt_number(columns = "Estimated # of Adults in District", decimals = 0) %>%
  
  opt_table_font(
    font = list(
      google_font(
        name = "Franklin Gothic Book"),
      "sans-serif")) %>%
  
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2") %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_body()) %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_title()) %>%
  
  tab_style(
    style = cell_text(font = "Franklin Gothic Book"),
    locations = cells_body(columns = everything())) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#0071a1"),
      cell_text(color = "white", font = "Franklin Gothic Book", v_align = "middle")),
    locations = cells_column_labels(columns = everything())) %>%
  
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "center", weight = "bold"), 
    locations = cells_column_labels(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_body(columns = "Indicator")) %>%
  
  tab_style(
    style = cell_fill(color = '#F2F2F2'),
    locations = cells_body(rows = seq(1, nrow(prevent_behav_result), 2))) 

prevent_behav_table


```


## SDoH
```{r sdoh, echo=FALSE}

# SDoH subset ----
sdoh_result <-
  county_result_tidy %>%
  filter(group == "sdoh" & do == params$do) %>%
  select("Indicator",
         "District Most Recent %",
         "Vermont Most Recent %",
         "Estimated # of Adults in District",
         "Data Years") 

# SDoH formatted table ----
sdoh_table <-
  
  sdoh_result %>%
  
  gt() %>%
  
  fmt_number(columns = "Estimated # of Adults in District", decimals = 0) %>%
  
  opt_table_font(
    font = list(
      google_font(
        name = "Franklin Gothic Book"),
      "sans-serif")) %>%
  
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2") %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_body()) %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_title()) %>%
  
  tab_style(
    style = cell_text(font = "Franklin Gothic Book"),
    locations = cells_body(columns = everything())) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#0071a1"),
      cell_text(color = "white", font = "Franklin Gothic Book", v_align = "middle")),
    locations = cells_column_labels(columns = everything())) %>%
  
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "center", weight = "bold"), 
    locations = cells_column_labels(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_body(columns = "Indicator")) %>%
  
  tab_style(
    style = cell_fill(color = '#F2F2F2'),
    locations = cells_body(rows = seq(1, nrow(sdoh_result), 2))) 

sdoh_table


```


## Chronic Conditions
```{r chronic_cond, echo=FALSE}

# Chronic conditions subset ----
chronic_cond_result <-
  county_result_tidy %>%
  filter(group == "chronic_cond" & do == params$do) %>%
  select("Indicator",
         "District Most Recent %",
         "Vermont Most Recent %",
         "Estimated # of Adults in District",
         "Data Years") 

# Chronic conditions formatted table ----
chronic_cond_table <-
  
  chronic_cond_result %>%
  
  gt() %>%
  
  fmt_number(columns = "Estimated # of Adults in District", decimals = 0) %>%
  
  opt_table_font(
    font = list(
      google_font(
        name = "Franklin Gothic Book"),
      "sans-serif")) %>%
  
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2") %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_body()) %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_title()) %>%
  
  tab_style(
    style = cell_text(font = "Franklin Gothic Book"),
    locations = cells_body(columns = everything())) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#0071a1"),
      cell_text(color = "white", font = "Franklin Gothic Book", v_align = "middle")),
    locations = cells_column_labels(columns = everything())) %>%
  
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "center", weight = "bold"), 
    locations = cells_column_labels(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_body(columns = "Indicator")) %>%
  
  tab_style(
    style = cell_fill(color = '#F2F2F2'),
    locations = cells_body(rows = seq(1, nrow(chronic_cond_result), 2))) 

chronic_cond_table

```


## Demographics
```{r demo, echo=FALSE}

# Demographics subset ----
demo_result <-
  county_result_tidy %>%
  filter(group == "demo" & do == params$do) %>%
  select("Indicator",
         "District Most Recent %",
         "Vermont Most Recent %",
         "Estimated # of Adults in District",
         "Data Years") 

# Demographics formatted table ----
demo_table <-
  
  demo_result %>%
  
  gt() %>%
  
  fmt_number(columns = "Estimated # of Adults in District", decimals = 0) %>%
  
  opt_table_font(
    font = list(
      google_font(
        name = "Franklin Gothic Book"),
      "sans-serif")) %>%
  
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2") %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_body()) %>%
  
  tab_style(
    style = cell_borders(
      color = "#f2f2f2",
      weight = px(1),
      style = "solid"),
    locations = cells_title()) %>%
  
  tab_style(
    style = cell_text(font = "Franklin Gothic Book"),
    locations = cells_body(columns = everything())) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#0071a1"),
      cell_text(color = "white", font = "Franklin Gothic Book", v_align = "middle")),
    locations = cells_column_labels(columns = everything())) %>%
  
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "center", weight = "bold"), 
    locations = cells_column_labels(columns = c("Vermont Most Recent %", "District Most Recent %", "Estimated # of Adults in District", "Data Years"))) %>%
  
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_body(columns = "Indicator")) %>%
  
  tab_style(
    style = cell_fill(color = '#F2F2F2'),
    locations = cells_body(rows = seq(1, nrow(demo_result), 2))) 

demo_table

```