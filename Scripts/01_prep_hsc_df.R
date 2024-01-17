# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  HSC index - diff in diff data prep
# REF ID:   c7a87f10 
# LICENSE:  MIT
# DATE:     2024-01-17
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "c7a87f10"

# IMPORT ------------------------------------------------------------------
  
  #read in HSC data
  df <- read_sheet("1s_0_Wz-E-Kxx2wWrtu7VTkhot6U0O7bRKEOwxfqaz4M") %>% 
    janitor::clean_names() %>% 
    select(-c(rmnch_id_2, loggdppc,logpop, time, regionid, logpop_time, interpolated_loggdp_per_capita, pepfar, pepfar_fund,
              time_per, gov_exper_on_health, external_health_expindture))
  
  #country name mapping forf WB income groups
  wb_income_groups <- read_sheet("1tQNqPsgekfty--oA5va4PQCFMrZBVro4fGjFk8zaa70", sheet = 2) %>% 
    select(c(1:6))
  
  #read in PEPFAR budgets
  df_budget <- read_sheet("1_sNMS0sTTL8fh5uQuEdXvTD7XTMRIDF8vVnHJLYcbo0", skip =2)
    
  # Remove "Sum of " from variable names
  new_variable_names <- str_remove_all(names(df_budget), "Sum of ")
  
  # Rename the data frame columns
  names(df_budget) <- new_variable_names
  
  
  #read in domestic health exp
 df_dom_exp <- read_sheet("1djoqHkISLj-FSzLqetV42rQfvyz9RJecvv8JS188YvE") %>% 
    janitor::clean_names() %>% 
    select(indicator, location, spatial_dim_value_code, period, value) %>% 
   mutate(indicator = "gghed")
 
 #read in external health exp
 df_ext_exp <- read_sheet("1trX4p3bvHIYgAIF560cOgcrqScYS9dleb1JxUJfIA0w") %>% 
   janitor::clean_names() %>% 
   select(indicator, location, spatial_dim_value_code, period, value) %>% 
   mutate(indicator = "ext_health_exp")
    

# MUNGE -------------------------------------------------------------------

  #grab pepfar country list to join
 pepfar_list <- pepfar_country_list %>% 
    count(country_iso, country) %>% 
    mutate(pepfar = 1) %>% 
    select(-n)
  
  #pivot budget columns and filter to what we need
  df_budget_tidy <- df_budget %>%
    select(-`...22`) %>% 
    rename(country = Country) %>% 
    pivot_longer(cols = 2:21, names_to = "fiscal_year") %>% 
    filter(fiscal_year %in% c("2005", "2010", "2015", "2017", "2019", "2021"),
           !str_detect(country, "Region"),
           country != "Grand Total") %>% 
    rename(pepfar_budget = value) 
  
  #join expenditure data
  df_all_exp <- df_ext_exp %>% 
    rbind(df_dom_exp) %>% 
    rename(country = location,
           country_iso = spatial_dim_value_code,
           year = period) %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(year = as.character(year))
  
#add PEPFAR logical  (1= PEPFAR, 0 = non-PEPFAR)
#add time period logical (0 = pre 2003, 1 = post 2023)
#join budget data: zeroes mean vals are missing in spreadsheet, NA means we have no data on it (anything before 2005)
#join new WB 2022 income groups
#join domestic health exp and external health exp
 df_join <- df %>% 
  mutate(country = ifelse(country == "eSwatini", "Eswatini", country)) %>% 
  left_join(pepfar_list, by = c("country", "ccode" = "country_iso")) %>% 
  mutate(pepfar = ifelse(is.na(pepfar), 0, pepfar)) %>% 
  mutate(time_period = ifelse(year == 2000, 0,1)) %>% 
  mutate(year = as.character(year)) %>% 
  left_join(df_budget_tidy, by = c("country", "year" = "fiscal_year")) %>% 
  mutate(country = case_when(country == "Congo (Brazzaville)" ~ "Republic of Congo",
                             TRUE ~ country)) %>% 
   tidylog::left_join(wb_income_groups, by = c("country" = "hsc_country", "ccode" = "hsc_iso")) %>% 
  select(-c(name_match, wb_country, wb_iso)) %>% 
  rename(wb_2022_income_group = wb_2024_income_group,
         wb_2021_income_group = income) %>% 
   tidylog::left_join(df_all_exp %>% select(-country), by = c("ccode" = "country_iso", "year"))

 write_csv(df_join, "Dataout/hsc_data_final.csv")

