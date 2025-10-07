# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.0 13/01/2025

# Join PHESS extracts for each year to create historical case dataset

# Version history --------------------------------------------------------------
# v2.0 Change to a three-year post-COVID HLM lookback period (2022-2024), six year 
#      data extracts for creating bar and ribbon charts (2019-2025), high vs. low 
#      volume conditions definitions, general code refactoring and restructuring
# v1.3 Addition of 2024 historical data
# v1.2 Exclude food-borne or water-borne illness as we are no longer including 
#      these in the report
# v1.1 Addition of VictoriaLinelist3 dataset
# v1.0 Final production version

# Setup ------------------------------------------------------------------------
library(pacman)

pacman::p_load(tidyverse,
               readxl,
               janitor,
               writexl)

# 2019 -------------------------------------------------------------------------
nephu_2019 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists",
                                       "/NEPHUCaseLinelist_2019.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# 2020 -------------------------------------------------------------------------
nephu_2020 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists",
                                       "/NEPHUCaseLinelist_2020.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# 2021 -------------------------------------------------------------------------
nephu_2021 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists",
                                       "/NEPHUCaseLinelist_2021.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# 2022 -------------------------------------------------------------------------
nephu_2022 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists",
                                       "/NEPHUCaseLinelist_2022.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# 2023 -------------------------------------------------------------------------
nephu_2023 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists", 
                                       "/NEPHUCaseLinelist_2023.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# 2024 -------------------------------------------------------------------------
nephu_2024 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHU Annual Linelists", 
                                       "/NEPHUCaseLinelist_2024.xlsx"),
                                sheet     = 1,
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names() %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                most_recent_event_classfication,
                event_type,
                risk_factors,
                primary_exposure,
                country_44,
                case_found_by,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                presented_to,
                death_due_to_the_notifiable_condition_answer_disease)

# Join datasets ----------------------------------------------------------------
nephu_2019_2024 <- nephu_2019 %>%
  dplyr::bind_rows(nephu_2020) %>% 
  dplyr::bind_rows(nephu_2021) %>% 
  dplyr::bind_rows(nephu_2022) %>% 
  dplyr::bind_rows(nephu_2023) %>% 
  dplyr::bind_rows(nephu_2024) %>% 
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

# Save dataset for further analyses --------------------------------------------
writexl::write_xlsx(nephu_2019_2024, paste0(here::here(), "/Data", "/NEPHUCaseLinelist_2019_2024.xlsx"))

