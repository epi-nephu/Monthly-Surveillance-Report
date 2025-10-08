# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 3.0 08/10/2025

# PHAR data extraction for internal (full) and external (summary) versions of monthly surveillance report

################################################################################
# Run setup script
################################################################################
source(paste0(here::here(), "/Code", "/01_data_prep_monthly_setup.R"))

################################################################################
# Connect to PHAR and extract data 
################################################################################
# Connect to PHAR
# Note: useProxy = 1 when in Austin offices, useProxy = 0 when WFH
con <- DBI::dbConnect(odbc::odbc(), "PHAR", useProxy = 0)

# NEPHU cases for entire lookback period
phar_nephu <- DBI::dbGetQuery(con,
                              glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                          WHERE EVENT_DATE >= DATE '{lookback_start}'
                                          AND EVENT_DATE <= DATE '{epimonth_enddate}'
                                          AND (LGA IN ({lga_name_sql})
                                               OR ASSIGNED_LPHU = 'North Eastern')")) %>% 
  janitor::clean_names()

# Risk factor information
phar_risk <- DBI::dbGetQuery(con,
                             glue::glue("SELECT * FROM dh_public_health.phess_release.caseexposures")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::select(event_id,
                travel_overseas = risk_factors_travel_overseas_country_specify,
                travel_interstate = risk_factors_travel_within_australia_state)

# VIC cases for reporting month
phar_vic <- DBI::dbGetQuery(con,
                            glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                        WHERE EVENT_DATE >= DATE '{epimonth_current}'
                                        AND EVENT_DATE <= DATE '{epimonth_enddate}'")) %>% 
  janitor::clean_names()

# Disconnect from PHAR
DBI::dbDisconnect(con)

################################################################################
# Load the conditions reference list
################################################################################
reference_conditions <- readxl::read_xlsx(paste0(here::here(), "/Data", "/Reference", "/ConditionsReferenceList.xlsx"),
                                          sheet     = 1,
                                          guess_max = min(100000, Inf)) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::mutate(date_made_notifiable = lubridate::ymd(date_made_notifiable),
                #
                one_years_data = dplyr::case_when(epimonth_enddate > date_made_notifiable + years(1) ~ "Yes",
                                                  TRUE ~ "No"),
                #
                two_years_data = dplyr::case_when(epimonth_enddate > date_made_notifiable + years(2) ~ "Yes",
                                                  TRUE ~ "No"),
                #
                three_years_data = dplyr::case_when(epimonth_enddate > date_made_notifiable + years(3) ~ "Yes",
                                                    TRUE ~ "No"))

################################################################################
# Data wrangling - NEPHU cases
################################################################################
# Join conditions reference list and risk factor information to NEPHU cases
# Separate legionellosis into longbeachae vs. pneumophila (and others)
cases_allnephu <- phar_nephu %>% 
  dplyr::mutate(
    condition = dplyr::case_when(
      organism_cause == "Legionella longbeachae" ~ "Legionella longbeachae",
      condition == "Legionellosis"               ~ "Legionella pneumophila",
      TRUE                                       ~ condition)) %>%
  #
  dplyr::left_join(phar_risk, by = "event_id") %>% 
  #
  dplyr::distinct(event_id, .keep_all = TRUE) %>% 
  #
  dplyr::left_join(reference_conditions, by = "condition", relationship = "many-to-many") %>% 
  #
  dplyr::filter(!is.na(condition_label))

# Restrict to confirmed and/or probable cases
# Exclude AMR conditions that were not assigned to NEPHU
# Exclude interstate case incorrectly entered as NEPHU resident
# Exclude RRV case incorrectly entered as unspecified flavivirus
cases_allnephu <- cases_allnephu %>% 
  dplyr::filter(event_type == "Case") %>% 
  dplyr::filter(event_classification %in% c("Confirmed", "Probable")) %>% 
  dplyr::filter(!(reporting_group == "Antimicrobial Resistant Organisms" & assigned_lphu != "North Eastern")) %>%
  dplyr::filter(event_id != "320245604761") %>% 
  dplyr::filter(event_id != "320255866141")

# Wrangle case data
cases_allnephu <- cases_allnephu %>%
  dplyr::mutate(
    event_date      = lubridate::ymd(event_date),
    event_month     = lubridate::month(event_date, label = TRUE, abbr = TRUE),
    event_year      = lubridate::year(event_date),
    event_yearmonth = lubridate::floor_date(event_date, unit = "month"),
    #
    age_years = dplyr::na_if(age_years, 999),
    #
    age_10year = factor(dplyr::case_when(
      age_years < 10  ~ "00-09 years",
      age_years < 20  ~ "10-19 years",
      age_years < 30  ~ "20-29 years",
      age_years < 40  ~ "30-39 years",
      age_years < 50  ~ "40-49 years",
      age_years < 60  ~ "50-59 years",
      age_years < 70  ~ "60-69 years",
      age_years < 80  ~ "70-79 years",
      age_years < 90  ~ "80-89 years",
      age_years >= 90 ~ "90+ years",
      TRUE ~ "Not stated"),
      #
      levels = c("00-09 years",
                 "10-19 years",
                 "20-29 years",
                 "30-39 years",
                 "40-49 years",
                 "50-59 years",
                 "60-69 years",
                 "70-79 years",
                 "80-89 years",
                 "90+ years",
                 "Not stated")),
    #
    age_group = factor(dplyr::case_when(
      age_years < 5   ~ "Less than 05 years",
      age_years < 15  ~ "05-14 years",
      age_years < 25  ~ "15-24 years",
      age_years < 45  ~ "25-44 years",
      age_years < 65  ~ "45-64 years",
      age_years >= 65 ~ "65+ years",
      TRUE ~ NA_character_),
      #
      levels = c("Less than 05 years",
                 "05-14 years",
                 "15-24 years",
                 "25-44 years",
                 "45-64 years",
                 "65+ years")),
    #
    sex = factor(dplyr::case_when(
      is.na(sex) ~ "Not stated",
      TRUE ~ as.character(sex)),
      #
      levels = c("Male",
                 "Female",
                 "Other",
                 "Not stated")),
    #
    local_government_area = factor(lga,
                                   levels = lga_name_long),
    #
    suburb = stringr::str_to_title(city))

cases_allnephu <- cases_allnephu %>% 
  dplyr::select(phess_id = event_id,
                event_date,
                event_month,
                event_year,
                event_yearmonth,
                condition,
                organism_cause,
                classification = event_classification,
                age_years,
                age_10year,
                age_group,
                sex,
                indigenous_status,
                country_of_birth,
                vital_status = died_from_disease,
                local_government_area,
                suburb,
                assigned_lphu,
                travel_overseas,
                travel_interstate,
                condition_label,
                chart_label,
                text_label,
                condition_group,
                reporting_group,
                one_years_data,
                two_years_data,
                three_years_data)

################################################################################
# Data wrangling - VIC cases
################################################################################
# Join conditions reference list to VIC cases
# Separate legionellosis into longbeachae vs. pneumophila (and others)
cases_allvic <- phar_vic %>% 
  dplyr::mutate(
    condition = dplyr::case_when(
      organism_cause == "Legionella longbeachae" ~ "Legionella longbeachae",
      condition == "Legionellosis"               ~ "Legionella pneumophila",
      TRUE                                       ~ condition)) %>%
  #
  dplyr::distinct(event_id, .keep_all = TRUE) %>% 
  #
  dplyr::left_join(reference_conditions, by = "condition", relationship = "many-to-many") %>% 
  #
  dplyr::filter(!is.na(condition_label))

# Restrict to confirmed and/or probable cases
# Exclude interstate case incorrectly entered as NEPHU resident
# Exclude RRV case incorrectly entered as unspecified flavivirus
cases_allvic <- cases_allvic %>% 
  dplyr::filter(event_type == "Case") %>% 
  dplyr::filter(event_classification %in% c("Confirmed", "Probable")) %>% 
  dplyr::filter(event_id != "320245604761") %>% 
  dplyr::filter(event_id != "320255866141")

# Wrangle case data
cases_allvic <- cases_allvic %>%
  dplyr::mutate(
    event_date      = lubridate::ymd(event_date),
    event_month     = lubridate::month(event_date, label = TRUE, abbr = TRUE),
    event_year      = lubridate::year(event_date),
    event_yearmonth = lubridate::floor_date(event_date, unit = "month"))

cases_allvic <- cases_allvic %>% 
  dplyr::select(phess_id = event_id,
                event_date,
                event_month,
                event_year,
                event_yearmonth,
                condition,
                organism_cause,
                classification = event_classification,
                lga,
                assigned_lphu,
                condition_label,
                chart_label,
                text_label,
                condition_group,
                reporting_group,
                one_years_data,
                two_years_data,
                three_years_data)

################################################################################
# Save data extracts for further analyses
################################################################################
writexl::write_xlsx(cases_allnephu,
                    paste0(here::here(), "/Data", "/NEPHU_Cases_", format(epimonth_enddate, format = "%Y%m%d"), ".xlsx"))

writexl::write_xlsx(cases_allvic,
                    paste0(here::here(), "/Data", "/VIC_Cases_", format(epimonth_enddate, format = "%Y%m%d"), ".xlsx"))

