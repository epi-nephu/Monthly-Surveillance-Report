# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.1 30/04/2025

# Setup and data prep for internal (full) and external (summary) versions of monthly surveillance report

# Version history --------------------------------------------------------------
# v2.1 Addition of avian influenza and Vibrio parahaemolyticus to conditions reference list
# v2.0 Change to a three-year post-COVID HLM lookback period (2022-2024), six year 
#      data extracts for creating bar and ribbon charts (2019-2025), high vs. low 
#      volume conditions definitions, general code refactoring and restructuring
# v1.3 Addition of 2024 historical data
# v1.2 Exclude food-borne or water-borne illness as we are no longer including 
#      these in the report
# v1.1 Addition of VictoriaLinelist3 dataset
# v1.0 Final production version

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               xfun,
               rmarkdown,
               here,
               readxl,
               janitor,
               lubridate,
               scales,
               rmarkdown,
               knitr,
               rlang,
               kableExtra,
               patchwork,
               apyramid,
               sf,
               ggthemes,
               ggtext)


# Colours ----------------------------------------------------------------------
# NEPHU colour palette
nephu_blue   <- "#191d43"
nephu_green  <- "#5bc788"
nephu_grey   <- "#eae5e0"
nephu_peach  <- "#ffa78c"
nephu_white  <- "#ffffff"
nephu_yellow <- "#fff694"

# Months -----------------------------------------------------------------------
# Start and end dates for current reporting month
epimonth_current <- as.Date("2025-09-01")
epimonth_enddate  <-  as.Date("2025-09-30")

filedate_epimonth <- format(epimonth_enddate, format = "%Y%m%d")

reporting_month <- format(epimonth_current, format = "%B %Y")

# Date of PHESS data extract
extract_date <- as.Date("2025-10-03")

# Year to date
month_number <- month(epimonth_current)

epimonth_ytd <- as.character(month(1:month_number, label = TRUE))

# Previous reporting month
epimonth_minus1 <- epimonth_current - months(1)

# Historical limits baseline periods - three-year lookback
hlm_baseline <- epimonth_current + months(-c(11:13, 23:25, 35:37))

# Years ------------------------------------------------------------------------
# Year range for historical data
filedate_ytd <- "2019_2024"

# Current and most recent years
ytd_current <- 2025

ytd_minus1 <- ytd_current - 1

# Three-year lookback period for comparisons
ytd_three_year <- (ytd_current - 3):(ytd_current - 1)

# Date ranges for ribbon charts
ytd_ribbon   <- (ytd_current - 6):ytd_current
ribbon_start <- epimonth_current - months(35)

# High-volume conditions definition --------------------------------------------
high_volume <- 10

# LGA names --------------------------------------------------------------------
lga_name_long <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", 
                    "Hume (C)", "Knox (C)", "Manningham (C)", 
                    "Maroondah (C)", "Nillumbik (S)", "Whitehorse (C)", 
                    "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

lga_name_short <- c("Banyule", "Boroondara", "Darebin", 
                    "Hume", "Knox", "Manningham", 
                    "Maroondah", "Nillumbik", "Whitehorse", 
                    "Whittlesea", "Yarra", "Yarra Ranges")

lga_nudge <- c("Boroondara", "Darebin", "Yarra", "Maroondah")

# Table formatting -------------------------------------------------------------
tables_colnames <- c(" ",
                     "Trend",
                     format(epimonth_current, format = "%b %Y"), 
                     format(epimonth_minus1, format = "%b %Y"), 
                     "Mean",
                     "Comparison",
                     "NEPHU",
                     "Victoria",
                     "Trend",
                     ytd_current, 
                     ytd_minus1,
                     "Mean")

# Load and wrangle case data ---------------------------------------------------
# Read in the NEPHU PHESS case linelist (current year)
cases_allnephu <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHUCaseLinelist_", filedate_epimonth, ".xlsx"),
                                    sheet     = 1,
                                    guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Read in the NEPHU PHESS case linelist (historical)
cases_historical <- readxl::read_xlsx(paste0(here::here(), "/Data", "/NEPHUCaseLinelist_", filedate_ytd, ".xlsx"),
                                      sheet     = 1,
                                      guess_max = min(100000, Inf))

# Join current and historical linelists and restrict to confirmed and/or probable cases residing in NEPHU LGAs
# Exclude AMR conditions as these are sourced from a different dataset
cases_allnephu <- cases_allnephu %>% 
  dplyr::bind_rows(cases_historical) %>% 
  #
  dplyr::filter(most_recent_event_classfication %in% c("Confirmed", "Probable")) %>% 
  dplyr::filter(event_type == "Case") %>%
  dplyr::filter(!condition %in% c("Candida auris",
                                  "Carbapenemase producing acinetobacter",
                                  "Carbapenemase producing enterobacterales",
                                  "Carbapenemase producing pseudomonas",
                                  "VanA Vancomycin resistant enterococcus",
                                  #
                                  "Food-borne or water-borne illness",
                                  #
                                  "Tuberculosis")) %>%
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

# Load and wrangle AMR case data -----------------------------------------------
# Read in the AMR linelist (current year)
cases_amr <- readxl::read_xlsx(paste0(here::here(), "/Data", "/AMRCaseLinelist_", filedate_epimonth, ".xlsx"),
                               sheet     = 1,
                               guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Read in the AMR linelist (historical)
cases_historical_amr <- readxl::read_xlsx(paste0(here::here(), "/Data", "/AMRCaseLinelist_", filedate_ytd, ".xlsx"),
                                          sheet     = 1,
                                          guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Join current and historical linelists and restrict to confirmed and/or suspected cases assigned to NEPHU
cases_amr <- cases_amr %>% 
  dplyr::bind_rows(cases_historical_amr) %>%
  #
  dplyr::filter(most_recent_event_classfication %in% c("Confirmed", "Suspected")) %>% 
  dplyr::filter(event_type == "Case") %>%
  dplyr::filter(local_public_health_unit == "North Eastern") %>% 
  #
  dplyr::filter(phess_id != "320245604761") %>% 
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

# Join AMR data to NEPHU PHESS case data
cases_allnephu <- cases_allnephu %>% 
  dplyr::bind_rows(cases_amr) %>% 
  # Exclude this case - incorrectly entered as unspecified flavivirus in PHESS
  dplyr::filter(phess_id != "320255866141") %>% 
  #
  dplyr::rename(age_years             = age_in_years_from_event_date,
                classification        = most_recent_event_classfication,
                followup_reason       = reason_for_follow_up,
                amr_resistance        = resistance_gene_s,
                travel_country        = country_44,
                amr_country           = select_country_36,
                amr_overseas_facility = did_the_case_visit_a_healthcare_facility_in_this_country,
                amr_acquisition       = what_is_the_most_likely_mode_of_acquisition,
                vital_status          = death_due_to_the_notifiable_condition_answer_disease,
                hospitalised          = presented_to) %>%
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                sero_group_subtype,
                amr_resistance,
                classification,
                event_type,
                followup_reason,
                risk_factors,
                travel_country,
                amr_country,
                amr_overseas_facility,
                amr_acquisition,
                age_years,
                sex,
                indigenous_status,
                country_of_birth,
                local_government_area,
                local_public_health_unit,
                hospitalised,
                health_care_facility,
                vital_status)

# Wrangle case data
cases_allnephu <- cases_allnephu %>%
  dplyr::mutate(event_date      = lubridate::ymd(event_date),
                event_month     = lubridate::month(event_date, label = TRUE, abbr = TRUE),
                event_year      = lubridate::year(event_date),
                event_yearmonth = lubridate::floor_date(event_date, unit = "month")) %>% 
  #
  dplyr::mutate(
    condition = dplyr::case_when(
      organism_cause == "Legionella longbeachae" ~ "Legionella longbeachae",
      condition == "Legionellosis"               ~ "Legionella pneumophila",
      TRUE                                       ~ condition),
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
      TRUE            ~ "Not stated"),
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
      TRUE            ~ NA_character_),
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
      TRUE       ~ as.character(sex)),
      #
      levels = c("Male",
                 "Female",
                 "Other",
                 "Not stated")),
    #
    indigenous_status = factor(dplyr::case_when(
      indigenous_status == "Aboriginal but not Torres Strait Islander origin" ~ "Aboriginal and/or Torres Strait Islander",
      indigenous_status == "Torres Strait Islander but not Aboriginal origin" ~ "Aboriginal and/or Torres Strait Islander",
      indigenous_status == "Aboriginal and Torres Strait Islander origin"     ~ "Aboriginal and/or Torres Strait Islander",
      indigenous_status == "Not Aboriginal or Torres Strait Islander"         ~ "Neither Aboriginal nor Torres Strait Islander",
      TRUE                                                                    ~ "Not stated"),
      #
      levels = c("Aboriginal and/or Torres Strait Islander",
                 "Neither Aboriginal nor Torres Strait Islander",
                 "Not stated")),
    #
    hospital_admission = factor(dplyr::case_when(
      stringr::str_detect(hospitalised, "Hospital admission") ~ "Yes",
      TRUE                                                    ~ "No"),
      #
      levels = c("Yes",
                 "No")),
    #
    emergency_presentation = factor(dplyr::case_when(
      stringr::str_detect(hospitalised, "Hospital emergency") ~ "Yes",
      TRUE                                                    ~ "No"),
      #
      levels = c("Yes",
                 "No")),
    #
    vital_status = factor(dplyr::case_when(
      is.na(vital_status)       ~ "Not stated",
      vital_status == "Unknown" ~ "Not stated",
      TRUE                      ~ as.character(vital_status))),
    #
    local_government_area = factor(local_government_area,
                                   levels = lga_name_long))

# Load and wrangle statewide PHESS case linelist (current month) ---------------
cases_vic_1 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/VictoriaLinelist1_", filedate_epimonth, ".xlsx"),
                                 sheet     = 1,
                                 guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

cases_vic_2 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/VictoriaLinelist2_", filedate_epimonth, ".xlsx"),
                                 sheet     = 1,
                                 guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

cases_vic_3 <- readxl::read_xlsx(paste0(here::here(), "/Data", "/VictoriaLinelist3_", filedate_epimonth, ".xlsx"),
                                 sheet     = 1,
                                 guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Join statewide PHESS case data and restrict to confirmed and/or probable cases
cases_allvic <- cases_vic_1 %>% 
  dplyr::bind_rows(cases_vic_2) %>%
  dplyr::bind_rows(cases_vic_3) %>% 
  #
  dplyr::filter(most_recent_event_classfication %in% c("Confirmed", "Probable")) %>% 
  dplyr::filter(event_type == "Case") %>% 
  dplyr::filter(!condition %in% c("Food-borne or water-borne illness",
                                  "Tuberculosis")) %>% 
  #
  dplyr::distinct(phess_id, .keep_all = TRUE) %>% 
  #
  dplyr::mutate(condition = dplyr::case_when(
    organism_cause == "Legionella longbeachae" ~ "Legionella longbeachae",
    condition == "Legionellosis"               ~ "Legionella pneumophila",
    TRUE                                       ~ condition)) %>% 
  #
  dplyr::select(phess_id,
                event_date,
                condition,
                organism_cause,
                event_type,
                most_recent_event_classfication,
                age_in_years_from_event_date,
                sex,
                indigenous_status,
                local_government_area,
                local_public_health_unit)

# Load the conditions reference list -------------------------------------------
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

# Join conditions reference list to NEPHU case linelist
cases_allnephu <- cases_allnephu %>% 
  dplyr::left_join(reference_conditions, by = "condition", relationship = "many-to-many") %>% 
  #
  dplyr::filter(!is.na(condition_label))

# Join conditions reference list to statewide case linelist
cases_allvic <- cases_allvic %>% 
  dplyr::left_join(reference_conditions, by = "condition", relationship = "many-to-many") %>% 
  #
  dplyr::filter(!is.na(condition_label))

# Load and wrangle population data ---------------------------------------------
# Read in population data
reference_population <- readxl::read_xlsx(paste0(here::here(), "/Data", "/Reference", "/LGAAllocationLong.xlsx"),
                                          sheet     = 1,
                                          guess_max = min(100000, Inf)) %>% 
  janitor::clean_names()

# Calculate NEPHU and statewide totals
population_nephu <- reference_population %>% 
  dplyr::filter(lphu == "NEPHU") %>%
  #
  dplyr::summarise(x2021_population = sum(x2021_population)) %>% 
  #
  dplyr::mutate(lga = "Total NEPHU")

population_vic <- reference_population %>% 
  dplyr::summarise(x2021_population = sum(x2021_population)) %>% 
  #
  dplyr::mutate(lga = "Total Victoria")

reference_population <- reference_population %>% 
  dplyr::filter(lphu == "NEPHU") %>% 
  #
  dplyr::select(-lphu) %>% 
  #
  dplyr::bind_rows(population_nephu) %>% 
  dplyr::bind_rows(population_vic)

# Calculate NEPHU and Victorian populations at risk for a single calendar month
population_nephu_month <- reference_population$x2021_population[13] / 12

population_vic_month <- reference_population$x2021_population[14] / 12

# Load and wrangle shape file for maps -----------------------------------------
nephu_lga.sf <- sf::st_read(here::here("Data/LGA_2022_AUST_GDA2020_SHP/LGA_2022_AUST_GDA2020.shp"),
                            quiet = TRUE) %>% 
  dplyr::select(-STE_CODE21, 
                -AUS_CODE21, 
                -AUS_NAME21, 
                -LOCI_URI21) %>% 
  #
  dplyr::rename(lga_code   = LGA_CODE22,
                lga_name   = LGA_NAME22,
                ste_name   = STE_NAME21,
                areasqkm   = AREASQKM,
                shp_length = SHAPE_Leng, 
                shp_area   = SHAPE_Area) %>% 
  #
  dplyr::filter(lga_name %in% lga_name_short) %>% 
  #
  dplyr::arrange(lga_name) %>% 
  #
  dplyr::mutate(lga = c("Banyule (C)", "Boroondara (C)", "Darebin (C)", 
                        "Hume (C)", "Knox (C)", "Manningham (C)", 
                        "Maroondah (C)", "Nillumbik (S)", "Whitehorse (C)", 
                        "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)"))

# Tidy up environment ----------------------------------------------------------
rm(cases_historical, cases_amr, cases_historical_amr, 
   cases_vic_1, cases_vic_2, cases_vic_3,
   population_nephu, population_vic)

