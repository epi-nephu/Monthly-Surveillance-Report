# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 3.0 08/10/2025

# Setup for internal (full) and external (summary) versions of monthly surveillance report

################################################################################
# Packages
################################################################################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               odbc,
               DBI,
               glue,
               writexl,
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

################################################################################
# Colours
################################################################################
# NEPHU colour palette
nephu_blue   <- "#191d43"
nephu_green  <- "#5bc788"
nephu_grey   <- "#eae5e0"
nephu_peach  <- "#ffa78c"
nephu_white  <- "#ffffff"
nephu_yellow <- "#fff694"

################################################################################
# Definitions and constants
################################################################################
# Start and end dates for current reporting month
epimonth_current <- as.Date("2025-09-01")
epimonth_enddate  <-  as.Date("2025-09-30")

reporting_month <- format(epimonth_current, format = "%B %Y")

# Date of PHAR data extract
extract_date <- as.Date("2025-10-08")

# Year to date
month_number <- month(epimonth_current)

epimonth_ytd <- as.character(month(1:month_number, label = TRUE))

# Previous reporting month
epimonth_minus1 <- epimonth_current - months(1)

# Historical limits baseline periods - three-year lookback
hlm_baseline <- epimonth_current + months(-c(11:13, 23:25, 35:37))

# Start date for PHAR data extracts (3 years prior to the start of the HLM baseline period)
lookback_start <- hlm_baseline[9] - years(3) - months(1)

# Current and most recent years
ytd_current <- 2025

ytd_minus1 <- ytd_current - 1

# Three-year lookback period for comparisons
ytd_three_year <- (ytd_current - 3):(ytd_current - 1)

# Date ranges for ribbon charts
ytd_ribbon   <- (ytd_current - 6):ytd_current
ribbon_start <- epimonth_current - months(35)

# High-volume conditions
high_volume <- 10

# LGA names
lga_name_long <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", 
                    "Hume (C)", "Knox (C)", "Manningham (C)", 
                    "Maroondah (C)", "Nillumbik (S)", "Whitehorse (C)", 
                    "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

lga_name_sql <- glue::glue_collapse(glue::glue("'{lga_name_long}'"), sep = ", ")

lga_name_short <- c("Banyule", "Boroondara", "Darebin", 
                    "Hume", "Knox", "Manningham", 
                    "Maroondah", "Nillumbik", "Whitehorse", 
                    "Whittlesea", "Yarra", "Yarra Ranges")

lga_nudge <- c("Boroondara", "Darebin", "Yarra", "Maroondah")

################################################################################
# Table formatting
################################################################################
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

################################################################################
# Read in conditions reference list
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
# Load and wrangle population data
################################################################################
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

################################################################################
# Load and wrangle shape file for maps
################################################################################
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

