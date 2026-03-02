# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 4.0 04/02/2025

# Code to prepare data for and format summary tables

# Current and previous month counts --------------------------------------------
f_table_dataprep_count <- function(data, conditions) {
  
  data_count <- data %>%
    dplyr::mutate(condition_label = factor(condition_label,
                                           levels = conditions)) %>%
    #
    dplyr::group_by(condition_label) %>%
    #
    dplyr::summarise(
      n_total_baseline = sum(event_yearmonth %in% c(epimonth_current, epimonth_minus1, hlm_baseline)),
      n_month_current  = sum(event_yearmonth == epimonth_current),
      n_month_minus1   = sum(event_yearmonth == epimonth_minus1)) %>% 
    #
    dplyr::ungroup() %>%
    #
    tidyr::complete(condition_label,
                    fill = list(n_month_current  = 0,
                                n_month_minus1   = 0,
                                n_total_baseline = 0)) %>%
    #
    dplyr::mutate(trend_month = dplyr::case_when(n_month_current > n_month_minus1 * 1.1 ~ "↑",
                                                 n_month_current < n_month_minus1 * 0.9 ~ "↓",
                                                 TRUE ~ "--"))
    # Alternate option if up/down arrows not displaying correctly
    # dplyr::mutate(trend_month = dplyr::case_when(n_month_current > n_month_minus1 * 1.1 ~ "&#x2191",
    #                                              n_month_current < n_month_minus1 * 0.9 ~ "&#x2193",
    #                                              TRUE ~ "--"))

  return(data_count)

}

# Monthly means and SDs for four-year lookback period -------------------------
# High volume conditions
f_table_dataprep_mean_highvolume <- function(data, conditions) {
  
  data_mean <- data %>% 
    dplyr::filter(event_yearmonth %in% hlm_baseline) %>% 
    #
    dplyr::mutate(event_yearmonth = factor(event_yearmonth,
                                           levels = hlm_baseline),
                  #
                  condition_label = factor(condition_label,
                                           levels = conditions)) %>%
    #
    dplyr::group_by(condition_label, event_yearmonth) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition_label, event_yearmonth,
                    fill = list(n = 0)) %>% 
    #
    dplyr::group_by(condition_label) %>% 
    dplyr::summarise(mean_month = mean(n, na.rm = TRUE),
                     sd_month   = sd(n, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  return(data_mean)

}
  
# Low volume conditions
f_table_dataprep_mean_lowvolume <- function(data, conditions) {
  
  data_mean <- data %>% 
    dplyr::filter(event_yearmonth >= epimonth_current - months(48) & event_yearmonth < epimonth_current) %>%
    #
    dplyr::mutate(event_year = factor(event_year,
                                      levels = c(ytd_four_year)),
                  #
                  event_month = factor(event_month,
                                       levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                  #
                  condition_label = factor(condition_label,
                                           levels = conditions)) %>% 
    #
    dplyr::group_by(condition_label, event_month) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition_label, event_month,
                    fill = list(n = 0)) %>%
    #
    dplyr::group_by(condition_label) %>% 
    dplyr::summarise(mean_month = mean(n, na.rm = TRUE),
                     sd_month   = sd(n, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(data_mean)

}

# NEPHU and Victorian rates for current month ----------------------------------
f_table_dataprep_rate <- function(data, data_vic, conditions) {
  
  data_rate <- data %>% 
    dplyr::mutate(condition_label = factor(condition_label,
                                           levels = conditions)) %>%
    #
    dplyr::group_by(condition_label) %>% 
    dplyr::summarise(n_nephu = sum(event_yearmonth == epimonth_current)) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition_label,
                    fill = list(n_nephu = 0)) %>% 
    #
    dplyr::mutate(rate_nephu = n_nephu / population_nephu_month * 100000) %>% 
    #
    dplyr::left_join(data_vic %>% 
                       dplyr::group_by(condition_label) %>% 
                       dplyr::summarise(n_vic = n()) %>% 
                       dplyr::ungroup() %>% 
                       #
                       tidyr::complete(condition_label,
                                       fill = list(n_vic = 0)) %>% 
                       #
                       dplyr::mutate(rate_vic = n_vic / population_vic_month * 100000),
                     by = "condition_label") %>% 
    #
    dplyr::mutate(rate_vic = dplyr::case_when(is.na(rate_vic) ~ 0.0,
                                              TRUE ~ rate_vic),
                  #
                  compare_rate = dplyr::case_when(rate_nephu > rate_vic * 1.1 ~ "↑",
                                                  rate_nephu < rate_vic * 0.9 ~ "↓",
                                                  TRUE ~ "--")) %>%
    # Alternate option if up/down arrows not displaying correctly
    # dplyr::mutate(rate_vic = dplyr::case_when(is.na(rate_vic) ~ 0.0,
    #                                           TRUE ~ rate_vic),
    #               #
    #               compare_rate = dplyr::case_when(rate_nephu > rate_vic * 1.1 ~ "&#x2191",
    #                                               rate_nephu < rate_vic * 0.9 ~ "&#x2193",
    #                                               TRUE ~ "--")) %>%
    #
    dplyr::select(condition_label,
                  compare_rate,
                  rate_nephu,
                  rate_vic)

  return(data_rate)

}

# YTD and yearly average counts ------------------------------------------------
f_table_dataprep_ytd <- function(data, conditions) {
  
  data_ytd <- data %>%
    dplyr::mutate(condition_label = factor(condition_label,
                                           levels = conditions)) %>%
    #
    dplyr::group_by(condition_label) %>%
    dplyr::summarise(
      n_ytd_current = sum(event_year == ytd_current & event_month %in% epimonth_ytd),
      n_ytd_minus1  = sum(event_year == ytd_minus1 & event_month %in% epimonth_ytd),
      n_ytd_allyr   = sum(event_year %in% ytd_four_year & event_month %in% epimonth_ytd),
      mean_ytd      = n_ytd_allyr / 4) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(condition_label,
                    fill = list(n_ytd_current = 0)) %>% 
    #
    dplyr::mutate(trend_ytd = dplyr::case_when(n_ytd_current > n_ytd_minus1 * 1.1 ~ "↑",
                                               n_ytd_current < n_ytd_minus1 * 0.9 ~ "↓",
                                               TRUE ~ "--")) %>%
    # Alternate option if up/down arrows not displaying correctly
    # dplyr::mutate(trend_ytd = dplyr::case_when(n_ytd_current > n_ytd_minus1 * 1.1 ~ "&#x2191",
    #                                            n_ytd_current < n_ytd_minus1 * 0.9 ~ "&#x2193",
    #                                            TRUE ~ "--")) %>%
    #
    dplyr::left_join(
      dplyr::select(reference_conditions, 
                    condition_label, 
                    one_years_data, 
                    four_years_data),
      by = "condition_label") %>%
    #
    replace(is.na(.), 0) %>%
    #
    dplyr::mutate(
      n_ytd_minus1 = dplyr::case_when(one_years_data == "No" ~ NA_integer_,
                                      TRUE ~ n_ytd_minus1),
      #
      mean_ytd = dplyr::case_when(four_years_data == "No" ~ NA_integer_,
                                  TRUE ~ mean_ytd)) %>% 
    #
    dplyr::select(condition_label,
                  trend_ytd,
                  n_ytd_current, n_ytd_minus1,
                  mean_ytd)

  return(data_ytd)

}

# Create and format summary table ------------------------------
f_table_format <- function(data, data_count, conditions, last_row) {
  
  data <- data %>%
    dplyr::select(-sig_month) %>%
    #
    dplyr::mutate(mean_month = round(mean_month, digits = 1),
                  mean_ytd   = round(mean_ytd, digits = 1),
                  rate_nephu = round(rate_nephu, digits = 1),
                  rate_vic   = round(rate_vic, digits = 1))
  
  original_names <- colnames(data)
  
  table <- data %>%
    gt::gt() %>%
    #
    gt::cols_label(.list = setNames(tables_colnames, original_names)) %>%
    #
    gt::sub_missing(missing_text = "-") %>%
    #
    gt::cols_align(align   = "left",
                   columns = 1) %>%
    #
    gt::cols_align(align   = "center",
                   columns = -1) %>%
    #
    gt::cols_width(1    ~ px(225),
                   2:5  ~ px(65),
                   6    ~ px(75),
                   7:12 ~ px(65)) %>%
    #
    gt::tab_style(style     = cell_fill(color = "seashell"),
                  locations = cells_body(columns = 6:8)) %>%
    #
    gt::tab_style(style     = cell_text(weight = "bold"),
                  locations = cells_column_labels(everything())) %>%
    #
    gt::tab_style(style     = list(cell_text(weight = "bold", 
                                             color  = "red")),
                  locations = cells_body(rows = data_count$sig_month == "Higher than expected")) %>%
    #
    gt::tab_style(style     = cell_text(style = "italic"),
                  locations = cells_body(rows = data_count$sig_month == "**")) %>%
    #
    gt::tab_spanner(label   = "Monthly counts",
                    columns = 2:5) %>%
    #
    gt::tab_spanner(label   = "Monthly rates",
                    columns = 6:8) %>%
    #
    gt::tab_spanner(label   = "YTD counts",
                    columns = 9:12) %>%
    #
    gt::tab_style(style     = cell_text(weight = "bold"),
                  locations = cells_column_spanners(everything())) %>% 
    #
    gt::opt_table_font(font = list("Arial",
                                   default_fonts())) %>%
    #
    gt::tab_options(table.font.size = px(12),
                    table.width     = "auto")
  
  if (!is.null(last_row)) {
    
    table <- table %>%
      gt::tab_row_group(label = conditions,
                        rows  = 1:last_row) %>%
      #
      gt::tab_style(style     = list(cell_fill(color  = "#FAF8F6"),
                                     cell_text(color  = "#191D43",
                                               weight = "bold")),
                    locations = cells_row_groups())

  }
  
  return(table)
  
}

