# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 4.0 04/02/2025

# Code to prepare data for and format bar charts

# Number of cases per year -----------------------------------------------------
f_barchart_dataprep_year <- function(data,
                                     condition_name) {
  
  data_year <- data %>%
    dplyr::filter(condition_label == condition_name) %>%
    dplyr::filter(event_year %in% ytd_bar) %>% 
    #
    dplyr::mutate(event_year = factor(event_year,
                                      levels = c(ytd_bar))) %>% 
    #
    dplyr::group_by(event_year) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(event_year,
                    fill = list(n = 0))
  
  return(data_year)
  
}

# Create and format bar chart - cases per year ---------------------------------
f_barchart_format_year <- function(data, chart_max_n, chart_title) {
  
  figure <- data %>%
    ggplot(aes(x = event_year, y = n)) +
    #
    geom_col(fill = nephu_green) +
    #
    # scale_y_continuous(limits = c(0, chart_ymax),
    #                    breaks = scales::breaks_width(chart_ybreaks),
    #                    expand = expansion(add = c(0, 0))) +
    #
    scale_y_continuous(limits = c(0, dplyr::case_when(chart_max_n <= 5     ~ 5.125,
                                                      chart_max_n <= 10    ~ (ceiling(chart_max_n / 1) * 1) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50    ~ (ceiling(chart_max_n / 5) * 5) + (chart_max_n * 0.05),
                                                      chart_max_n <= 100   ~ (ceiling(chart_max_n / 10) * 10) + (chart_max_n * 0.05),
                                                      chart_max_n <= 200   ~ (ceiling(chart_max_n / 20) * 20) + (chart_max_n * 0.05),
                                                      chart_max_n <= 500   ~ (ceiling(chart_max_n / 50) * 50) + (chart_max_n * 0.05),
                                                      chart_max_n <= 1000  ~ (ceiling(chart_max_n / 100) * 100) + (chart_max_n * 0.05),
                                                      chart_max_n <= 2500  ~ (ceiling(chart_max_n / 250) * 250) + (chart_max_n * 0.05),
                                                      chart_max_n <= 5000  ~ (ceiling(chart_max_n / 500) * 500) + (chart_max_n * 0.05),
                                                      chart_max_n <= 10000 ~ (ceiling(chart_max_n / 1000) * 1000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 20000 ~ (ceiling(chart_max_n / 2000) * 2000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50000 ~ (ceiling(chart_max_n / 5000) * 5000) + (chart_max_n * 0.05),
                                                      TRUE ~ NA)),
                       #
                       breaks = scales::breaks_width(dplyr::case_when(chart_max_n <= 10    ~ 1,
                                                                      chart_max_n <= 50    ~ 5,
                                                                      chart_max_n <= 100   ~ 10,
                                                                      chart_max_n <= 200   ~ 20,
                                                                      chart_max_n <= 500   ~ 50,
                                                                      chart_max_n <= 1000  ~ 100,
                                                                      chart_max_n <= 2500  ~ 250,
                                                                      chart_max_n <= 5000  ~ 500,
                                                                      chart_max_n <= 10000 ~ 1000,
                                                                      chart_max_n <= 20000 ~ 2000,
                                                                      chart_max_n <= 50000 ~ 5000,
                                                                      TRUE ~ NA)),
                       #
                       expand = expansion(add = c(0, 0))) +
    #
    labs(title = paste0(chart_title, " in NEPHU, ",
                        format(as.Date(paste0(ytd_bar[1], "-01", "-01")), format = "%b %Y"),
                        " to ",
                        format(epimonth_current, format = "%b %Y")),
         #
         x = NULL,
         y = "Number of cases") +
    #
    theme_classic() +
    #
    theme(plot.title.position = "plot",
          plot.title          = element_text(size  = 10,
                                             face  = "bold",
                                             hjust = 0),
          #
          plot.margin = margin(5, 5, 0, 5),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          axis.text = element_text(size = 8))
  
  return(figure)
  
}

# Number of cases per month ----------------------------------------------------
f_barchart_dataprep_month_count <- function(data, condition_name) {
  
  data_count <- data %>%
    dplyr::mutate(event_yearmonth = factor(event_yearmonth)) %>%
    #
    dplyr::filter(condition_label == condition_name) %>%
    #
    dplyr::group_by(event_yearmonth, .drop = FALSE) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::ungroup() %>%
    #
    dplyr::mutate(event_yearmonth = as.Date(event_yearmonth),
                  event_month     = lubridate::month(event_yearmonth, label = TRUE, abbr = TRUE))
  
  return(data_count)
  
}

# Mean number of cases per month -----------------------------------------------
f_barchart_dataprep_month_mean <- function(data, data_count, condition_name) {
  
  four_years_data <- data %>% 
    dplyr::filter(condition_label == condition_name) %>% 
    dplyr::select(four_years_data) %>% 
    unique()
  
  volume <- data %>% 
    dplyr::filter(condition_label == condition_name) %>% 
    dplyr::filter(event_yearmonth >= epimonth_current - months(48) & event_yearmonth < epimonth_current) %>% 
    #
    dplyr::summarise(n = n()) %>%
    #
    dplyr::mutate(mean = n / 4) %>% 
    #
    dplyr::mutate(volume = dplyr::case_when(
      mean < high_volume ~ "Low volume",
      TRUE ~ "High volume")) %>%
    #
    pull(volume)
  
  if (volume == "High volume") {
    
    data_mean <- data_count %>%
      dplyr::arrange(event_yearmonth) %>%
      #
      dplyr::group_by(event_yearmonth) %>%
      #
      dplyr::mutate(
        mean = sapply(seq_along(event_yearmonth), function(i) {
          #
          rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37, 47:49))
          #
          rolling_values <- data_count$n[data_count$event_yearmonth %in% rolling_months]
          mean(rolling_values, na.rm = TRUE)
          #
          })) %>%
      #
      dplyr::ungroup() %>%
      #
      dplyr::mutate(mean = dplyr::case_when(
        four_years_data == "No" ~ NA_integer_,
        TRUE ~ mean)) %>% 
      #
      dplyr::slice_tail(n = 12)
    
  } else {
    
    data_mean <- data_count %>% 
      dplyr::filter(event_yearmonth >= epimonth_current - months(48) & event_yearmonth < epimonth_current) %>%
      #
      dplyr::mutate(mean = mean(n, na.rm = TRUE)) %>%
      #
      dplyr::distinct(event_month, .keep_all = TRUE) %>% 
      #
      dplyr::select(-event_yearmonth,
                    -n)
    
    data_mean <- data_count %>% 
      dplyr::left_join(data_mean, by = "event_month") %>% 
      #
      dplyr::slice_tail(n = 12)

    }
  
  return(data_mean)

}

# Create and format bar chart - cases per month --------------------------------
f_barchart_format_month <- function(data, figure_number, figure_title) {
  
  chart_max_n <- max(data$n)
  
  figure <- data %>%
    ggplot(aes(x = event_yearmonth)) +
    #
    geom_col(aes(y = n, fill = "Number of cases"),
             alpha = 1) +
    #
    scale_fill_manual(values = nephu_green) +
    #
    geom_line(aes(y = mean, colour = "Mean number of cases"),
              linewidth = 1) +
    #
    scale_color_manual(values = nephu_blue) +
    #
    scale_x_date(expand      = c(0.025, 0.025),
                 date_labels = "%b %Y", 
                 breaks      = "1 month") +
    #
    # scale_y_continuous(limits = c(0, ymax),
    #                    breaks = scales::breaks_width(ybreaks),
    #                    expand = expansion(add = c(0, yexpand))) +
    #
    scale_y_continuous(limits = c(0, dplyr::case_when(chart_max_n <= 5     ~ 5.125,
                                                      chart_max_n <= 10    ~ (ceiling(chart_max_n / 1) * 1) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50    ~ (ceiling(chart_max_n / 5) * 5) + (chart_max_n * 0.05),
                                                      chart_max_n <= 100   ~ (ceiling(chart_max_n / 10) * 10) + (chart_max_n * 0.05),
                                                      chart_max_n <= 200   ~ (ceiling(chart_max_n / 20) * 20) + (chart_max_n * 0.05),
                                                      chart_max_n <= 500   ~ (ceiling(chart_max_n / 50) * 50) + (chart_max_n * 0.05),
                                                      chart_max_n <= 1000  ~ (ceiling(chart_max_n / 100) * 100) + (chart_max_n * 0.05),
                                                      chart_max_n <= 2500  ~ (ceiling(chart_max_n / 250) * 250) + (chart_max_n * 0.05),
                                                      chart_max_n <= 5000  ~ (ceiling(chart_max_n / 500) * 500) + (chart_max_n * 0.05),
                                                      chart_max_n <= 10000 ~ (ceiling(chart_max_n / 1000) * 1000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 20000 ~ (ceiling(chart_max_n / 2000) * 2000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50000 ~ (ceiling(chart_max_n / 5000) * 5000) + (chart_max_n * 0.05),
                                                      TRUE ~ NA)),
                       #
                       breaks = scales::breaks_width(dplyr::case_when(chart_max_n <= 10    ~ 1,
                                                                      chart_max_n <= 50    ~ 5,
                                                                      chart_max_n <= 100   ~ 10,
                                                                      chart_max_n <= 200   ~ 20,
                                                                      chart_max_n <= 500   ~ 50,
                                                                      chart_max_n <= 1000  ~ 100,
                                                                      chart_max_n <= 2500  ~ 250,
                                                                      chart_max_n <= 5000  ~ 500,
                                                                      chart_max_n <= 10000 ~ 1000,
                                                                      chart_max_n <= 20000 ~ 2000,
                                                                      chart_max_n <= 50000 ~ 5000,
                                                                      TRUE ~ NA)),
                       #
                       expand = expansion(add = c(0, 0))) +
    #
    labs(title = paste0("Figure ", figure_number, ". ",
                        figure_title,
                        " in NEPHU, ",
                        format(epimonth_current - months(11), format = "%b %Y"), 
                        " to ", 
                        format(epimonth_current, format = "%b %Y")),
         x = NULL,
         y = "Number of cases") +
    #
    theme_classic() +
    #
    theme(plot.title.position = "plot",
          plot.title          = ggtext::element_markdown(size  = 10,
                                                         face  = "bold",
                                                         hjust = 0),
          #
          plot.margin = margin(5, 5, 0, 5),
          #
          axis.title   = element_text(size = 9),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          axis.text   = element_text(size = 8),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          legend.title    = element_blank(),
          legend.position = "bottom",
          legend.margin   = margin(0, 0, 0, 0)) +
    #
    guides(fill  = guide_legend(order = 1),
           color = guide_legend(order = 2))
  
  return(figure)

}

