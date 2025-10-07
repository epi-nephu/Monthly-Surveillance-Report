# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.0 13/01/2025

# Code to prepare data for and format bar charts

# Number of cases per year -----------------------------------------------------
f_barchart_dataprep_year <- function(data,
                                     condition_name) {
  
  data_year <- data %>%
    dplyr::filter(condition_label == condition_name) %>%
    #
    dplyr::mutate(event_year = factor(event_year,
                                      levels = c(ytd_ribbon))) %>% 
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
f_barchart_format_year <- function(data, ymax, yexpand, charttitle) {
  
  figure <- data %>%
    ggplot(aes(x = event_year, y = n)) +
    #
    geom_col(fill = nephu_green) +
    #
    scale_y_continuous(limits = c(0, ymax),
                       expand = expansion(add = c(0, yexpand)),
                       breaks = scales::breaks_extended(n = 5)) +
    #
    labs(title = paste0(charttitle, " in NEPHU, ",
                        format(as.Date(paste0(ytd_ribbon[1], "-01", "-01")), format = "%b %Y"),
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
          plot.margin  = margin(5, 5, 0, 5),
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
  
  three_years_data <- data %>% 
    dplyr::filter(condition_label == condition_name) %>% 
    dplyr::select(three_years_data) %>% 
    unique()
  
  volume <- data %>% 
    dplyr::filter(condition_label == condition_name) %>% 
    dplyr::filter(event_yearmonth >= epimonth_current - months(36) & event_yearmonth < epimonth_current) %>% 
    #
    dplyr::summarise(n = n()) %>%
    #
    dplyr::mutate(mean = n / 3) %>% 
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
          rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37))
          #
          rolling_values <- data_count$n[data_count$event_yearmonth %in% rolling_months]
          mean(rolling_values, na.rm = TRUE)
          #
          })) %>%
      #
      dplyr::ungroup() %>%
      #
      dplyr::mutate(mean = dplyr::case_when(
        three_years_data == "No" ~ NA_integer_,
        TRUE ~ mean)) %>% 
      #
      dplyr::slice_tail(n = 12)
    
  } else {
    
    data_mean <- data_count %>% 
      dplyr::filter(event_yearmonth >= epimonth_current - months(36) & event_yearmonth < epimonth_current) %>%
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
f_barchart_format_month <- function(data, ymax, ybreaks, yexpand, figure_number, figure_title) {
  
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
    scale_y_continuous(limits = c(0, ymax),
                       breaks = scales::breaks_width(ybreaks),
                       expand = expansion(add = c(0, yexpand))) +
    #
    labs(title = paste0(figure_number,
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

