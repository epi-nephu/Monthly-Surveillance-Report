# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 4.0 04/02/2025

# Code to prepare data for and format ribbon plots

# Number of cases per month ----------------------------------------------------
f_ribbon_dataprep_count <- function(data, variable) {
  
  variable <- rlang::enquo(variable)
  
  data_count <- data %>% 
    dplyr::mutate(event_year = factor(event_year,
                                      levels = c(ytd_ribbon)),
                  #
                  event_month = factor(event_month,
                                       levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
    #
    dplyr::group_by(!!variable, event_year, event_month) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup() %>% 
    #
    tidyr::complete(!!variable, event_year, event_month,
                    fill = list(n = 0)) %>% 
    #
    dplyr::mutate(event_yearmonth = lubridate::ymd(paste0(event_year, " ", event_month, " 01")),
                  #
                  n = dplyr::case_when(
                    event_yearmonth > epimonth_current ~ NA_integer_,
                    TRUE                               ~ as.numeric(n))) %>% 
    #
    dplyr::filter(event_yearmonth <= epimonth_current + months(1)) %>% 
    #
    dplyr::select(!!variable, 
                  event_yearmonth,
                  event_year,
                  event_month,
                  n)
  
  return(data_count)

}

# Mean and upper/lower limits for each month -----------------------------------
# High volume conditions
f_ribbon_dataprep_mean_highvolume <- function(data, variable) {
  
  variable <- rlang::enquo(variable)
#  var_name <- rlang::as_name(variable)
  
  # data_mean <- data %>%
  #   dplyr::arrange(!!variable, event_yearmonth) %>%
  #   #
  #   dplyr::group_by(!!variable, event_yearmonth) %>%
  #   # 
  #   dplyr::mutate(
  #     mean = sapply(seq_along(event_yearmonth), function(i) {
  #       #
  #       rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37))
  #       # 
  #       rolling_values <- data$n[data$event_yearmonth %in% rolling_months]
  #       mean(rolling_values, na.rm = TRUE)
  #       #
  #       }),
  #     #
  #     sd = sapply(seq_along(event_yearmonth), function(i) {
  #       # 
  #       rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37))
  #       #
  #       rolling_values <- data$n[data$event_yearmonth %in% rolling_months]
  #       sd(rolling_values, na.rm = TRUE)
  #       #
  #     }),
  #     # 
  #     limit_sd_upper = mean + (sd * 2),
  #     limit_sd_lower = mean - (sd * 2)) %>%
  #   #
  #   dplyr::ungroup() %>% 
  #   #
  #   dplyr::filter(event_yearmonth >= ribbon_start)
  
  data_mean <- data %>%
    dplyr::arrange(!!variable, event_yearmonth) %>%
    #
    dplyr::group_by(!!variable) %>%
    # 
    dplyr::mutate(
      mean = sapply(seq_along(event_yearmonth), function(i) {
        #
        #current_var    <- data[[var_name]][i]
        current_month  <- event_yearmonth[i]
        rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37, 47:49))
        # 
        rolling_values <- n[event_yearmonth %in% rolling_months]
        mean(rolling_values, na.rm = TRUE)
        #
      }),
      #
      sd = sapply(seq_along(event_yearmonth), function(i) {
        # 
        #current_var    <- data[[var_name]][i]
        current_month  <- event_yearmonth[i]
        rolling_months <- event_yearmonth[i] + months(-c(11:13, 23:25, 35:37, 47:49))
        #
        rolling_values <- n[event_yearmonth %in% rolling_months]
        sd(rolling_values, na.rm = TRUE)
        #
      }),
      # 
      limit_sd_upper = mean + (sd * 2),
      limit_sd_lower = mean - (sd * 2)) %>%
    #
    dplyr::ungroup() %>% 
    #
    dplyr::filter(event_yearmonth >= ribbon_start)
  
  return(data_mean)

}

# Mean and upper/lower limits for each month -----------------------------------
# Low volume conditions
f_ribbon_dataprep_mean_lowvolume <- function(data, variable) {
  
  variable <- rlang::enquo(variable)
  
  data_mean <- data %>% 
    dplyr::filter(event_yearmonth >= epimonth_current - months(48) & event_yearmonth < epimonth_current) %>%
    #
    dplyr::group_by(!!variable) %>% 
    dplyr::mutate(mean = mean(n, na.rm = TRUE),
                  sd   = sd(n, na.rm = TRUE),
                  #
                  limit_sd_upper = mean + (sd * 2),
                  limit_sd_lower = mean - (sd * 2)) %>%
    dplyr::ungroup() %>% 
    #
    dplyr::distinct(!!variable, event_month, .keep_all = TRUE) %>% 
    #
    dplyr::select(-event_yearmonth,
                  -event_year,
                  -n)
  
  return(data_mean)
  
}

# Create and format ribbon chart -----------------------------------------------
# Historical data available
f_ribbon_format_historical_yes <- function(data, chart_title) {
  
  max_sd <- max(data$limit_sd_upper)
  
  max_n <- data %>% 
    dplyr::filter(event_yearmonth < epimonth_current) %>%
    #
    dplyr::arrange(desc(n)) %>%
    #
    dplyr::slice(1) %>%
    #
    pull(n)
  
  chart_max_n <- max(max_n, max_sd)
  
  figure <- ggplot(data = data,
                   aes(x = event_yearmonth)) +
    #
    geom_ribbon(data = data,
                aes(ymin = pmax(0, limit_sd_lower), ymax = limit_sd_upper, fill = "2SD limits from mean"),
                alpha = 0.75) +
    #
    geom_line(data = data,
              aes(y = mean, colour = "Mean number of cases"),
              linewidth = 0.5,
              linetype  = "longdash",
              alpha     = 0.75) +
    #
    geom_line(data = dplyr::filter(data, event_yearmonth <= epimonth_current),
              aes(y = n, colour = "Observed number of cases"),
              linewidth = 1.25,
              alpha     = 1) +
    #
    scale_x_date(expand      = c(0, 0),
                 date_labels = "%b %Y", 
                 breaks      = "3 month") +
    #
    scale_y_continuous(limits = c(0, dplyr::case_when(chart_max_n <= 1   ~ 1.0025,
                                                      chart_max_n <= 2.5 ~ 2.5625,
                                                      chart_max_n <= 5   ~ 5.125,
                                                      chart_max_n <= 10  ~ (ceiling(chart_max_n / 1) * 1) + (chart_max_n * 0.025),
                                                      chart_max_n <= 50  ~ (ceiling(chart_max_n / 5) * 5) + (chart_max_n * 0.025),
                                                      chart_max_n <= 100 ~ (ceiling(chart_max_n / 10) * 10) + (chart_max_n * 0.025),
                                                      chart_max_n <= 200 ~ (ceiling(chart_max_n / 20) * 20) + (chart_max_n * 0.025),
                                                      chart_max_n <= 500 ~ (ceiling(chart_max_n / 50) * 50) + (chart_max_n * 0.025),
                                                      chart_max_n <= 1000 ~ (ceiling(chart_max_n / 100) * 100) + (chart_max_n * 0.025),
                                                      chart_max_n <= 2500 ~ (ceiling(chart_max_n / 250) * 250) + (chart_max_n * 0.025),
                                                      chart_max_n <= 5000 ~ (ceiling(chart_max_n / 500) * 500) + (chart_max_n * 0.025),
                                                      chart_max_n <= 10000 ~ (ceiling(chart_max_n / 1000) * 1000) + (chart_max_n * 0.025),
                                                      chart_max_n <= 20000 ~ (ceiling(chart_max_n / 2000) * 2000) + (chart_max_n * 0.025),
                                                      chart_max_n <= 50000 ~ (ceiling(chart_max_n / 5000) * 5000) + (chart_max_n * 0.025),
                                                      TRUE ~ NA)),
                       #
                       breaks = scales::breaks_width(dplyr::case_when(chart_max_n <= 1   ~ 0.25,
                                                                      chart_max_n <= 2.5 ~ 0.5,
                                                                      chart_max_n <= 10  ~ 1,
                                                                      chart_max_n <= 50  ~ 5,
                                                                      chart_max_n <= 100 ~ 10,
                                                                      chart_max_n <= 200 ~ 20,
                                                                      chart_max_n <= 500 ~ 50,
                                                                      chart_max_n <= 1000 ~ 100,
                                                                      chart_max_n <= 2500 ~ 250,
                                                                      chart_max_n <= 5000 ~ 500,
                                                                      chart_max_n <= 10000 ~ 1000,
                                                                      chart_max_n <= 20000 ~ 2000,
                                                                      chart_max_n <= 50000 ~ 5000,
                                                                      TRUE ~ NA))) +
    #
    scale_colour_manual(values = c("Mean number of cases"     = nephu_blue,
                                   "Observed number of cases" = nephu_green),
                        breaks = c("Observed number of cases", "Mean number of cases")) +
    #
    scale_fill_manual(values = c("2SD limits from mean" = nephu_grey),
                      breaks = "2SD limits from mean") +
    #
    guides(colour = guide_legend(order = 1),
           fill   = guide_legend(order = 2)) +
    #
    labs(title = paste0(chart_title, " in NEPHU, ",
                        format(ribbon_start, format = "%b %Y"),
                        " to ",
                        format(epimonth_current, format = "%b %Y")),
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
          axis.text   = element_text(size = 8),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          legend.title    = element_blank(),
          legend.position = "bottom")
  
  return(figure)

}

# Create and format ribbon chart -----------------------------------------------
# Historical data not available
f_ribbon_format_historical_no <- function(data, chart_title) {
  
  chart_max_n <- data %>% 
    dplyr::filter(event_yearmonth < epimonth_current) %>%
    #
    dplyr::arrange(desc(n)) %>%
    #
    dplyr::slice(1) %>%
    #
    pull(n)
  
  figure <- data %>%
    dplyr::filter(event_yearmonth >= ribbon_start) %>% 
    #
    ggplot(aes(x = event_yearmonth)) +
    #
    geom_line(aes(y = n, colour = "Observed number of cases"),
              linewidth = 1.25,
              alpha     = 1) +
    #
    scale_x_date(expand      = c(0, 0),
                 date_labels = "%b %Y", 
                 breaks      = "3 month") +
    #
    scale_y_continuous(limits = c(0, dplyr::case_when(chart_max_n <= 1   ~ 1.005,
                                                      chart_max_n <= 2.5 ~ 2.5625,
                                                      chart_max_n <= 5   ~ 5.125,
                                                      chart_max_n <= 10  ~ (ceiling(chart_max_n / 1) * 1) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50  ~ (ceiling(chart_max_n / 5) * 5) + (chart_max_n * 0.05),
                                                      chart_max_n <= 100 ~ (ceiling(chart_max_n / 10) * 10) + (chart_max_n * 0.05),
                                                      chart_max_n <= 200 ~ (ceiling(chart_max_n / 20) * 20) + (chart_max_n * 0.05),
                                                      chart_max_n <= 500 ~ (ceiling(chart_max_n / 50) * 50) + (chart_max_n * 0.05),
                                                      chart_max_n <= 1000 ~ (ceiling(chart_max_n / 100) * 100) + (chart_max_n * 0.05),
                                                      chart_max_n <= 2500 ~ (ceiling(chart_max_n / 250) * 250) + (chart_max_n * 0.05),
                                                      chart_max_n <= 5000 ~ (ceiling(chart_max_n / 500) * 500) + (chart_max_n * 0.05),
                                                      chart_max_n <= 10000 ~ (ceiling(chart_max_n / 1000) * 1000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 20000 ~ (ceiling(chart_max_n / 2000) * 2000) + (chart_max_n * 0.05),
                                                      chart_max_n <= 50000 ~ (ceiling(chart_max_n / 5000) * 5000) + (chart_max_n * 0.05),
                                                      TRUE ~ NA)),
                       #
                       breaks = scales::breaks_width(dplyr::case_when(chart_max_n <= 1   ~ 0.25,
                                                                      chart_max_n <= 2.5 ~ 0.5,
                                                                      chart_max_n <= 10  ~ 1,
                                                                      chart_max_n <= 50  ~ 5,
                                                                      chart_max_n <= 100 ~ 10,
                                                                      chart_max_n <= 200 ~ 20,
                                                                      chart_max_n <= 500 ~ 50,
                                                                      chart_max_n <= 1000 ~ 100,
                                                                      chart_max_n <= 2500 ~ 250,
                                                                      chart_max_n <= 5000 ~ 500,
                                                                      chart_max_n <= 10000 ~ 1000,
                                                                      chart_max_n <= 20000 ~ 2000,
                                                                      chart_max_n <= 50000 ~ 5000,
                                                                      TRUE ~ NA))) +
    #
    scale_colour_manual(values = c("Observed number of cases" = nephu_green)) +
    #
    labs(title = paste0(chart_title, " in NEPHU, ",
                        format(ribbon_start, format = "%b %Y"),
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
          axis.text   = element_text(size = 8),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          legend.title    = element_blank(),
          legend.position = "bottom")
  
  return(figure)

}

