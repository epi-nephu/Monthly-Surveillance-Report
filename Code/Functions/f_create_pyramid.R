# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.0 13/01/2025

# Code to prepare data for and format age pyramids

# Create and format age pyramid ------------------------------------------------
f_pyramid_format <- function(data, charttitle) {
  
  figure <- data %>% 
    apyramid::age_pyramid(age_group     = age_10year,
                          split_by      = sex,
                          show_midpoint = FALSE,
                          pal           = c(nephu_blue, nephu_peach)) +
    #
    labs(title = paste0(charttitle, " age and sex distribution, ", "NEPHU, ",
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
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          axis.text = element_text(size = 8),
          #
          legend.position = "bottom",
          legend.title    = element_blank())
  
  return(figure)

}

