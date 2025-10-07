# NEPHU Monthly Surveillance Report
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.0 13/01/2025

# Code to prepare data for and format notification rate maps

# Notification rate by LGA -----------------------------------------------------
f_map_dataprep <- function(data, data_reference) {
  
  data <- data %>%
    dplyr::filter(event_yearmonth >= ribbon_start) %>% 
    #
    dplyr::group_by(local_government_area) %>% 
    dplyr::summarise(n_cases = n()) %>% 
    dplyr::ungroup() %>% 
    #
    dplyr::left_join(data_reference, by = c("local_government_area" = "lga")) %>% 
    #
    dplyr::mutate(rate = (n_cases / (x2021_population * 5)) * 100000)
  
  return(data)

}

# Create and format map --------------------------------------------------------
f_map_format <- function(data, data_shape, charttitle) {
  
  figure <- ggplot(data) +
    geom_sf(aes(fill = rate), color = "black") +
    #
    scale_fill_gradient_tableau(palette = "Blue", name = "Rate (per 100,000 person-years)") +
    #
    geom_sf_text(data = data_shape %>% filter(!(lga_name %in% lga_nudge)), 
                 aes(label = lga_name), 
                 size = 3) + 
    #
    geom_sf_text(data = data_shape %>% filter(lga_name == "Darebin"),
                 aes(label = lga_name), 
                 size    = 3, 
                 nudge_x = -0.07, 
                 nudge_y = -0.01) +
    #
    geom_sf_text(data = data_shape %>% filter(lga_name == "Yarra"), 
                 aes(label = lga_name), 
                 size    = 3, 
                 nudge_x = -0.03, 
                 nudge_y = -0.01) +
    #
    geom_sf_text(data = data_shape %>% filter(lga_name == "Boroondara"), 
                 aes(label = lga_name), 
                 size    = 3, 
                 nudge_x = -0.07, 
                 nudge_y = -0.025) +
    #
    geom_sf_text(data = data_shape %>% filter(lga_name == "Maroondah"), 
                 aes(label = lga_name), 
                 size    = 3, 
                 nudge_x = 0.03, 
                 nudge_y = -0.01) + 
    #
    labs(title = paste0(charttitle, " notification rate by LGA, ", "NEPHU, ",
                        format(ribbon_start, format = "%b %Y"),
                        " to ",
                        format(epimonth_current, format = "%b %Y"))) +
    #
    theme_void() +
    #
    theme(plot.title.position = "plot",
          plot.title          = element_text(size  = 10,
                                             face  = "bold",
                                             hjust = 0.5),
          #
          plot.margin = margin(5, 5, 0, 5),
          #
          legend.position = "bottom",
          legend.title    = element_text(size = 9)) + 
    #
    guides(fill = guide_colorbar(barwidth = 10))
  
  return(figure)
  
}

