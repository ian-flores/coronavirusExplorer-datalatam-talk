# Module UI
  
#' @title   mod_region_barplot_module_ui and mod_region_barplot_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param country_filter The name of the country to be displayed
#' @param ts_data The timeseries data in a tidy format
#'
#' @rdname mod_region_barplot_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_region_barplot_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns('region_barplot'))
  )
}
    
# Module Server
    
#' @rdname mod_region_barplot_module
#' @export
#' @keywords internal
    
mod_region_barplot_module_server <- function(input, output, session, country_filter, ts_data){
  ns <- session$ns
  
  country_data <- ts_data %>%
    dplyr::filter(country == country_filter)
  
  region_country_data <- country_data %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(total_cases = sum(num_cases))
  
  output$region_barplot <- shiny::renderPlot({region_country_data %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(region, total_cases), y = total_cases)) +
    ggplot2::geom_bar(stat = 'identity', fill = 'brown', alpha = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'Region',
         y = 'Total de Casos',
         title = glue::glue('Total cases de casos por region en {country_filter}'))
  })
}
    
## To be copied in the UI
# mod_region_barplot_module_ui("region_barplot_module_ui_1")
    
## To be copied in the server
# callModule(mod_region_barplot_module_server, "region_barplot_module_ui_1")
 
