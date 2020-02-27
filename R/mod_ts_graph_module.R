# Module UI
  
#' @title   mod_ts_graph_module_ui and mod_ts_graph_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ts_graph_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ts_graph_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns('ggplot_ts_graph'))
  )
}
    
# Module Server
    
#' @rdname mod_ts_graph_module
#' @export
#' @keywords internal
    
mod_ts_graph_module_server <- function(input, output, session, country_filter, ts_data){
  ns <- session$ns
  
  country_data <- ts_data %>%
    dplyr::filter(country == country_filter)
  
  overall_country_data <- country_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(total_cases = sum(num_cases)) 
  
  overall_country_last_date_data <- overall_country_data %>%
    dplyr::filter(date == max(date))
  
  output$ggplot_ts_graph <- shiny::renderPlot({overall_country_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = total_cases)) +
    ggplot2::geom_point(colour = 'brown', alpha = 0.5, size = 3) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'Fecha',
         y = 'Casos Reportados',
         title = glue::glue('Numero de Casos por Dia {country_filter}'),
         subtitle = glue::glue('Ultima fecha disponible: {overall_country_last_date_data$date} ({overall_country_last_date_data$total_cases} casos)'))
    })
  
}
    
## To be copied in the UI
# mod_ts_graph_module_ui("ts_graph_module_ui_1")
    
## To be copied in the server
# callModule(mod_ts_graph_module_server, "ts_graph_module_ui_1")
 
