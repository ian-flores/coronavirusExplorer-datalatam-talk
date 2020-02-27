#' @import shiny
app_server <- function(input, output,session) {
  data_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
  
  coronavirus_cases_ts <- readr::read_csv(data_url)
  
  tidy_coronavirus_cases_ts <- coronavirus_cases_ts %>%
    tidyr::gather(key = 'date', value = 'num_cases', -`Province/State`:-Long) %>%
    dplyr::rename('region' = `Province/State`,
                  'country' = `Country/Region`,
                  'lat' = `Lat`,
                  'long' = `Long`) %>%
    dplyr::mutate(date = lubridate::mdy(date))
  
  # List the first level callModules here
  callModule(mod_ts_graph_module_server, "ts_graph_china", 'Mainland China', tidy_coronavirus_cases_ts)
  callModule(mod_region_barplot_module_server, 'barplot_china', 'Mainland China', tidy_coronavirus_cases_ts)
}
