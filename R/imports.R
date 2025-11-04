#' R/imports.R
#' @name MoveRakeR-imports
#' @keywords internal
#' ------------------------------------------- #
#' Centralised import directives for MoveRakeR
#' ------------------------------------------- #
#'
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @import leaflet
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinybusy
#' @import units
#' @import readr
#' @import sf
#' @import sp
#' @import ggplot2
#' @importFrom rmapshaper ms_simplify
#' @importFrom move getMovebankAnimals getMovebankLocationData movebankLogin move moveStack
#' @importFrom RODBC sqlQuery odbcConnect
#' @importFrom viridisLite viridis
#' @importFrom lubridate date with_tz round_date year
#' @importFrom geosphere distRhumb distHaversine
#' @importFrom adehabitatLT as.ltraj redisltraj
#' @importFrom sfheaders sf_multilinestring
#' @importFrom htmltools tagList
#' @importFrom suncalc getSunlightTimes
#' @importFrom flexdashboard renderGauge
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom DT renderDataTable dataTableOutput datatable formatRound formatStyle styleEqual
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable, getQueryString))
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek, first, last, between, transpose))
#' importFrom(stats, sd, na.omit, as.formula, median, var, quantile, rnorm, runif)
#' importFrom(graphics, boxplot, hist, legend, par, segments)
#' importFrom(methods, is)
#' importFrom(utils, combn, head, tail)
NULL
