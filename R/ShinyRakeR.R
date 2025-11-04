#' The MoveRakeR Shiny app
#'
#' An interactive Shiny app to visualise and assess the impacts of potentially erroneous data over time
#'
#' @details
#' By running the \code{ShinyRakeR} command, this launches an interactive MoveRakeR dashboard. From here the user has
#' two main tabs: (1) Threshold explorer, and (2) Temporal inspector.
#'
#' (1) The Threshold explorer tab
#' This tab uses a combination of some of the processes available in \code{clean_GPS} and
#' a soft 'rake' of the data leaning into the presentation within the \code{rake} function. This tab allows
#' the user to first label gapsections using the \code{gap_section} function (optionally), and then annotate data through:
#'
#' - Number of fixes per bird;
#' - Bespoke existing flagged columns already in the data;
#' - Missing longitude/latitude data;
#' - Duplicate date-time stamps;
#' - A minimum number of satellites;
#' - Minimum PDOP and HDOP values;
#' - Labelling of single gapsections from initial use of the \code{gap_section} function;
#' - The trajectory speed filter through \code{speed_filt};
#' - A custom simple trajectory speed filter threshold;
#' - The turning angle filter as through \code{turn_filt};
#' - A maximum perceived extent of the xy bounds; and
#' - A combination of all columns selected.
#'
#' These combinations can be viewed spatially in the map on the right-hand side and fixes labelled as potentially
#' 'incorrect' can be coloured as red points separate to 'good' fixes plotted in blue, or NAs if relevant, as grey.
#' Individual TagIDs can also be selected from the plot and a built in time slider can subset data for specific temporal periods.
#'
#' The 'raking' of the data takes place under the map presented as small gauges to assess what would be the
#' consequences of dropping data marked as erroneous and the contribution of these drops from the different
#' steps used. A DT::datatable is also provided to show which TagIDs contribute to these decisions per annotation.
#'
#' The user can also export annotated data back to the current R session global environment.
#'
#' (2) The temporal inspector tab
#' This tab has a separate map and timeslider that is linked to the threshold explorer tab. This can be used to bring
#' up numeric variables in the data and plot them through time using \code{ggplot} or \code{plotly}. This tab is therefore
#' handy to show variation over the covariates through time and where these fixes are spatially.
#'
#' An interactive histogram is also provided to visualise the distribution of these variables.
#'
#' A further visualisation of the density of animals tracked over time is provided using
#' the \code{tag_timeline} function, sensitive to the timeline slider, with options to adjust plotting over ggplot codes for date-time breaks.
#'
#' This Shiny app is functional but will hopefully receive further updates going forward.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude. If no data are supplied
#' a fake dataset is automatically generated.
#' @param dev A developer testing tool not to be used, default FALSE.
#' @param launchBrowser TRUE/FALSE (Default TRUE) whether to launch a browser window (preferred) otherwise a local
#' window will be used in RStudio, but app visual performance may then be sub-optimal.
#' @param Providers A list of providers to use in the base leaflet maps for options of switching between
#' tiled layers; defaults to: c("OpenStreetMap", "GoogleEarth", "Esri.OceanBasemap", "Esri.WorldImagery").
#'
#' @seealso [MoveRakeR::rake], @seealso [MoveRakeR::rake_outlie]
#'
#' @examples
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata)
#' ShinyRakeR(data)
#'
#' @export
ShinyRakeR <- function(data = NULL, dev = FALSE, launchBrowser = TRUE,
                       Providers = c("OpenStreetMap", "GoogleEarth", "Esri.OceanBasemap", "Esri.WorldImagery")
  ){

  if(dev){
    appDir <- "Y:/R packages/MoveRakeR/inst/shiny/ShinyRakeR/"
  } else{
    appDir <- system.file("shiny", "ShinyRakeR", package = "MoveRakeR")

  }
  if (appDir == "") stop("Could not find app directory. Try reinstalling MoveRakeR.")

  # Inject data into global environment of app
  if(!is.null(data)) assign("userData", data, envir = .GlobalEnv)

  shiny::shinyOptions(userData = data, Providers = Providers)

  require(data.table)
  require(shinydashboard)
  require(shinybusy)
  require(flexdashboard)
  require(shinyWidgets)
  require(plotly)
  require(ggplot2)
  require(leaflet)
  require(shiny)
  require(dplyr)
  require(tidyr)
  require(geosphere)
  require(DT)
  #require(leafgl)

  shiny::runApp(appDir, display.mode = "normal", launch.browser = launchBrowser)
}













