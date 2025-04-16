#' Interactive leaflet map for visualising trips of animals
#'
#' @description
#' An Shiny R interactive map to view trips of animals, with some basic summaries and plots based on
#' the \code{leaflet} R package.
#'
#' @details
#' Function \code{plot_leaflet_trips} is aimed at providing visual summaries of telemetry
#' data after "trips" having been identified in the data, for example through function \code{\link{define_trips}}.
#' The function uses the JavaScript \code{leaflet} R package to plot base map tiles of OSM, Google Earth and ESRI,
#' over which movements of animals are overlain. It is hoped that this interactive experience will
#' serve both initial data checking and further investigating potential patterns in the data.
#'
#' To avoid complications with coding classes, \code{plot_leaflet_trips} is only available for the Track-family
#' of classes. So any \code{tibble::tibble} outputs if that's the way data were processed up to this
#' function, then use the conversion to the \code{Track} class before feeding into this function. The same
#' is also true for generation of trip statistics, so before to convert to the \code{Trip} class before
#' using \code{plot_leaflet_trips}. Since this current function is likely dealing with multiple animals
#' it is assumed the user would not ever want to use \code{plot_leaflet_trips} with the \code{Track2}
#' class directed at lapplying functions in open session base R.
#'
#' @param data A \code{Track} or \code{TrackStack} object; note \code{TrackMultiStack} objects not currently handled.
#' @param TagID Option for a concatenated list of TagID numbers to be subsetted from the data.
#' @param trips A \code{Trip} or \code{TripStack} object (note \code{TripMultStack}) objects not currently handled.
#' This is a dataset output from the function \code{trip_stats} function that should be run in the R session first
#' to be fed into this argument. If NULL, then the trip stats are bypassed and only one tab (a leaflet visualisation) is
#' returned in the Shiny output. If trips is not NULL in this argument, then three additional summary tabs
#' are returned (see below).
#' @param plotby A character string that must match a desired column name in the Track-family
#' object data, indicating how to label points across all animals. If NULL (default), this i
#' bypassed and colouring of points is done by TagID and a colour palette.
#' @param col A vector of colours than can be hard-coded in this argument for plotting; if
#' "col" is not equal to the length of the factor levels of plotting in argument "plotby", then an error
#' is returned; if "plotby" is NULL and "col" is also NULL, then an automatic colour palette is used for TagID.
#' @param shapes Option to add shapes to the leaflet map, as a \code{sf} POLYGON or MULTIPOLGON geometry object.
#' Defaults to NULL. This must be specified as list(shape1, shape2, ...), including  for single
#' shapes too, i.e. list(shape1).
#' @param points A \code{data.frame} of \emph{additional} xy points to be plotted on the map, i.e. this does
#' not mean whether to plot the animal telemetry points, but if separate points of another sort are needed (e.g. wind turbine locations.
#' @param radius Numeric integer of the radius of the points to plot, defaulting to 4. Note, limited
#' flexibility is currently available for colours of points (leaflet::colorFactor) and lines (grey).
#'
#' @return
#' Up to six tabs are currently available in the output. First, \strong{"Leaflet visualisation"} is the geographic
#' view of the data for animals selected, with interaction available for selecting individual
#' animals, and individual trips via their trip ID "tripNo"; a timeslide is also provided; trip
#' numbers per animal are dynamically updated for the selected animal. Animals can only
#' be selected in the leaflet plotting tab, with other tabs subsequently updating accordingly. The time slider, however,
#' only works if "All" trips are selected to simplify the dynamic reactivity.
#' If a bird's trip No is selected, and then another TagID is chosen in the other drop-down box, that doesn't have that same corresponding tripNo,
#' then the selection reverts to "All" for the next bird chosen to avoid crashes. A tab named
#' \strong{"Data spans"} is always given regardless of \code{trips == NULL}; this tab provides a \code{DT::datatable}
#' of "TagID", "start", "end" for basic information of start and end dates of the animal's
#' deployment, and "dur.days","dur.days.mon" for the total time span studied (end minus start), and
#' the total period within that span where the tag was considered "actively" monitoring, based on selections
#' of gapsections in \code{clean_GPS} or \code{gapsection}; the function \code{tabulate_history} is used
#' for this purpose, and can be run as a standalone function outside this routine. A further tab
#' \strong{"Likely sampling rates"} uses another function \code{assign_rates} to produce a tabulation of
#' most likely sampling rates of animals, using hard-coded choices between 10s and 5 hours+.
#'
#' If \code{trips != NULL}, then three further tabs also returned with more information on the metrics of individual trips. The
#' tab \strong{"Trip details"} gives the key trip metrics of all individual trips of the animal selected,
#' and if tripNos are also selected in the first tab, then only that trip is given in the second
#' tab, allowing the user to identify trips by eye, and immediately see their linked statistics.
#' Should trips not be equal to NULL, then a tab of \strong{"Trip metric distributions"} is returned
#' visualising some simple plots of histograms of the individual bird's trip duration alongside
#' all birds, further regression of trip duration and maximum distance per trip, and trip
#' duration plotted over DateTime. More could be added here so this is a work in progress. The
#' final tab \strong{"Trip stats"} then summarises the metrics across all trips for individual animals, using the function
#' \code{tabulate_trips}, as can also be used in open code. For bird-level information in tables,
#' choices selected in the leaflet map carry through to yellow highlights of that individual in
#' the other linked tables. Note \code{plot_leaflet_trips} works animal by animal and doesn't
#' view all animals together on a plot - see the simpler \code{plot_leaflet} function for that.
#'
#' # NOTE: It is anticipated this app will be updated to make use of R Shiny Bootstrap.
#'
#' @seealso [MoveRakeR::plot_leaflet], [MoveRakeR::tabulate_trips], [MoveRakeR::tabulate_history], [MoveRakeR::assign_rates]
#'
#' @references
#' Cheng, J., Karambelkar, B. & Xie, Y. (2019). leaflet: Create Interactive Web Maps with the JavaScript
#' Leaflet' Library. R package version 2.0.3. \url{https://CRAN.R-project.org/package=leaflet}
#'
#' @examples
#'
#' # I could keep going with this leaflet function, as it's possible to add other things!
#' # In particular, as can be done in MoveBank, selecting a point for deletion if it's deemed
#' # visually incorrect slipped through the speed filtering in earlier steps.
#'
#' # as always read in initial data, here, using an example at Havergate 2020 for lesser black-backed gulls
#' p4 <- 32631
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' indata <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' data_in <- indata %>% clean_GPS(.,speedfilt = TRUE, drop = FALSE) %>%
#'   define_trips(method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4)
#'
#' # get trip summary
#' trips <- trip_stats(data_in, nestfile = NULL, nest_lls=NULL, use_shape = FALSE)
#'
#' # Can now plot the data interactively - simple example for a subset of four birds, no trips selected
#' plot_leaflet_trips(data = data_in, trips = trips)
#'
#' # or without the trips, the extra tabs will not load
#' plot_leaflet_trips(data = data_in)
#'
#' # A full example, trip metrics and additional shapes and points of wind farms (NOT RUN)
#' # plot_leaflet_trips(data = data_in,  trips = data_trips, shapes = sf_shape, points = sf_points)
#'
#' # (note, individual TagIDs can be selected in the function too via TagID argument, to screen out birds not wanting shown)
#'
#' #### END ###
#'
#' @import sf
#' @importFrom sfheaders sf_multilinestring
#' @import leaflet
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable, getQueryString))
#' @import readr
#' @import DT
#' @import ggplot2
#' @export
plot_leaflet_trips <- function(data, TagID = NULL, gap = 8*60*60, gap_tol = 0.4, plotby = NULL, col = NULL, trips = NULL, shapes = NULL, points=NULL, radius = 4){

  ###################
  # Raw telemetry data

  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  if(!is.null(TagID)){
    data <- data[data$TagID %in% TagID,]
  }
  onedata <- data

  # history tabulation
  tab_hist <-  tabulate_history2(onedata)

  # as this is over gaps and we are more interested in the rates tags were on rather than overall dt gaps -
  # it would be more sensible to exclude those and evaluate rates over gapsections!
  # further option to call gapsection, work out the rates per gapsection and recombine...

  if(!is.null(gap)){

    # then call gap_section function to assign
    onedata = gap_section2(onedata, GAP = gap, tol = gap_tol)

    # and evaluate the rates by gapsection! i.e. dropping out NA's through the tidy approach of group_by()
    rates = assign_rates2(onedata, by = unique(c("year",plotby,"gapsec")), split = FALSE, group_after_gap = TRUE)

  } else{
    # business as usual
    rates = assign_rates2(onedata, by = plotby, split = FALSE, group_after_gap = TRUE)

  }

  # check if some variables exist for plotting in the leaflet popup clicker table
  # if they don't add them in the data, as otherwise it's a pain
  if(!exists("speed_2d",onedata)){onedata$speed_2d <- NA}
  if(!exists("speed_3d",onedata)){onedata$speed_3d <- NA}
  if(!exists("altitude",onedata)){onedata$altitude <- NA}
  if(!exists("altitude_agl",onedata)){onedata$altitude_agl <- NA}
  if(!exists("satellites_used",onedata)){onedata$satellites_used <- NA}

  ###################
  # Trip stat data

  if(!is.null(trips)){

    #if(is_Trip(trips)){
      onedatatrips <- trips
    #}
    #if(!is(trips, "Trip")){ # using the generic is S3 method, will check if Track, TrackStack or TrackMultiStack now
    #  stop("Trip data are not a valid Trip or TripStack object")
    #}

    # subset the trips dataframe for the birds that may be selected
    if(!is.null(TagID)){
      onedatatrips <- onedatatrips[onedatatrips$TagID %in% TagID,]
    }
  }



  #############################################
  # THE SHINY APP

  # ------------------------------ #
  # shiny ui

  if(!is.null(trips)){
    ui <- shiny::navbarPage(
      title = 'Data visualisation',
      shiny::tabPanel(title='Leaflet Map',
                      shiny::fluidPage(
                        shiny::titlePanel("Leaflet visualisation"),
                        #shiny::uiOutput("precinct"),
                        shiny::column(6,
                          shiny::uiOutput("tid")
                          ),
                        shiny::column(6,
                          shiny::uiOutput("tripNo")
                          ),
                        shiny::column(12,
                          shiny::uiOutput("Slider")
                        ),
                        leaflet::leafletOutput("mymap",width = "100%", height = 600)
                        #,
                        #shiny::actionButton("saveMap","Save map")

                        #Giving an input name and listing out types to choose in the Shiny app
                      )
      ),
      shiny::tabPanel(title='Trip details',
                      shiny::fluidRow(
                        shiny::column(
                          width=10, shiny::h1('Key trip metrics'), shiny::hr(),
                          DT::dataTableOutput('trips')

                        )
                      )
      ),
      shiny::tabPanel(title='Trip stats',
                      shiny::fluidRow(
                        shiny::column(
                          width=10, shiny::h1('Summary trip stats'), shiny::hr(),
                          DT::dataTableOutput('tripstats')

                        )
                      )
      ),
      shiny::tabPanel(title = "Trip metric distributions",
                      shiny::fluidRow(
                        title="Trip duration histogram",
                        shiny::column(
                          width=5,shiny::h2("One bird"),
                          shiny::plotOutput(outputId = "TDhist")
                        ),
                        shiny::column(
                          width=5,shiny::h2("All birds"),
                          shiny::plotOutput(outputId = "TDhist2")
                        )),
                      shiny::fluidRow(
                        title="Trip duration VS max distance",
                        shiny::column(
                          width=5,shiny::h2(""),
                          shiny::plotOutput(outputId = "xy")
                        ),
                        shiny::column(
                          width=5,shiny::h2(""),
                          shiny::plotOutput(outputId = "xy2")
                        )),
                      shiny::fluidRow(
                        title="Trip duration over time",
                        #column(
                        #   width=5,h2(""),
                        #   plotOutput(outputId = "TDtime")
                        #)
                        #,
                        shiny::column(
                          width=5,shiny::h2(""),
                          shiny::plotOutput(outputId = "TDtime2")
                        )
                      )

      ),
      shiny::tabPanel(title='Data spans',
                      shiny::fluidRow(
                        shiny::column(
                          width=10, shiny::h1('Monitoring period'), shiny::hr(),
                          DT::dataTableOutput('dataspans')

                        )
                      )
      ),
      shiny::tabPanel(title='Rates',
                      shiny::fluidRow(
                        shiny::column(
                          width=4, shiny::h1('Likely sampling rates'), shiny::hr(),
                          DT::dataTableOutput('rates')

                        ),
                        shiny::column(
                          width=5, shiny::h1('Rate distribution'), shiny::hr(),
                          shiny::plotOutput(outputId = "rates.ggplot"),
                          "Bar widths represent estimated range in potential rates the tag may have been on"
                        )
                      )
      )
    )

  } else{
    ui <- shiny::navbarPage(
      title = 'Data visualisation',
      shiny::tabPanel(title='Leaflet Map',
                      shiny::fluidPage(
                        shiny::titlePanel("Leaflet visualisation"),
                        #shiny::uiOutput("precinct"),
                        shiny::column(6,
                          shiny::uiOutput("tid")
                        ),
                        shiny::column(6,
                          shiny::uiOutput("tripNo")
                        ),
                        shiny::column(12,
                          shiny::uiOutput("Slider")
                        ),
                        leaflet::leafletOutput("mymap",width = "100%", height = 600)
                        #,
                        #shiny::actionButton("saveMap","Save map")
                        #Giving an input name and listing out types to choose in the Shiny app
                      )
      ),
      shiny::tabPanel(title='Data spans',
                      shiny::fluidRow(
                        shiny::column(
                          width=10, shiny::h1('Monitoring period'), shiny::hr(),
                          DT::dataTableOutput('dataspans')

                        )
                      )
      ),
      shiny::tabPanel(title='Rates',
                      shiny::fluidRow(
                        shiny::column(
                          width=5, shiny::h1('Likely sampling rates'), shiny::hr(),
                          DT::dataTableOutput('rates')

                        ),
                        shiny::column(
                          width=6, shiny::h1('Rate distribution'), shiny::hr(),
                          shiny::plotOutput(outputId = "rates.ggplot"),
                          "Bar widths represent estimated range in potential rates the tag may have been on"
                        )
                      )
      )
    )
  }



  # ----------------------------- #
  # shiny server
  server <- function(input, output, session){

    # check for presence of the trip column, stop if not present
    if(!exists("tripNo",where = onedata)){stop("Needs column tripNo, run function define_trips first")}

    # check if some variables exist for plotting in the leaflet popup clicker table
    # if they don't add them in the data, as otherwise it's a pain
    if(!exists("speed_2d",onedata)){onedata$speed_2d <- NA}
    if(!exists("speed_3d",onedata)){onedata$speed_3d <- NA}
    if(!exists("altitude",onedata)){onedata$altitude <- NA}
    if(!exists("altitude_agl",onedata)){onedata$altitude_agl <- NA}
    if(!exists("satellites_used",onedata)){onedata$satellites_used <- NA}

    # check for additional data rows for trips that come and go - stored in attributes from
    # define_trips(); if existing, add to data for plotting
    onedata_1 <- subset(onedata, select = c(TagID, DateTime, longitude, latitude, tripNo, speed_2d, altitude, altitude_agl, satellites_used))
    extra_dat <- attr(attr(data, "define_trips"), "extra_rows")

    if(!is.null(extra_dat)){
      extra_dat <- subset(extra_data, select = c(TagID, DateTime, longitude, latitude, tripNo, speed_2d, altitude, altitude_agl, satellites_used))
    }

    onedata_1 <- rbind(onedata_1, extra_dat)
    onedata_1 <- onedata_1[order(onedata_1$TagID, onedata_1$DateTime),]

    # if not using a specified point label column in data for point colouring, then subset
    # without that additional column, otherwise included in subsetted data
    if(!is.null(plotby)){
      onedata_ <- onedata[,which(names(onedata) == plotby)]
      onedata_1 <- cbind(onedata_1,onedata_)
      onedata_1 <- onedata_1[order(onedata_1$TagID, onedata_1$DateTime),]
      names(onedata_1)[length(onedata_1)] <- plotby
    }
    onedata <- onedata_1

    if(any(is.na(onedata$tripNo))){
      onedata$tripNo <- ifelse(is.na(onedata$tripNo),0,onedata$tripNo)
    }
    onedata <- onedata[onedata$tripNo != 0,]

    # If not using plotby then just colour up by TagID, else use the poltby column
    if(is.null(plotby)){

      if(is.null(col)){
        pal <- leaflet::colorFactor(palette = "RdYlGn", domain = onedata$TagID)
      } else{
        if(length(col) != length(onedata$TagID)){stop("Colour length should equal chosen levels, not 'plotby' so = TagID")}
        pal <- leaflet::colorFactor(palette = col, domain = onedata$TagID)

      }


    } else if(!is.null(plotby)){
      onedata[,which(names(onedata) == plotby)] <- ifelse(is.na(onedata[,which(names(onedata) == plotby)]),"NoLab",onedata[,which(names(onedata) == plotby)])

      # Order factor levels by alphabetical
      x <- data.frame(ord=1:length(unique(onedata[,which(names(onedata) == plotby)])), val = sort(unique(onedata[,which(names(onedata) == plotby)])))
      names(x)[2] <- plotby
      onedata <- merge(onedata,x,by=plotby)
      onedata <- onedata[order(onedata$TagID,onedata$DateTime),]
      onedata[,which(names(onedata) == plotby)] <- factor(onedata[,which(names(onedata) == plotby)], levels = unique(onedata[,which(names(onedata) == plotby)][order(onedata$ord)]))
      #levels(onedata$ClassName) # fine

      if(is.null(col)){
        pal <- leaflet::colorFactor(palette = "RdYlGn", domain = onedata[,which(names(onedata) == plotby)])
      } else{
        if(length(col) != length(unique(onedata[,which(names(onedata) == plotby)]))){
          stop(
            paste0(
              "Colour length = ", length(col),
              " but should equal ", plotby,
              " i.e. length ",
              length(unique(onedata[,which(names(onedata) == plotby)]))
            )
          )
        }
        pal <- leaflet::colorFactor(palette = col, domain = onedata[,which(names(onedata) == plotby)])
      }

    }

    #onedata[,which(names(onedata) == plotby)]



    # -------------------- #
    # General dynamic update of tripNo list for whatever bird is selected and time slider
    shiny::observe({

      #browser()

      if(is.null(input$TagID)){
        datasel <- onedata[onedata$TagID == unique(onedata$TagID)[1],]
      } else{
        datasel <- onedata[onedata$TagID == input$TagID,]
      }
      shiny::updateSelectInput(session=session,"tripNo", "tripNo",
                               choices = c("All",unique(datasel$tripNo)))

    })

    shiny::observeEvent(input$TagID,{

      if(is.null(input$TagID)){
        datasel <- onedata[onedata$TagID == unique(onedata$TagID)[1],]
      } else{
        datasel <- onedata[onedata$TagID == input$TagID,]
      }
      shiny::updateSliderInput(session=session,inputId = "slider", min = min(datasel$DateTime), max = max(datasel$DateTime), value = range(datasel$DateTime))

    })

    # -------------------- #
    # output renders for UI drop down selector and time slider
    #output$precinct <- shiny::renderUI({
    #  choices <- as.character(unique(onedata$TagID))
    #  tripchoices <- unique(na.omit(onedata$tripNo))
    #  #tripchoices <- c('All', tripchoices)
    #  shiny::fluidRow(shiny::column(width=6,
    #                                shiny::selectInput(inputId = "TagID", label = "TagID", choices = choices, selected = NULL)
    #  ),
    #  shiny::column(width=6,
    #                shiny::selectInput(inputId = "tripNo", label = "tripNo", choices = tripchoices, selected = "All")
    #  ),
    #  shiny::column(width=6,
    #                shiny::sliderInput(inputId = "slider", label = "Select time range", min(onedata$DateTime), max(onedata$DateTime),
    #                                   value = range(onedata$DateTime))
    #  )
    #  )
    #})

    output$tid <- shiny::renderUI({
      choices <- as.character(unique(onedata$TagID))
      shiny::selectInput(inputId = "TagID", label = "TagID", choices = choices, selected = NULL)
    })
    output$tripNo <- shiny::renderUI({
      tripchoices <- unique(na.omit(onedata$tripNo))
      shiny::selectInput(inputId = "tripNo", label = "tripNo", choices = tripchoices, selected = "All")
    })
    output$Slider <- shiny::renderUI({
      shiny::sliderInput(inputId = "slider", label = "Select time range", min(onedata$DateTime), max(onedata$DateTime),
                         value = range(onedata$DateTime), width = "100%")
    })

    # reactive values for mapshot nullify
    v <- reactiveValues()
    v$point <- NULL
    v$line <- NULL
    v$colour <- NULL
    v$TagID <- NULL
    v$radius <- radius
    v$leg.val <- NULL
    v$leg.tit <- NULL
    v$add.points <- NULL

    # ---------------------- #
    # output the base map first, so no continual redrawing of this on every plot

    map_reactive <- reactive({

      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addTiles(group = "OSM(default)", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
        leaflet::setView(lat = mean(onedata$latitude), lng = mean(onedata$longitude), zoom = 9) %>%
        leaflet::mapOptions(zoomToLimits = "never") %>%
        leaflet::addScaleBar(position = "bottomleft")

      if(!is.null(shapes)){
        # assess how many shapes, and reproject
        shapes <- lapply(shapes,function(x) sf::st_transform(x, crs = 4326))

        # add shapes
        for(i in 1:length(shapes)){
          m <- leaflet::addPolygons(map = m,data = shapes[[i]], color = "black", fill = FALSE, weight = 1, smoothFactor = 0.5,  opacity = 1.0, fillOpacity = 0.5)
        }
      }
      m
    })

    # -------------------------- #
    # For saving in mapshot
    output$mymap <- leaflet::renderLeaflet({
      map_reactive()
    })


    # -------------------------- #
    # Monitoring periods
    shiny::observeEvent({
      input$TagID

    },{

      # data.table of trip stats
      options(DT.options = list(pageLength = 10))
      output$dataspans <- DT::renderDataTable(DT::datatable(tab_hist,
                                                            rownames=FALSE,
                                                            filter=list(position="top",clear=TRUE,plain=TRUE),
                                                            options = list(
                                                              autoWidth=TRUE,
                                                              lengthChange=TRUE,
                                                              dom='tip',
                                                              columnDefs = list(list(targets = "_all", width = '50px')),
                                                              scrollX=FALSE)
      ) %>% DT::formatRound(c('Dur_days','Dur_days_mon'),3) %>%
        DT::formatStyle(
          columns = 'TagID', target = 'row',
          backgroundColor = DT::styleEqual(input$TagID,'yellow')
        )

      )

    })

    # -------------------------- #
    # Observe likely rates tag was on

    shiny::observeEvent({
      input$TagID

    },{

      #print(input$TagID)

      if(input$TagID != "All"){
        rates. <- rates[rates$TagID == input$TagID,]
        #rates. <- rates[rates$TagID == "202",]

      } else{

        rates2 = data.table::data.table(rates)
        rates2 = rates2[,lapply(.SD,sum,na.rm=TRUE), by = c("likely_rate", "start", "end"), .SDcols = "N"]

        # group all the "<X"
        rates. = rates %>% group_by(likely_rate, start, end) %>% summarise(N = sum(N), .groups = "keep") %>%
          mutate(TagID = "All") %>% relocate(TagID, .before = likely_rate)

        rates2 = rates. %>% group_by() %>%  filter(!grepl("<", likely_rate)) %>%
          mutate(likely_rate = as.numeric(likely_rate)) %>%
          arrange(likely_rate) %>%
          mutate(likely_rate = as.character(likely_rate))

        rates3 = rates.[as.numeric(rates.$end) == max(as.numeric(rates.$end)),]
        rates3$N <- sum(rates.[grepl("<", rates.$likely_rate),]$N)

        rates. <- rbind(rates2, rates3)

      }

      rates2 <- rates. %>% rename('Likely rate (s)' = "likely_rate",
                                  'Start (s)' = "start",
                                  'End (s)' = "end",
                                  'n fixes' = "N")

      #print(rates2)

      options(DT.options = list(pageLength = 12))
      output$rates <- DT::renderDataTable(DT::datatable(rates2,
                                                        rownames=FALSE,
                                                        filter="none",
                                                        options = list(
                                                          paging = FALSE,
                                                          autoWidth=TRUE,
                                                          lengthChange=TRUE,
                                                          dom='tip',
                                                          columnDefs = list(list(targets = "_all", width = '30px')),
                                                          scrollX=FALSE)
      ))

      vals <- unique(c(rates.$start,rates.$end))
      rates.$mid <- (as.numeric(rates.$end) - as.numeric(rates.$start)) / 2
      rates. <- rates. %>% mutate(likely_rate = ifelse(grepl("<", likely_rate), end, likely_rate),
                                  likely_rate = as.numeric(likely_rate),
                                  end = as.numeric(end))

      grobj <- ggplot2::ggplot(rates.,ggplot2::aes(x = mid, y = N)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = N)) +
        #geom_bar(stat = "identity", color = "black", width = rate.dist$width) +
        ggplot2::scale_x_continuous(breaks = rates.$likely_rate, limits = c(0,max(rates.$likely_rate)))
        #ggplot2::scale_x_discrete(breaks = rates.$likely_rate)

      grobj = grobj + ggplot2::labs(x = "Likely sampling rate (s)", y = "Frequency")
      grobj <- grobj + ggplot2::theme_bw()
      grobj <- grobj + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),text = ggplot2::element_text(size=15))
      #grobj <- grobj + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank(),axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
      #grobj <- grobj + ggplot2::ggtitle("Likely sampling rates")

      output$rates.ggplot <- shiny::renderPlot(grobj)

    })



    # -------------------- #
    # Seprate TAB for trip details. Updates to the map automatically update the trip details, output = "trips"
    if(!is.null(trips)){
      shiny::observeEvent({
        input$TagID
        input$tripNo

      },{

        ########
        onedatatrips$Start <- as.POSIXct(onedatatrips$Start,format = "%Y-%m-%d %H:%M:%S", tz="UTC")
        onedatatrips$End <- as.POSIXct(onedatatrips$End,format = "%Y-%m-%d %H:%M:%S", tz="UTC")

        # automatically update the TagID in the table for birds selected in the map drop down
        # (we could do this for trips as well perhaps)
        onedatatrips2 <- onedatatrips[onedatatrips$TagID == input$TagID,]

        output$TDhist <- shiny::renderPlot({
          hist(onedatatrips2$TripDur,breaks = 50, col="black",border="white",
               xlab="Trip duration (hours)", main = input$TagID)

        })
        output$TDhist2 <- shiny::renderPlot({
          hist(onedatatrips$TripDur,breaks = 50, col="black",border="white",
               xlab="Trip duration (days)", main = "All birds")

        })
        output$xy <- shiny::renderPlot(
          plot(onedatatrips2$DistMax ~ onedatatrips2$TripDur,pch=21,col="black",bg="black",
               xlab = "Trip duration (days)", ylab = "Maximum distance (km)",
               main = input$TagID)
        )
        output$xy2 <- shiny::renderPlot(
          plot(onedatatrips$DistMax ~ onedatatrips$TripDur,pch=21,col="black",bg="black",
               xlab = "Trip duration (days)", ylab = "Maximum distance (km)",
               main = "All birds")
        )

        ####
        #x <- onedatatrips
        #x$Date <- as.Date(onedatatrips$start)
        #x$TagID <- as.factor(x$TagID)
        #output$TDtime <- renderPlot(

        # ggplot(data = x,mapping=aes(x=Date,y=TripDur,color=TagID)) +
        #    geom_point(alpha=0.1) + geom_line()

        #plot(onedatatrips$TripDur ~ onedatatrips$start,pch=21,col="black",bg="black",
        #     xlab = "Trip duration (days)", ylab = "Maximum distance (km)",
        #     main = "Allbirds",type="l")
        #)

        output$TDtime2 <- shiny::renderPlot(
          plot(onedatatrips2$TripDur ~ onedatatrips2$Start,pch=21,col="black",bg="black",
               xlab = "Date", ylab = "Trip duration (days)",
               main = input$TagID,type="l")
        )




        #########

        if(input$tripNo != "All"){
          onedatatrips2a <- onedatatrips2[onedatatrips2$tripNo == input$tripNo,]
        } else(onedatatrips2a <- onedatatrips2)


        # simplfy the output
        onedatatrips3 <- data.frame(TagID = onedatatrips2a$TagID, subset(onedatatrips2a, select = c(-TotalMon,-ActiveMon,-col_lon,-col_lat,-TagID,-max_lat,-max_lon)))
        #row.names(onedatatrips3) <- 1:dim(onedatatrips3)[1]
        onedatatrips3$tripNo <- as.factor(onedatatrips3$tripNo)
        # you can do rounding directly in datatable

        # data.table of trip stats
        options(DT.options = list(pageLength = 10))
        output$trips <- DT::renderDataTable(DT::datatable(onedatatrips3,
                                                          rownames=FALSE,
                                                          filter=list(position="top",clear=TRUE,plain=TRUE),
                                                          options = list(
                                                            autoWidth=TRUE,
                                                            lengthChange=TRUE,
                                                            dom='tip',
                                                            columnDefs = list(list(targets = "_all", width = '50px')),
                                                            scrollX=FALSE)
        ) %>% # selection = "multiple",escape=FALSE,
          DT::formatRound(c('DistMax','DistTotal','MaxBear','TripDur'),3)


        ) # end rendering datatable

        output$tripstats <- DT::renderDataTable(DT::datatable(tabulate_trips2(onedatatrips)) %>%
                                                  DT::formatStyle(
                                                    columns = 'TagID', target = 'row',
                                                    backgroundColor = DT::styleEqual(input$TagID,'yellow')
                                                  )
        )


      })
    }


    # -------------------- #
    # LEAFLET MAP
    # observe events selected and update accordingly
    shiny::observeEvent({
      input$TagID
      input$tripNo
      input$slider

    },{


      ## subset the data based on the choice - not having ALL data displayed this time, one bird at a time for trip investigation
      onedata2 <- onedata[onedata$TagID == input$TagID, ]

      # if a trip for a bird gets chosen that does not exist for the bird (e.g. after being on another bird selection)
      # need to have logic to revert back to All
      if(input$tripNo != "All"){
        if(!input$tripNo %in% unique(onedata2$tripNo)){
          onedata2 <- onedata[onedata$TagID == input$TagID,]
        } else if(input$tripNo %in% unique(onedata2$tripNo)){
          onedata2 <- onedata2[onedata2$tripNo == input$tripNo,]
        }
      } else if(input$tripNo == 'All'){
        onedata2 <- onedata[onedata$TagID == input$TagID,]
      }


      # time slider
      # if tripNo gets chosen but the time slider is outside the range....default back to all trips for the bird
      if(input$slider[1] < min(onedata2$DateTime,na.rm=TRUE) | input$slider[2] > max(onedata2$DateTime,na.rm=TRUE)){
        if(input$tripNo != 'All' & input$tripNo %in% unique(onedata2$tripNo)){
          onedata2 <- onedata2[onedata2$tripNo == input$tripNo,]
        }
        if(!input$tripNo %in% unique(onedata2$tripNo) & input$tripNo != 'All'){
          onedata2 <- onedata[onedata$TagID == input$TagID,]
          onedata2 <- onedata2[onedata2$DateTime >= input$slider[1] & onedata2$DateTime <= input$slider[2],]

        }
      } else if(input$tripNo == 'All'){
        onedata2 <- onedata[onedata$TagID == input$TagID, ]
        onedata2 <- onedata2[onedata2$DateTime >= input$slider[1] & onedata2$DateTime <= input$slider[2],]

      }

      ##### too complex to update slider again for trips, for now
      #updateSliderInput(session=session,inputId = "slider", min = min(onedata2$DateTime), max = max(onedata2$DateTime), value = range(onedata2$DateTime))

      #######
      z <- data.frame(long = onedata2$longitude, lat = onedata2$latitude)
      y <- sfheaders::sf_linestring(z, x = "long", y = "lat")


      # Legend - we ideally want only the one legend for the bird selected in this case
      # as not all birds are on the same

      if(is.null(plotby)){
        VAL <- onedata2$TagID # if TAGID then we only want one colour legend loaded
        TITLE <- "TagID"
      } else{
        VAL <- onedata[,which(names(onedata) == plotby)] # maybe levels for some individuals not present so keep full colur range from all data
        TITLE <- plotby
      }

      # LEAFLET COLOURING OF POINTS - whether custom colours or colour palette defined earlier
      if(is.null(plotby)){
        COL <- pal(onedata2$TagID)
        POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                       "<b>","Trip Number:", "</b>", onedata2$tripNo, "<br>",
                       "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                       "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                       "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                       "<b>","Altitude (m):", "</b>", onedata2$altitude_agl, "<br>",
                       "<b>","Satellites used:", "</b>", onedata2$satellites_used, "<br>",
                       "<b>","Speed (m/s):","</b>", round(onedata2$speed_2d,5))
      } else if(!is.null(plotby)){
        COL <- pal(onedata2[,which(names(onedata2) == plotby)])
        POPUP <- POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                                "<b>","Trip Number:", "</b>", onedata2$tripNo, "<br>",
                                "<b>",paste0(plotby,":"), "</b>", onedata2[,which(names(onedata2) == plotby)], "<br>",
                                "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                                "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                                "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                                "<b>","Altitude (m):", "</b>", onedata2$altitude_agl, "<br>",
                                "<b>","Satellites used:", "</b>", onedata2$satellites_used, "<br>",
                                "<b>","Speed (m/s):","</b>", round(onedata2$speed_2d,5))
      }


      ## plot the subsetted data
      leaflet::leafletProxy("mymap") %>%
        leaflet::clearMarkers() %>%
        leaflet::clearControls() %>%
        leaflet::addPolylines(data = y, weight = 2, color = "grey", opacity = 0.5, fillOpacity = 0.5, layerId = "foo") %>%
        leaflet::addCircleMarkers(lat = onedata2$latitude, lng = onedata2$longitude,
                                  radius= radius, fillOpacity = 0.5, stroke = FALSE, color=COL,
                                  popup = POPUP) %>%
        leaflet::addLegend(pal = pal, values = VAL, opacity = 1, title = TITLE)


      if(!is.null(points)){
        points <- lapply(points,function(x){
          names(x) <- tolower(names(x))

          if(any(!c("latitude","longitude") %in% names(x))) { # any NOT TRUE?
            stop("needs latitude and longitude named columns")
          }

          x <- subset(x,select=c(latitude,longitude))

          return(x)
        })

        points <- do.call('rbind',points)

        v$add.points <- points # save as reactive for mapshot output

        # points to add to the map? assess how many sets of points, and check for named lat long colums
        leaflet::leafletProxy("mymap")  %>% leaflet::addCircleMarkers(lat = points$latitude, lng = points$longitude, radius= radius, fillOpacity = 1, color = "black")

      }

    })


  }

  shiny::shinyApp(ui = ui, server = server)

}







