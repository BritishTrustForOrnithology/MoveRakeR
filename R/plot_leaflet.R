#' Interactive leaflet map for visualising tracking data
#'
#' @description
#' \code{plot_leaflet} subsets a Track or TrackStack object for TagIDs and feeds this through to
#' a ui and server plotting an interactive shiny app.
#'
#' @details
#' The map works using an R Shiny App, and makes use of efficient processing to update
#' the base map rather than plotting afresh and as a preferCanvas option to increase speed.
#' However, the function is still slow for lots of data. Three different basemaps are
#' also implemented being GoogleEarth, OSM and ESRI. Note, currently no saving still grabs
#' of images are coded. The plot appears in a tab in the shiny app, and more tabs are planned
#' to summarise the the data etc in other views. Points of multiple birds are also coloured
#' automatically using a simple colour palette, but no custom coloration is yet coded. A save
#' option is also now available beneath the leaflet map so the user can take a snapshot of the
#' map to be saved to the global working directory as in \code{getwd()}.
#'
#' To avoid complications with coding classes, \code{plot_leaflet} is only available for the Track-family
#' of classes. So any \code{tibble::tibble} outputs if that's the way data were processed up to this
#' function, then use the conversion to the \code{Track} class before feeding into this function.
#'
#' It is anticipated this app will be updated to make use of R Shiny Bootstrap.
#'
#' @param data A \code{Track} or \code{TrackStack} object; note TrackMultiStack objects not currently handled.
#' @param TagID Whether to subset TagIDs for the data fed in. It is advisable to
#' consider this for large datasets, or reduce the time span of the data used, as very large
#' numbers of points are not well handled. TagID should be a concatenated vector.
#' @param ud for plotting KDEs on the map; additional toggle will appear in the shiny app for individuals.
#' However, please note, this is not currently available in \code{MoveRakeR} as the
#' a separate function not currently coded in the package \code{compute_vertices} is also needed that uses the
#' "estUD" classes of individual animals output from the \code{adehabitatHR::kernelUD} workflow; get in touch
#' if interested. Function \code{compute_vertices} creates a vertices class
#' of contours from the UD that can then be plotted on the leaflet map through the UD argument in
#' \code{plot_leaflet}.
#' @param plotby A character string that must match a desired column name in the Track-family
#' object data, indicating how to label points across all animals. If NULL (default), this i
#' bypassed and colouring of points is done by TagID and a colour palette.
#' @param col A vector of colours than can be hard-coded in this argument for plotting; if
#' "col" is not equal to the length of the factor levels of plotting in argument "plotby", then an error
#' is returned; if "plotby" is NULL and "col" is also NULL, then an automatic colour palette is used for TagID.
#' @param shapes Option to add shapes to the leaflet map, as a \code{sf} POLYGON or MULTIPOLGON geometry object.
#' Defaults to NULL. This must be specified as list(shape1, shape2, ...), including  for single
#' shapes too, i.e. list(shape1).
#' @param points Option to add additional points to the map, as with the shapes argument, should
#' be specified as a list even if just one set of points. The function expects "latitude" and "longitude" columns
#' in the input points argument per layer, each specified as a data.frame object, but the function
#' automatically converts text of the column names to lower case.
#' @param pcol A colour for which to plot the additional points added to the map, defaulting to "black".
#' @param scol A colour for which to plot the additional polygon outlines added to the map, defaulting to "black". Note
#' currently shape fills are not coded.
#' @param plot_w Value for plotting width of leaflet device.
#' @param plot_h Value for plotting height of leaflet device.
#' @param radius Numeric integer of the radius of the points to plot, defaulting to 4.
#' @param legend Logical defaulting to TRUE as to whether a legend is to be displayed for telemetry point classifications.
#' @param lines4fixes Another logical argument as to whether to draw the lines between fixes, defaults to TRUE.
#' Ideally this should be another button in the shiny app to toggle but it is not yet implemented that way.
#' @param fixes Another logical as to displaying options, TRUE/FALSE, for the telemetry fixes, defaulting to TRUE.
#'
#' @return
#' A local browser window with the Shiny app deployed.
#'
#' @seealso @seealso [MoveRakeR::plot_leaflet_trips]
#'
#' @references
#' Cheng, J., Karambelkar, B. & Xie, Y. (2019). leaflet: Create Interactive Web Maps with the JavaScript
#' Leaflet' Library. R package version 2.0.3. \url{https://CRAN.R-project.org/package=leaflet}
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' plot_leaflet(data)
#'
#' # Note there are also options to bring in UDs and vertices drawn as polygons but these are
#' # at the moment under development in MoveRakeR, and involve code currently not publicly available
#'
#' p4s = 32631
#' k_prep = kernelUD_prep(data, p4s = p4s)
#' xy <- kernelUD_grid(data = data,  Choice=20, res=500, ADJVAL = 20000, p4s = p4s)
#' UD <- adehabitatHR::kernelUD(k_prep, h = "href", grid=xy, same4all = FALSE, kern = "bivnorm")
#' vers <- compute_vertices(ud = UD, vertices = c(95,75,50), geo_transform = TRUE)
#' plot_leaflet(data, shapes = list(ukmap), ud = vers, radius = 1, col = "black", legend = TRUE)
#'
#' @export
plot_leaflet <- function(data, TagID = NULL, ud = NULL, plotby = NULL, col = NULL,
                         plot_w = "100%", plot_h = "75vh", shapes = NULL,
                         pcol = "black", scol = "black", points=NULL, radius = 4, legend = TRUE,
                         lines4fixes = TRUE, fixes = TRUE){


  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  if(!is.null(shapes)){
    shapes <- lapply(shapes,function(x) sf::st_transform(x, 4326))
  }

  # ------------------------------ #
  # UD trial
  # adding on polygons of a UD of one or several animals
  # TagID MUST match the named list of birds though otherwise impossible to keep track of in inputs

  # for this we will have to have shaded polys I think

  #ud = vers

  if(!is.null(ud)){
    ##### plot_leaflet will ONLY accept a vertices computed KDE
    ##### of class "vertices"

    # check class

    # check if the ud is a list, and if so, do all the individual elements have a tagid that MATCHES every element
    # in the data? OR....is that an issue and if it doesn't then when it comes
    # to adding points over the top, don't allow it?

    if(is.list(ud)){

      if(!"vertices" %in% class(ud[[1]])){
        stop("Only accepts a vertices class: likely you are trying to feed a full UD? Run function compute_vertices first")
      }

      if(is.list(ud[[1]][[1]])){
        stop("Double nested lists not coded!")
      }

    } else if(!is.list(ud)){

      if(!"vertices" %in% class(ud)){
        stop("Only accepts a vertices class: likely you are trying to feed a full UD? Run function compute_vertices first")
      }

    }

  }

  ##### also for the shapes - need a custom way of either providing the set colours
  ##### in function argument or have a way of dynamically selecting them from ANOTHER
  ##### dropdown

  # ------------------------------ #
  # check if some variables exist for plotting in the leaflet popup clicker table
  # if they don't add them in the data, as otherwise it's a pain
  if(!exists("speed_2d",data)){data$speed_2d <- NA}
  if(!exists("speed_3d",data)){data$speed_3d <- NA}
  if(!exists("altitude",data)){data$altitude <- NA}
  if(!exists("altitude_agl",data)){data$altitude_agl <- NA}
  if(!exists("satellites_used",data)){data$satellites_used <- NA}

  onedata <- data
  #################################
  ############ shiny ui ###########
  #################################

  #ui <- shiny::navbarPage(
  #  title = 'Data visualisation',
  #  shiny::tabPanel(title='Leaflet Map',
  #      shiny::fluidPage(
  #        shiny::titlePanel("Leaflet visualisation"),
  #        leaflet::leafletOutput("mymap",width = plot_w, height = plot_h),
  #        shiny::hr(),
  #        shiny::fluidRow(
  #          shiny::column(6,
  #            shiny::uiOutput("tid"),
  #            shiny::uiOutput("shape"),
  #            shiny::uiOutput("UD")
  #          ),
  #          shiny::column(6,
  #           shiny::uiOutput("ltog"),
  #           shiny::uiOutput("ptog"),
  #           shiny::uiOutput("clear")
  #          )
  #        ),
  #        shiny::column(12,
  #                      #shiny::column(12,
  #                      shiny::uiOutput("Slider")
  #                      #)
  #        )
  #        #shiny::uiOutput("shapecol"),
  #      #Giving an input name and listing out types to choose in the Shiny app
  #    )
  #  )
  #)

  ui <- dashboardPage(
    dashboardHeader(title = "Data visualisation"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Leaflet Map", tabName = "leaflet_tab", icon = icon("map"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "leaflet_tab",
                # Map title
                h3("Leaflet visualisation"),
                # Map output
                leafletOutput("mymap", width = plot_w, height = plot_h),
                hr(),
                # Controls row
                fluidRow(
                  column(6,
                         uiOutput("tid"),
                         uiOutput("shape"),
                         uiOutput("UD")
                  ),
                  column(6,
                         uiOutput("ltog"),
                         uiOutput("ptog"),
                         uiOutput("clear")
                  )
                ),
                # Slider full-width
                fluidRow(
                  column(12,
                         uiOutput("Slider")
                  )
                )
        )
      )
    )
  )


  #################################
  ########## main server ##########
  #################################

  # shiny server
  server <- function(input, output, session){

    ###############################
    ##### animal GPS data stuff ###
    ###############################

    # check if some variables exist for plotting in the leaflet popup clicker table
    # if they don't add them in the data, as otherwise it's a pain
    if(!exists("speed_2d",onedata)){onedata$speed_2d <- NA}
    if(!exists("speed_3d",onedata)){onedata$speed_3d <- NA}
    if(!exists("altitude",onedata)){onedata$altitude <- NA}
    if(!exists("altitude_agl",onedata)){onedata$altitude_agl <- NA}
    if(!exists("satellites_used",onedata)){onedata$satellites_used <- NA}

    # if only altitude is provided not _agl...
    if(exists("altitude",onedata) & !exists("altitude_agl",onedata)){onedata$altitude_agl <- onedata$altitude}

    # if not using a specified point label column in data for point colouring, then subset
    # without that additional column, otherwise included in subsetted data
    if(is.null(plotby)){
      #onedata <- subset(onedata, select = c(TagID, DateTime, longitude, latitude, tripNo, speed_2d, altitude, altitude_agl, satellites_used))
      onedata <- subset(onedata, select = c(TagID, DateTime, longitude, latitude, speed_2d, altitude, altitude_agl, satellites_used))

    } else{
      #onedata. <- subset(onedata, select = c(TagID, DateTime, longitude, latitude, tripNo, speed_2d, altitude, altitude_agl, satellites_used))
      onedata. <- subset(onedata, select = c(TagID, DateTime, longitude, latitude, speed_2d, altitude, altitude_agl, satellites_used))
      onedata.. <- onedata[,which(names(onedata) == plotby)]
      onedata <- cbind(onedata.,onedata..)
      names(onedata)[length(onedata)] <- plotby
    }

    # If not using plotby then just colour up by TagID, else use the poltby column
    if(is.null(plotby)){

      if(is.null(col)){
        pal <- leaflet::colorFactor(palette = "RdYlGn", domain = onedata$TagID)
      } else{

        if(length(col) == 1){col <- rep(col,length(unique(onedata$TagID)))}

        if(length(col) != length(unique(onedata$TagID))){stop("Colour length should equal chosen levels, not 'plotby' so = TagID")}
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


    ######################################################
    ############## USER INTERFACE ELEMENTS ###############
    ######################################################

    #########################
    ######### clear #########
    #########################
    output$clear <- shiny::renderUI({

       htmltools::tagList(
        shiny::actionButton(inputId = "clear", label = "Clear")

      )
    })

    #########################
    #### POINT TOGGLE #######
    #########################

    output$ptog <- shiny::renderUI({
      htmltools::tagList(
        shinyWidgets::switchInput(inputId = "ptog", label = "Fixes", value = TRUE, size = "small", handleWidth = '100px', offLabel = "Off")
        #shinyWidgets::awesomeCheckbox(
        #  inputId = "ptog",
        #  label = "Fixes",
        #  value = TRUE,
        #  status = "danger"
        #)
      )
    })

    #########################
    ##### LINE TOGGLE #######
    #########################

    output$ltog <- shiny::renderUI({
      htmltools::tagList(
        shinyWidgets::switchInput(inputId = "ltog", label = "Lines", value = TRUE, size = "small", handleWidth = '100px', offLabel = "Off")
        #shinyWidgets::awesomeCheckbox(
        #  inputId = "ltog",
        #  label = "Lines",
        #  value = TRUE,
        #  status = "danger"
        #)
      )
    })



    #########################
    ######## precinct #######
    #########################

    #if(is.null(plotby)){
    #  output$precinct <- shiny::renderUI({
    #    choices <- as.character(unique(onedata$TagID))
    #    choices <- c('All', choices)
    #    htmltools::tagList(
    #      shiny::selectInput(inputId = "TagID", label = "TagID", choices = choices, selected = "All"),
    #      shiny::sliderInput(inputId = "slider", label = "Select time range", min(onedata$DateTime), max(onedata$DateTime),
    #                         value = range(onedata$DateTime))
    #    )
    #  })
    #}
    #if(!is.null(plotby)){
    #  output$precinct <- shiny::renderUI({
    #    choices <- as.character(unique(onedata[,which(names(onedata) == plotby)]))
    #    choices <- c('All', choices)
    #    htmltools::tagList(
    #      shiny::selectInput(inputId = "TagID", label = plotby, choices = choices, selected = "All"),
    #      shiny::sliderInput(inputId = "slider", label = "Select time range", min(onedata$DateTime), max(onedata$DateTime),
    #                         value = range(onedata$DateTime))
    #    )
    #  })
    #}

    output$tid <- shiny::renderUI({

      if(is.null(plotby)){
        choices <- as.character(unique(onedata$TagID))
        label = "TagID"
      } else{
        choices <- as.character(unique(onedata[,which(names(onedata) == plotby)]))
        label = plotby
      }
      choices <- c('All', choices)
      shiny::selectInput(inputId = "TagID", label = label, choices = choices, selected = "All")

    })

    output$Slider <- shiny::renderUI({

       shiny::sliderInput(inputId = "slider", label = "Select time range", min(onedata$DateTime), max(onedata$DateTime), value = range(onedata$DateTime), width = "100%")

    })


    #########################
    ######### shapes ########
    #########################

    ##### idea for reactive shape display

    if(!is.null(shapes)){
      output$shape <- shiny::renderUI({

        shpno <- length(shapes)
        choicesS <- paste("shape",1:shpno)
        choicesS <- c('All', choicesS)
        htmltools::tagList(
          shiny::selectInput(inputId = "shape", label = "shape", choices = choicesS, selected = "All")
        )
      })
    }

    #########################
    ########### UD ##########
    #########################

    ##### idea for the KDE plotting with a supplied UD

    if(!is.null(ud)){
      output$UD <- shiny::renderUI({

        if(is.list(ud[[1]])){
          # then we have many vertices to plot (i.e. a list of listed vertices)

          # if the supplied ud is a list, then take NAME of the UD for the drop down
          # if no name is provided then assign animal_1... _2 etc
          # but note that will then NOT match any data necessarily if you want to add points over top

          if(is.null(names(ud))){
            an_nm <- paste("Animal_",1:length(ud),sep="")
            names(ud) <- an_nm # then assign them to the UD
          }
          if(!is.null(names(ud))){
            an_nm <- names(ud)
          }

          # make the first animal that selected
          #selected <- names(ud)[1]
        }

        if(!is.list(ud[[1]])){
          # then we have one vertices to plot (i.e. a single set of vertices in a list)
          if(is.null(names(ud))){
            an_nm <- "OneUD"
            names(ud) <- an_nm # then assign them to the UD
           }

          if(!is.null(names(ud))){
            an_nm <- names(ud)
          }
          #selected <- an_nm
        }

        # note no ALL option! BUT WE NEED A "none"
        an_nm <- c(an_nm, "none")
        selected <- "none"

        htmltools::tagList(
          shiny::selectInput(inputId = "UD", label = "UD", choices = an_nm, selected = selected)
        )
    })
    }


    ##### NULLIFY "v" choices for saving map, set up blank values
    #map <- reactiveValues(dat = 0)

    v <- reactiveValues()
    v$point <- NULL
    v$colour <- NULL
    v$TagID <- NULL
    v$shape <- NULL
    v$ud <- NULL
    v$line <- NULL
    v$ltog <- TRUE
    v$ptog <- TRUE
    v$radius <- radius
    v$add.points <- NULL
    #v$add.points.popup <- NULL # not needed
    v$leg.val <- NULL
    v$leg.tit <- NULL
    v$update <- 0 # whether to update map if time slider returns no data

    ######################################################
    ############### REACTIVE MAP ELEMENTS ################
    ######################################################

    map_reactive <- reactive({
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addTiles(group = "OSM(default)", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
        leaflet::addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
        leaflet::setView(lat = mean(onedata$latitude), lng = mean(onedata$longitude), zoom = 9) %>%
        leaflet::mapOptions(zoomToLimits = "never") %>%
        leaflet::addScaleBar(position = "bottomleft")


      if(legend == TRUE){
        m = leaflet::addLegend(map = m, pal = pal, values = onedata$TagID, opacity = 1, title = "TagID")
      }

      m
    })


   #### initial reactive map set up for output
   output$mymap <- leaflet::renderLeaflet({
     map_reactive()
    })

   # --------------------------- #
   # CLEAR MAP
   # --------------------------- #
   layerId2 = paste("foo",1:(length(shapes)+1))

   shiny::observeEvent({
     input$clear
   },{
     leaflet::leafletProxy("mymap") %>%
       leaflet::clearMarkers() %>%
       leaflet::clearControls() %>%
       leaflet::removeShape(layerId = c("1","2","3","foo",paste0("line_foo", 1:10000)))

   })

   # --------------------------- #
   # Points, slider
   # --------------------------- #

   #### first observe event of TagID, slider AND the shapes now (still need to do additional point layer in same way)
    shiny::observeEvent({
      input$TagID
      input$slider
      input$ltog
      input$ptog

    },{

      #####################################################
      ## plot the subsetted data, clear previous points and lines only (not polys)
      leaflet::leafletProxy("mymap") %>%
        leaflet::clearMarkers() %>%
        leaflet::clearControls() %>%
        leaflet::removeShape(layerId = paste0("line_foo", 1:10000))

      #if(is.null(td)) return()
      ## get the choice from the drop-down box
      # This could be the TagID if plotby is NULL but if custom selected column, needs to reflect that
      TAGID = input$TagID

      if(is.null(plotby)){
        ## supbset the data based on the choice
        if(TAGID != 'All'){
          onedata2 <- onedata[onedata$TagID == TAGID, ]
        }else{
          onedata2 <- onedata
        }
      }


      if(!is.null(plotby)){
        ## subset the data based on the choice
        if(TAGID != 'All'){
          onedata2 <- onedata[onedata[,which(names(onedata) == plotby)] == TAGID,]
        }else{
          onedata2 <- onedata
        }
      }


      onedata2 <- onedata2[onedata2$DateTime >= input$slider[1] & onedata2$DateTime <= input$slider[2],]

      #print(onedata2)

      if(dim(onedata2)[1] > 0){

        if(lines4fixes){

          ### LINES FROM THE POINTS SELECTED
          # **** this is so much simpler in sf... no splitting of lines in SpatialLines etc etc ****

          #z <- data.frame(long = onedata2$longitude, lat = onedata2$latitude)
          z <- droplevels(data.frame(long = onedata2$longitude, lat = onedata2$latitude, DateTime = onedata2$DateTime, TagID = onedata2$TagID))

          z <- z[order(z$TagID, z$DateTime),]
          #browser()

          ## sfheaders for multilines
          y <- sfheaders::sf_multilinestring(z, x = "long", y = "lat", multilinestring_id = "TagID")

        } else{
          y <- NULL
        }

        ### if the input toggle ltog is FALSE, then set lines to NULL, i.e. turn off
        if(!input$ltog){
          yy <- NULL
        } else{yy <- y}

        if(is.null(plotby)){
          VAL <- onedata2$TagID # if TAGID then we only want one colour legend loaded
          TITLE <- "TagID"
        } else{
          VAL <- onedata2[,which(names(onedata2) == plotby)] # nah.... :::>>> maybe levels for some individuals not present so keep full colur range from all data
          TITLE <- plotby
        }


        # LEAFLET COLOURING OF POINTS - whether custom colours or colour palette defined earlier
        if(is.null(plotby)){
          COL <- pal(onedata2$TagID)
          POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                         #"<b>","Trip Number:", "</b>", onedata2$tripNo, "<br>",
                         "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                         "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                         "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                         "<b>","Altitude (m):", "</b>", onedata2$altitude_agl, "<br>",
                         "<b>","Satellites used:", "</b>", onedata2$satellites_used, "<br>",
                         "<b>","Speed (m/s):","</b>", round(onedata2$speed_2d,5))
        } else if(!is.null(plotby)){
          COL <- pal(onedata2[,which(names(onedata2) == plotby)])
          POPUP <- POPUP <- paste("<b>", onedata2$TagID,"</b>", "<br>",
                                  #"<b>","Trip Number:", "</b>", onedata2$tripNo, "<br>",
                                  "<b>",paste0(plotby,":"), "</b>", onedata2[,which(names(onedata2) == plotby)], "<br>",
                                  "<b>","DateTime:", "</b>", onedata2$DateTime, "<br>",
                                  "<b>","Latitude:", "</b>", round(onedata2$latitude,5), "<br>",
                                  "<b>","Longitude:", "</b>", round(onedata2$longitude,5), "<br>",
                                  "<b>","Altitude (m):", "</b>", onedata2$altitude_agl, "<br>",
                                  "<b>","Satellites used:", "</b>", onedata2$satellites_used, "<br>",
                                  "<b>","Speed (m/s):","</b>", round(onedata2$speed_2d,5))
        }

        ### if the input toggle ptog is FALSE, then set points to NULL, i.e. turn off
        if(!input$ptog){
          pp <- NULL
        } else{
          pp <- data.frame(latitude = onedata2$latitude, longitude = onedata2$longitude)
        }

        # SEPARATE OUTPUT LAYER FOR SAVING
        v$point = pp
        v$colour = COL
        v$TagID <- TAGID
        v$line <- yy
        v$ltog <- input$ltog
        v$ptog <- input$ptog
        v$leg.val <- VAL
        v$leg.tit <- TITLE
        v$update <- 1


        #leaflet::leafletProxy("mymap") %>%
        #  leaflet::removeControl(layerId = "GPS_points")


        # if lines for fixes selected in function by user AND if the toggle is TRUE
        if(lines4fixes){
          if(!is.null(yy)){

            #m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
            #  leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
            #  leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
            #  leaflet::addTiles(group = "OSM(default)", options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE)) %>%
            #  leaflet::addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
            #  leaflet::setView(lat = mean(onedata2$latitude), lng = mean(onedata2$longitude), zoom = 9) %>%
            #  leaflet::mapOptions(zoomToLimits = "never") %>%
            #  leaflet::addScaleBar(position = "bottomleft")

            layerID <- paste0("line_foo",1:nrow(yy))

            #mm <- m %>% leaflet::addPolylines(data = y, weight = 2, color = "grey", opacity = 0.5, fillOpacity = 0.5, layerId = layerID)
            #mm %>% leaflet::removeShape(layerId = layerID)

            leaflet::leafletProxy("mymap") %>%
            leaflet::addPolylines(data = yy, weight = 2, color = "grey", opacity = 0.5, fillOpacity = 0.5, layerId = layerID)

          } else{
            leaflet::leafletProxy("mymap") %>%
              leaflet::removeShape(layerId = "foo") %>%
              leaflet::removeShape(layerId = paste0("line_foo", 1:10000))

          }

        }


        # if fixes themselves to be displayed from selection in function by user AND if the toggle is TRUE
        if(fixes == TRUE){
          if(input$ptog){
            leaflet::leafletProxy("mymap") %>%
            leaflet::addCircleMarkers(lat = onedata2$latitude, lng = onedata2$longitude,
                                    radius= radius, fillOpacity = 0.5, stroke = FALSE, color=COL,
                                    popup = POPUP)
          }
        }

        if(legend == TRUE){
          leaflet::leafletProxy("mymap") %>%
            leaflet::addLegend(pal = pal, values = VAL, opacity = 1, title = TITLE)
        }

      } else {v$update <- 0} # ONLY PLOT IF THE TIME SLIDER HAS SELECTED DATA

      # points to add to the map? assess how many sets of points, and check for named lat long colums
      if(!is.null(points)){

        ### removing this as now we build latitude longitude directly from the
        ### sf so only need to make sre is 4326

        #points <- lapply(points,function(x){
        #  names(x) <- tolower(names(x))

          #if(any(!c("latitude","longitude") %in% names(x))) { # any NOT TRUE?
          #  stop("needs latitude and longitude named columns")
          #}

          #x$cc = paste(x$longitude, x$latitude, sep = "_")
          #
          #if(any(duplicated(x$cc))){
          #
          #  warning("Duplicate GPS point location")
          #  # removing duplicate point
          #
          #} NAH LEAVE AS IS

          #x <- subset(x,select=c(latitude,longitude))

        #  return(x)
        #})

        # if retaining all columns beyong lat long, then
        # need separate point plots and popups per list of points
        # previously was rbinded together so much simpler

        #points <- do.call('rbind',points)

        #browser()

        POPUP_POINTS <- list()
        p = 1
        for(p in 1:length(points)){

          # drop geometry from sf? this isn't ideal - should be plotting directly below....

          if( inherits(points[[p]], "sf") ){
            coords <- sf::st_coordinates(points[[p]])
            points[[p]]$longitude <- as.vector(coords[,1])
            points[[p]]$latitude <- as.vector(coords[,2])
            points[[p]] <- sf::st_drop_geometry(points[[p]])
          }

          # first do the lat long that should be present given check above
          POPUP_POINTS[[p]] <- paste(
                                "<b>","Latitude:", "</b>", round(points[[p]]$latitude,5), "<br>",
                                "<b>","Longitude:", "</b>", round(points[[p]]$longitude,5), "<br>"

                                )


          # Assess extra columns to add to popup
          nms = names(points[[p]])
          nms = nms[!nms %in% c("latitude","longitude")]

          # cycle over and add to the popup
          n = 1
          for(n in 1:length(nms)){

            w = which(names(points[[p]]) == nms[n])

            col = as.data.frame(points[[p]][,w])

            if(class(col[[1]]) == "numeric"){
              col <- round(col,5)
            }

            POPUP_POINTS[[p]] <- paste(POPUP_POINTS[[p]],paste("<b>",names(col),"</b>", col[[1]], "<br>"))
          }


        }

        v$add.points <- points # save as reactive for mapshot output
        #v$add.points.popup <- POPUP_POINTS # NOT NEEDED FOR OUTPUT save popup for each list of points

        # could do in loop above but keeping separate for clarity
        # cycle over each points list separately i.e. given they may have different popups of columns etc
        for(p in 1:length(points)){

          leaflet::leafletProxy("mymap")  %>%
            leaflet::addCircleMarkers(lat = points[[p]]$latitude,
                                      lng = points[[p]]$longitude,
                                      radius= radius,
                                      fillOpacity = 1,
                                      color = pcol,
                                      popup = POPUP_POINTS[[p]])

        }



      }


      })



    # --------------------------- #
    # shapes
    # --------------------------- #


    shiny::observeEvent({
      input$shape
    },{

      # # # # # # # #
      # subset the shape selected

      if(!is.null(shapes)){ # i.e. from the original function specification

        SHAPE = input$shape # user selected
        #SHAPE <- "Shape 1"


        if(SHAPE != "All"){
          shpn <- as.numeric(strsplit(SHAPE, " ")[[1]][2]) # get the numeric value of order of shape
          shapes2 <- shapes[[shpn]]

          poly_col <- "black"
          layerId = paste("foo",shpn)

        } else{ # ALL
          ## old rgeos:
          #shapes2 <- rgeos::gUnaryUnion(do.call('rbind',shapes))

          ## new sf:
          shapes2 <- sf::st_union(do.call('rbind',shapes))

          poly_col <- "black"
          layerId <- paste0("foo ",length(shapes)+1)


        }



        # if the input selected is a specific shape, just add that shape
        leaflet::leafletProxy("mymap")  %>%
          leaflet::removeShape(layerId = layerId2) %>%
          leaflet::addPolygons(data = shapes2,
                               color = poly_col,
                               fill = FALSE,
                               weight = 1,
                               smoothFactor = 0.5,
                               opacity = 1.0,
                               fillOpacity = 0.5, layerId = layerId)


      }

      # # # # # # # #

    })

    # --------------------------- #
    # KDEs
    # --------------------------- #

    #### separate observeEvent for the KDEs - plot on polygons - with opaqueness (can also be toggled?)

    shiny::observeEvent({
      input$UD
      #input$TagID
    },{

      # if the ud is a list, then ...

      ud_nm = input$UD # user selected, and will always be a list of the vertices from "compute_vertices"

      if(ud_nm != "none"){

        ud2 <- ud[names(ud) == ud_nm][[1]] # should always be a list
        ud3 <- do.call('rbind',ud2)

        # if the input selected is a specific shape, just add that shape
        leaflet::leafletProxy("mymap")  %>%
          leaflet::addPolygons(data = ud3,
                               color = c("royalblue","yellow","red"),
                               fill = c("royalblue","yellow","red"),
                               weight = 1,
                               smoothFactor = 0.5,
                               opacity = 0.5,
                               fillOpacity = 0.5, layerId = c("1","2","3"),
                               #options = list(o1,o2,o3)
          )

      }

      if(ud_nm == "none"){
        leaflet::leafletProxy("mymap")  %>%
          leaflet::removeShape(layerId = c("1","2","3"))


      }


      v$ud <- ud_nm


    })



  }

  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}
















