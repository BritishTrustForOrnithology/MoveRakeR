#' Add numbered trips to telemetry data based on a central place
#'
#' \code{define_trips} adds a further column to the tracking dataset containing a numbered
#' integer representing sequential trips/visits within the data, to and from a defined central place.
#'
#' @details
#' For studies focused on the breeding season of animals, and where study species adopt
#' central place foraging activities, it is common for tracking studies to ask questions
#' such as how far did individuals travel or how long did they spend, on their excursions. A first
#' step in answering those questions is to label positional fixes as "on a trip" or "at the colony"
#' and number them accordingly for further analysis. That is the main purpose of \code{define_trips}.
#'
#' In the case of birds, a central place is of course at the finest scale, the individual nest,
#' or local territory, but, particularly for colonial species, this definition may need expansion to a wider
#' area, such as a colony, or wider area over which animals may start and end trips.
#' \code{define_trips} provides functionality to compute trips based on a predefined
#' area - here coded in the form of a rectangle, circle, GIS polygon using \code{sf} \code{POLGON} feature geometry,
#' but further flexibility is introduced to allow a separated \code{flag} column identifying
#' any means the user may have of distinguishing time at the central place or away on trips.
#' Further R packages may also be of interest to identify the nest location of individual animals,
#' including \code{trip} (Sumner et al. 2009, Sumner 2011),
#' \code{recurse} (Bracis et al. 2018), and \code{nestR} (Picardi et al. 2020) - see also \code{ExMove} for
#' a similar workflow.
#'
#' Finally, a small but very important detail about the \code{define_trips} function. Animals may
#' have a tendency to arrive back from a trip, and then potentially immediately depart again, thus sharing a start and end time,
#' resulting in complications downstream when defining trip metrics. This also depends on how the central
#' place is defined and sampling intervals of the tag.
#' To make sure all tripNos have a start and end, where that arrive-re-depart situation above arises,
#' \code{define_trips} adds in an extra row for the trip with all other data NA'd so that all trips have start and end times.
#' So a WARNING here - many analyses require date-times for individual animals to have unique DateTimes.
#' So they should be removed again should further analyses be needed, e.g. KDEs, HMMs etc. Further functions. They can also
#' be dropped again here through the argument \code{keep_extra_row}.
#'
#' A note of caution. This function has wider appeal e.g. in wanting to label
#' a set of fixes in time as a "trip" or whatever really using the "Flag" method. But
#' the function here is essentially looking for what the user calls the "colony".
#' So labelled results may be different to what you expect if colony based for example on
#' a sampling rate threshold. In which case \code{gap_section} is probably what you want. An
#' example of this is if you JUST have trips and no colony info in a file, and you want to segreate trips
#' based on gaps between them being more than your tag sampling rate. It's a simple enough data.table task anyway!
#'
#' A separate function \code{make_shape} can be used to replicate the circle or rectangle
#' as a separate shape for further use, for example in plotting with \code{plot_leaflet} using
#' the \emph{shapes} argument.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param method A character string, one of \code{rect}, \code{circ}, \code{shape} or \code{flag}.
#' Each method represents an approach for the area chosen from which to determine trips from a central place.
#' With the exception of the \code{flag} method thus being (1) a rectangle or square, (2)
#' a circle (note not an ellipse), (3) a polygon of any  shape the user wishes (e.g. based on habitat or geographic boundaries)
#' and (4) a specific column in the data, with the name of that column given by \code{Flag = variable} - see below.
#' Methods rely on the argument \strong{lls} - see below - to define geometry boundaries, midpoints, or
#' a polygon of the shape itself. For the method \code{shape},
#' it is highly recommended that the shape be a single polygon - the \code{define_trips} function
#' has not been tested on multiple polygons and could cause complexities and confusions. Should
#' multiple areas be of interest, then specific separate definition of those areas with separate shapes
#' is recommended or using methods such as R package \code{recurse}.
#' The \code{flag} method can also be used in conjunction with the other three methods to take a combination of
#' trips identified by the shapes selected with the further \code{flag} information - i.e.
#' points identified as falling inside the rectangle, circle or shape, and/or also matching
#' the separate \code{Flag} column in the dataset. The user can also choose from two
#' \code{Flag_options} - see below - to determine how the shapes and flags are combined.
#' @param lls A vector used for determining the boundaries or inputs for the shape of the central place.
#' For the \code{rect} method, lls \strong{must} be a vector of length 4, representing the
#' \code{c(minlong, maxlat, maxlong, minlat)} of the bounding box to be used; for the \code{circ}
#' method, \code{lls} must be a vector of length 2 representing \code{longitude,latitude} of
#' the central point, and must be accompanied by a value for the \code{radius} for which
#' a buffer is applied around the central point - see below. For method \code{shape}, an object of
#' class \code{sf} is required, restricted here for the simple feature \code{POLGON} geometry. \code{lls} is not required for the \code{flag} method when used on its own.
#' Coordinates for lls under \code{rect} and \code{circ} are expected as geographic WGS84. The method \code{shape} supplied
#' will be projected to the given CRS provided through \code{p4s}.
#' @param radius The radius around the central point for the \code{circ} method - see above, specified
#' as a numeric value in metres, defaulting to 1000 m if no value is provided.
#' @param by Additional by variables to group the data by for assessing trips, e.g. if you bootstrap the data
#' from sub-sampling perhaps, then you may want by = "boot"; if the \code{birdyear} argument
#' is TRUE, the actual grouping would then be: c(TagID, year, boot).
#' @param birdyear whether to assess animals as bird-years i.e. if a large gap in monitoring between seasons, or just as birds (defaults to TRUE for birdyears).
#' @param GIS Logical defaulting to TRUE if the first part of the GIS processing within the function is to be carried out.
#' @param definetrips Logical defaulting to TRUE if the second part of the defining of trips is to be carried out.
#' @param use_current_by An option (logical default FALSE) to use the existing tidy by variable structure in the data
#' but including TagID as the highest level grouping if not included. Could lead to unexpected results so
#' check this carefully. Default is to use by variable grouping, set to NULL with birdyear TRUE to giving TagID, year by default.
#' @param Flag Named column in the dataset, indicating whether an animal is deemed to be in a
#' colony, or central place starting point. This column is used in the \code{flag} method
#' and in conjunction with the other three methods for a combination approach. The variable \code{Flag} for the
#' \code{flag} method must be a binary column of 1 or 0 for locations assigned as inside or outside
#' the colony, for instance by other methods such as visual re-sighting surveys, cameras or telemetry base station automatic detection, etc.
#' @param keep_extra_row Logical defaults to TRUE for keeping the extra row for trip statistics,
#' if a bird comes back to the colony and leaves again before registering a second fix there.
#' @param new_after_gap Logical. Whether to start a new trip after a \emph{"gap"} in the
#' date-time telemetry record; gaps are initially determined through the function \code{\link{clean_GPS}}
#' and can be set there to any length defined by the user. Should new gaps be decided upon after
#' \code{clean_GPS} then function \code{\link{gap_section}} can be used to re-define gapsections.
#' The issue here is related to potentially \strong{incomplete} trips when the tag may stop recording
#' for some reason, e.g. low battery, but where an animal can still be away
#' on a trip after the gap (i.e. did it come back and leave again?). This argument is used to
#' tell the function if a new trip should be started after the gap (the default = TRUE), or
#' if we want to include the time away from the colony after the gap as the same trip - i.e. should
#' gaps be known and potentially small enough to allow that choice - it is up to the user.
#' @param Flag_options One of two options - \code{maximal} or \code{minimal} - can be selected in this argument to be used alongside
#' methods \code{rect}, \code{circ} or \code{shape} methods (see above); (1) for the
#' \code{maximal} option, points that are outside the shape but "flagged" by the
#' additional \code{Flag} column as at the central place, are thus taken as at the central place,
#' and points that are inside the shape are always taken as colony, even if the \code{Flag}
#' column suggest they are not; (2) for the \code{minimal} option, only cases where both
#' the \code{Flag} and the overlap with the shape via \code{rect}, \code{circ} or \code{shape} methods
#' match, are taken as being inside the colony - hence a point can be within the shape but
#' not flagged and so becomes marked as NOT at the central place even if inside the shape,
#' or outside the shape, if "flagged" as at the central place will still be marked as on a trip.
#' @param trip_lab_noend There may be instances where you want to label incomplete trips not returning to the
#' central place at the end of the dataset as a trip. Set this parameter to TRUE will do that, default FALSE.
#' @param trip_lab_nostart There may be instances where you want to label incomplete trips at the start returning
#' to the central at the start of the dataset as a trip. Set this parameter to TRUE will do that, default FALSE. Note,
#' however this will classify for example all non-breeding data as a trip if the dataset if monitoring is throughout the year.
#' @param trip_methods Either \code{"new"} or \code{"old"} character string, defaults to \code{"new"}, giving available a
#' deprecated older method doing essentially the same process as the new option with less efficient code.
#' @param tripcon_sten logical defaulting to TRUE for whether the data should be assigned "trips" for the
#' very start and end of the data - situations can arise where you can read in data before an animal
#' is at a central place, which without setting this to TRUE, would then code "trips" for those periods
#' before it arrived to the central place and after it left; that may be needed if you are SURE the
#' data are for the breeding season, but then the trips would be incomplete anyway. If used properly, setting this is TRUE
#' also allows the define_trips function to essentially tell you when the first real time the bird was at the
#' central place by the start time of the first trip and end time of the last.
#' @param plot logical, defaults to FALSE, but if TRUE provides a plot of the shape used and the
#' points inside or outside the shape for which trips are based (useful for comparing different
#' trip classification systems).
#' @param p4s A numeric or \code{sf} \code{CRS} argument, here defaulting to epsg:3035 (NW Europe). This is used in projecting
#' the supplied \code{WGS84} \code{lls} argument for code{rect} and \code{circ} methods, and for projecting the \code{sf}
#' object supplied for the \code{shepe} method.
#' @param verbose logical (default TRUE) for whether detailed messaging is required.
#'
#' @return The function returns a tibble format with an additional \code{Track} class
#' with with additional columns of: \emph{Col}, a binary 1 or 0 for whether the positional fix
#' was identified within the chosen central place area or outside a trip; and \emph{tripNo}, a column of numeric
#' integer values delineating sequentially ordered trips for each individual TagID based on the options chosen.
#' A value of zero is returned for animals inside the defined central place area.
#'
#' @seealso [MoveRakeR::clean_GPS], [MoveRakeR::trip_stats]
#'
#' @examples
#'
#' # -------------------------------------------------------------------------- #
#' # Make sure columns of bird id, date-time, longitude and latitude are called:
#' # TagID, DateTime, longitude, and latitude, respectively and then use
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' ## Defining trips based on rectangle around Walney LBBG colony, using a new projection,
#' ## Lamert Aziimuth Equal Area centred on the colony
#' ColLon = -3.185
#' ColLat = 54.045
#'
#' # sp to sf transition: Although not receommended to use +proj notation - is there another way of setting specific coordinates in a Lambert EA? Not sure
#' p4 <- sf::st_crs((paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep="")))
#'
#' # or can e.g. specify an epsg number or any sf crs for that matter
#' # p4 <- 3035
#'
#' #' # ----------------------------- #
#' # Set up a four-length vector lat-long bound for a rectangle of colony
#' #' # ----------------------------- #
#' LLS <- c(-3.2, 54.0553,-3.1689, 54.0437)
#'
#' # simple definition of trips using a basic rectangle around the site
#' data_rect <- define_trips(data = indata, method="rect", lls = LLS, p4s = p4)
#'
#' # ----------------------------- #
#' # simple circle of 1000 m radius
#' # ----------------------------- #
#' LLS <- c(-3.2, 54.0553)
#' data_circ <- define_trips(data = indata, method="circ", lls = LLS, p4s = p4)
#'
#' # check in local shiny app:
#' plot_leaflet_trips(data_circ)
#'
#' # ----------------------------- #
#' ### Loading in a kml file for a shape
#' # ----------------------------- #
#' path_dir = "DIR"
#' path_nm <- "XXX.kml"
#'
#' shp_in <- sf::st_read(paste0(path_dir, path_nm))
#' kml = as.data.frame(sf::st_coordinates(shp_in))
#'
#' # make sf POLYGON, ideally should be dealt with in the function but like this for now
#' poly <- sfheaders::sf_polygon(obj = kml, x = "X", y = "Y")
#' poly = sf::st_set_crs(poly, 4326)
#'
#' # then defining trips by this shape
#' data_shape <- define_trips(data = data, method = "shape", plot=TRUE, lls = poly, p4s = 27700)
#'
#' ###### testing the two halves of define_trips()
#'
#' data_1st <- data %>% define_trips(GIS = TRUE, definetrips = FALSE,
#'                                   method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4) # add trips
#'
#' # note we have no boot variable, so a warning will be returned
#' data_2nd <- data_1st %>% define_trips(by = "boot", GIS = FALSE, definetrips = TRUE,
#'                                       birdyear = TRUE,
#'                                        method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4) # add trips
#'
#' @references
#' Bracis, C., Bildstein, K.L. & Mueller, T. (2018) recurse Revisitation analysis uncovers
#' spatio‐temporal patterns in animal movement data.\emph{Ecography}, \strong{41}, 1801-1811.
#'
#' Langley, L.P., Lang, S.D.J., Ozsanlav-Harris, L., Trevail, A.M. (2024) ExMove: An open-source toolkit for
#' processing and exploring animal-tracking data in R. Journal of Animal Ecology, 93(7), 784-795.
#'
#' Picardi, S., Smith, B.J., Boone, M.E., Frederick, P.C., Cecere, J.G.,
#' Rubolini, D., Serra, L., Pirrello, S., Borkhataria, R.R. & Basille, M. (2020)
#' Analysis of movement recursions to detect reproductive events and estimate their fate
#' in central place foragers Movement Ecology \url{https://doi.org/10.1101/562025}.
#' Sumner, M.D. (2011). The Tag Location Problem. Ph.D. thesis, University of
#' Tasmania. \url{https://eprints.utas.edu.au/12273/3/sumner.pdf}.
#'
#' Sumner, M.D., Wotherspoon, S.J. & Hindell, M.A. (2009). Bayesian estimation of animal
#' movement from archival and satellite tags. \emph{PLoS ONE}, \strong{4}(10).
#' \url{http://dx.plos.org/10.1371/journal.pone.0007324}.
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import lubridate
#' @import sf
#' @export
define_trips <- function(data, use_current_by = FALSE, by = NULL, birdyear = TRUE, definetrips = TRUE,
                         GIS = TRUE, method="rect", lls = NULL, radius = 1000, Flag = NULL,
                         keep_extra_row = TRUE, new_after_gap = TRUE, Flag_options = "Maximal",
                         trip_lab_noend = FALSE,  trip_lab_nostart = FALSE,
                         tripcon_sten = TRUE, p4s = 3035, verbose = TRUE){


  if(!definetrips & !GIS){
    stop("definetrips = FALSE and GIS = FALSE< Nothing for function to do")
  }


  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  # get attributes from previous function runs
  attr_list <- get_attributes(data)
  # --------------------------------------- #

  if(verbose){message("Object of ", length(unique(data$TagID)), " animals, ", nrow(data), " rows of data")}

  # ----------------------------------------------------------------- #
  # part 1 GIS-ing
  # ----------------------------------------------------------------- #

  if(GIS){
    ############### GIS PROCESSING ###############

    if(verbose){message("GIS processing using method = ", method)}

    ## sf preparation
    b_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
    b_sf <- sf::st_transform(b_sf, p4s)

    if(method == "rect"){
      if(is.null(lls) | is(lls, "SpatialPolygons") | length(lls) != 4) stop("wrong length supplied for lat long object")

      # make a cartesian polygon of the rectangle
      ylim = c(lls[4],lls[2])
      xlim = c(lls[1],lls[3])
      sfc = sf::st_sfc(sf::st_point(c(xlim[1],ylim[1])),sf::st_point(c(xlim[1],ylim[2])), sf::st_point(c(xlim[2],ylim[2])), sf::st_point(c(xlim[2],ylim[1])))
      sfc %>% sf::st_bbox() %>%  sf::st_as_sfc() %>% sf::st_as_sf() %>% {. ->> bbox_shape}
      bbox_shape <- sf::st_set_crs(bbox_shape, "epsg:4326")
      bbox_shape <- sf::st_transform(bbox_shape, crs = p4s)

      # overlap of box with the sf GPS data
      bbox_shape$overlap = 1
      bird_join <- sf::st_join(b_sf, bbox_shape, join = sf::st_covered_by) #geometric predicate 'within', as Sam did in QGIS, but there are some border issues with geometies in my version so st_covered_by works more consistently
      bird_join$overlap <- ifelse(is.na(bird_join$overlap),0,1)
      data$Col <- bird_join$overlap # put back in the original dataset

      shape_used <- bbox_shape
    }

    data_test = data[data$TagID %in% unique(data$TagID)[1],]

    if(method == "circ"){

      if(is(lls, "SpatialPolygons")){
        # convert to sf
        lls <- sf::st_as_sf(lls)
      }
      if(inherits(lls, "sf")){
        if(sf::st_geometry_type(lls) == "POLYGON"){
          lls <- sf::st_transform(lls, p4s)
        }

      } else{

        if(is.null(lls) | length(lls) > 2) stop("wrong length supplied for lat long object")

      }

      if(is.null(radius)) stop("no circle radius supplied")

      ## new sf approach:
      bb <- data.frame(x=lls[1],y=lls[2]) # note these HAVE to be WGS 84
      bb_sf <- sf::st_as_sf(bb, coords = c("x", "y"), crs = 4326)
      bb_sf <- sf::st_transform(bb_sf, p4s)
      cc_sf <- sf::st_buffer(bb_sf, radius) # make buffer


      # overlap again used, similar to rect
      cc_sf$overlap = 1
      bird_join_circ <- sf::st_join(b_sf, cc_sf, join = sf::st_covered_by) #geometric predicate 'within', as Sam did in QGIS, but there are some border issues with geometies in my version so st_covered_by works more consistently
      bird_join_circ$overlap <- ifelse(is.na(bird_join_circ$overlap),0,1)
      data$Col <- bird_join_circ$overlap # put back in the original dataset

      shape_used <- cc_sf

    }




    if(method == "shape"){

      ## new sf version will need the shape to be ... a POLYGON shape
      ## allow conversion from sp::SpatialPolygons
      if(is(lls, "SpatialPolygons")){
        lls = st_as_sf(lls)
      }

      if(!inherits(lls,"sf")){
        stop("Needs an sf POLYGON object (older sp methods are now not used")
      }

      # otherwise stop the function if a POLYGON class is not found either from above conversion or direct supply by user
      if(!sf::st_geometry_type(lls) == "POLYGON"){
        stop("Needs an POLYGON object (e.g. check you haven't provided a multi-part polygon")
      }

      # transform
      lls <- sf::st_transform(lls, p4s)

      # using the same overlap proceedures....
      lls$overlap = 1
      bird_join_shape <- sf::st_join(b_sf, lls, join = sf::st_covered_by) #geometric predicate 'within', as Sam did in QGIS, but there are some border issues with geometies in my version so st_covered_by works more consistently
      bird_join_shape$overlap <- ifelse(is.na(bird_join_shape$overlap),0,1)
      data$Col <- bird_join_shape$overlap # put back in the original dataset

      shape_used <- lls

    }

    if(method != "flag"){

      # Prioritise flag outside the shape, otherwise shape (done for terns)
      # if flag == 1 and shape == 0, take 1, i.e. prioritise flag outside shape, else inside colony shape, take the shape
      #   covers in the false: if flag == 0 and shape == 1, then shape == 1,0
      #   and also           : if flag == 1 and shape == 1, then shape == 1,0
      #
      # Only retain a 1 for "Col" if BOTH flag AND col are 1
      # if flag == 1 and shape = 1, then 1, else 0
      #
      # Other options of prioritising a shape fully or a flag fully are covered outside this

      if(!is.null(Flag)){
        warning("also using Flag column for additional colony refinement")

        shcol = data$Col # original from just the shape

        if(Flag_options == "maximal"){ # prioritise flag outside shape, but in shape, prioritise shape
          data$Col <- ifelse(data[,which(names(data) == Flag)] == 1 & data$Col == 0,1,data$Col)

        }
        if(Flag_options == "minimal"){ # only mark as Col where both Flag and Col agree
          data$Col <- ifelse(data[,which(names(data) == Flag)] == 1 & data$Col == 1,1,0)

        }
      }

    } else if(method == "flag"){
      if(is.null(Flag)){stop("needs a column of [1,0] called 'flag'")}
      if(length(which(names(data) == Flag)) == 0){stop("No column called ", Flag, " found in data")}

      data$Col =  data[,which(names(data) == Flag)]

      shape_used <- NULL

    }

    data_dp_fin <- tibble(data) %>% group_by(TagID)  # in case finishing here and not doing second half

  }

  # ----------------------------------------------------------------- #
  # part 2 Assigning trips
  # ----------------------------------------------------------------- #

  #### on rare occassions we may only want to have carried out the GIS-ing first
  #### e.g. if bootstrapping the data for sub-sampling then the fixes are what they are and we
  #### can then carry out the second have trip assignment separately

  if(definetrips){

    data_dp <- tibble(data)

    ########## MAIN BIRD TRIP PROCESSING ##########
    if(verbose){message("Defining trips")}

    if(!exists("Col", data)){
      stop("Column Col doesn't exist - did you run define_trips with GIS = TRUE?")
    }

    # Beh, trip or colony
    data$Beh <- ifelse(data$Col != 1,"Trip","Colony")

    if(!exists("gap", where = data)){
      warning("No gapsections defined, likely clean_GPS() was not used: assuming NO GAPS in the telemetry datetime record")
      data$gap <- 0
      data$gapsec <- 1

    }

    #######################################
    ####### working here with dplyr
    data_dp <- tibble(data)
    #data_dp$TagID0 <- data_dp$TagID


    # ---------------------------------------- #
    ##### assess what by variable groupings exists in the data already
    if(use_current_by){

      # testing capture of by variables...
      #data$year <- lubridate::year(data$DateTime)
      #data_dp <- data %>% group_by(TagID, year)
      #data_dp <- data_dp %>% group_by(TagID)
      #data_dp <- data_dp %>% ungroup()

      grps <- data_dp %>% summarise(data = list(cur_group()), .groups = "keep") %>% names()
      grps <- grps[grps != "data"]

      # then any by variable grouping in the data will be fed through IN PLACE OF
      # ANYTHING THE USER SPECIFIES. This functionality is because if we start merging by variables
      # then that could lead to odd combinations of levels as hierarchical in nature!

      if(length(grps) == 0){
        # suggest therefore going to a default TagID grouping? Over-ridden if birdyear = TRUE
        by = NULL
      } else{
        by = grps
      }

      # so if by is set as NULL by user but 'use_current_by' is TRUE and data already have grouping
      # those will be used. But this is potentially DANGEROUS and the code here is supposed to be bird by bird
      # so suggest that this is also checked in that instance as a failsafe
      # Just wary of giving the user 'free-rein' over the function! Make TagID the highest level grouping so at least the function will work as intended

      if(!is.null(by) & !"TagID" %in% by){
        by <- c("TagID", by)
      }
      by <- unique(by)

    } else{

      ### set up by variable grouping by user specification
      # if year not found, make it, as it is easy enough if user forgets
      if("year" %in% by & !exists("year", data_dp)){
        data_dp$year <- lubridate::year(data_dp$DateTime)
      }
      # do we want TagID and year directly specified? Default yes as is safer
      if(birdyear){
        # then we also want year as well as TagID, aside from any OTHER by variables
        # group the data using TagID and whatever by variables supplied
        by <- c("year", by)
        data_dp$year <- lubridate::year(data_dp$DateTime)
      }
      by <- unique(by) # just in case this ends up with multiple named by variables

      # check if by variables are actually present!
      #by <- c("year", "foo", "bar")
      if(!is.null(by)){
        #by <- "huh"
        if(any(!by %in% names(data_dp))){

          badvars <- by[!by %in% names(data_dp)]
          if(verbose){
            warning("Variables: ", paste(paste0("'",badvars,"'"), collapse = ", ") ," were not found in data, so will be removed from tbl 'by' grouping!")
          }
          by <- by[by %in% names(data_dp)]
          if(length(by) == 0){
            by <- NULL
          }
        }
      }
    }

    if(verbose){
      if(!is.null(by)){
        if(verbose){message("Grouping data by: TagID, ", by)}
      } else{
        if(verbose){message("Grouping data by: TagID")} # concern here obviously with commas in the messaging :)
      }
    }

    # Carry out initial grouping
    data_dp <- data_dp %>% group_by(TagID, !!!syms(by)) # separating out this line for readability

    # ---------------------------------------- #
    #data_dp_here <- data_dp


    ########## gap labelling - do we want new trips after a gap?
    data_dp <- data_dp %>%
      mutate(gap = case_when(row_number() == n() ~ 0, TRUE ~ gap)) %>%
      filter_at(vars(gap), all_vars(!is.na(.)))

    if(new_after_gap){
      data_dp <- data_dp %>% mutate(Beh = if_else(gap == 1, 'Colony', Beh))
    }

    ########## trips that come-and-go - not a good process as adds in extra information but removing locational information and any other variables are NAd
    st <- data_dp %>%
      group_by(TagID, !!!syms(by)) %>%
      filter(lag(Beh) == "Trip" & lead(Beh) == "Trip" & Beh == "Colony")

    # bit of base code: NA out everything except DateTime, TagID, gapsec, gap, Col and Beh
    non_NA <- c("DateTime", "TagID", "TagID0","gapsec", "gap", "Col", "Beh", "longitude", "latitude", by)
    st_na <- data.frame(apply(st, MARGIN = c(1,2), function(x){x <- NA}))
    st_na[,which(names(st_na) %in% non_NA )] <- st[,which(names(st) %in% non_NA )]
    st <- tibble(st_na)

    # STORE THESE ROWS AS AN EXTRA SLOT (but note they need to be rbinded here to work out trip number they belong to)
    # label as extra_row lookup
    data_dp$extra_row <- 0
    st$extra_row <- 1

    if(nrow(st) > 0){
      data_dp <- rbind(data_dp, st) %>% arrange(TagID, !!!syms(by), DateTime)
    } else{
      data_dp <- data_dp %>% arrange(TagID, !!!syms(by), DateTime)
    }
    #data_dp_extra <- st

    ######## Drop the labelling of trips at the very start and end if the bird is already ON a trip or leaves and data  at end are also on a trip
    if(tripcon_sten){

      # better way is looking up in position
      # find out if the position where first 1 is detected is the same as the start of the dataset
      data_dp = data_dp %>%
        mutate(first_col_row = case_when(Col == 1 ~ cur_group_rows() )) %>% ### <-----???
        mutate(max_col_row = case_when(Col == 1 ~ cur_group_rows() )) %>% ### <-----???
        ungroup() %>% # needed because first_col_row is NOT WITHIN TagID and below rn IS
        mutate(rn = row_number()) %>%
        group_by(TagID, !!!syms(by)) %>%

        # RELABEL fixes as at colony to avoid inclusion in trip definitions
        #filter(rn >= min(first_col_row, na.rm=TRUE))
        mutate(Beh = ifelse(rn < min(first_col_row, na.rm=TRUE), "Colony", Beh),
               Beh = ifelse(rn > max(max_col_row, na.rm=TRUE), "Colony", Beh))

    }

    ### identify trips
    data_dp <- data_dp %>%
      group_by(TagID, !!!syms(by)) %>%
      mutate(lag = if_else(Beh == lag(Beh), 1, 0),
             lead = if_else(Beh == lead(Beh), 1, 0),
             stend = if_else(Beh == "Colony" & lead == 0, "start", if_else(Beh == "Colony" & lag == 0, "end", NA),NA),
             Beh = if_else(Beh == "Trip" | !is.na(stend), "Trip", "Colony")) # relabels Beh so including start and end inside the colony (central place)

    # Trips only, we  want to keep colony locations in the output
    data_dp_trip <- data_dp  %>%
      filter(Beh == "Trip") %>%
      mutate(pnum = case_when(stend == "start" ~ cur_group_rows())) %>%
      tidyr::fill(pnum) %>% # fills NAs in between (down default) non NA entries: https://tidyr.tidyverse.org/reference/fill.html
      mutate(tripNo = consecutive_id(pnum)) %>% # neat trick using consecutive_ids: https://dplyr.tidyverse.org/reference/consecutive_id.html
      dplyr::select(-c(lag, lead, stend, pnum, first_col_row, max_col_row, rn, Beh))

    # include colony locations
    data_dp_col <- data_dp %>% filter(Beh == "Colony") %>% mutate(tripNo = 0) %>%
      dplyr::select(-c(lag, lead, stend, first_col_row, max_col_row, rn, Beh))

    data_dp_fin = bind_rows(data_dp_trip, data_dp_col) %>% arrange(TagID, DateTime) %>%
      mutate(rn = row_number(),
             tripNo = if_else( rn == 1 & is.na(tripNo), lead(tripNo), tripNo),
             tripNo = if_else( rn == max(rn) & is.na(tripNo), lag(tripNo), tripNo)
             )



    # Specific cases - where we have a trip that is never ending after leaving the colony
    # this is currently missed at this point in the routine; similarly for the start, if a bird
    # is coming back from migration, before the colony, then label that too, and bump tripNos up 1
    # but then that codes all migration BEFORE returning to the colony as a trip!
    # so perhaps this is an option not set as default - e.g. if you know the data ends at a cetain point and
    # you want to label the last movements of a bird leeaving the colony as a 'trip'
    # then at least this option is present.

    if(trip_lab_noend){
      ### adjust the trip numbering at the end of the animal / by variable
      data_dp_fin = data_dp_fin %>%
        mutate(rn = row_number(),
               trip_check = case_when(tripNo == max(tripNo, na.rm=TRUE) ~ rn),
               trip_check = if_else(is.na(trip_check), 0,trip_check),
               tripNo = if_else( rn > max(trip_check) & Col == 0 | rn > max(trip_check) & lead(Col) == 0, max(tripNo,na.rm=TRUE)+1, tripNo ),
               tripNo = if_else( rn == 1 & is.na(tripNo), lead(tripNo), tripNo),
               tripNo = if_else( rn == max(rn) & is.na(tripNo), lag(tripNo), tripNo)
        )  %>%
        dplyr::select(-c(rn,trip_check))
    }

    if(trip_lab_nostart){
      ###### adjust start numbering
      data_dp_fin = data_dp_fin %>%
        mutate(rn = row_number(),
               trip_check = case_when(tripNo == 1 ~ rn),
               tripNo = if_else( rn < min(trip_check,na.rm=TRUE) & Col == 0 | rn < min(trip_check,na.rm=TRUE) & lag(Col) == 0, -99, tripNo ),
               tripNo = if_else( rn == 1 & is.na(tripNo), lead(tripNo), tripNo),
               tripNo = if_else( rn == max(rn) & is.na(tripNo), lag(tripNo), tripNo),
               check = ifelse(any(tripNo == -99),1,0),
               tripNo = ifelse(check == 1, ifelse(tripNo > 0 & tripNo != -99, tripNo + 1, tripNo), tripNo),
               tripNo = ifelse(check == 1, ifelse(tripNo == -99, 1, tripNo), tripNo)
        ) %>%
        dplyr::select(-c(check,rn,trip_check))
    }
    #tail(data.frame(data_dp_fin), 20)

    # remove the extra row stuff
    extra_rows = data_dp_fin[data_dp_fin$extra_row == 1,] # for storing as attribute

    if(!keep_extra_row){ # unsure about this if needed
      data_dp_fin <- data_dp_fin[!duplicated(data_dp_fin$DateTime),]
    }

    #data_dp_fin$TagID <- data_dp_fin$TagID0
    data_dp_fin <- data_dp_fin %>% group_by(TagID, !!!syms(by))

    data_dp_fin <- data_dp_fin[data_dp_fin$extra_row == 0,] # output always retains the original dataset even if come-and-go trips are found

  }

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data_dp_fin <- give_attributes(data_dp_fin, attr_list)

  # --------------------------------------------------------- #
  # new attributes on current function run
  if(is.null(attr(data_dp_fin, "define_trips") )){
    attr(data_dp_fin, "define_trips") <- "define_trips"
  }

  # sub_attributes for arguments, depending on the definetrips and GIS arguments
  attr(attr(data_dp_fin, "define_trips"), "GIS") <- GIS
  attr(attr(data_dp_fin, "define_trips"), "definetrips") <- definetrips

  if(definetrips){
    attr(attr(data_dp_fin, "define_trips"), "extra_rows") <- extra_rows # extra rows that may be needed for trips where start and ends are the same per trip at the cplace
    attr(attr(data_dp_fin, "define_trips"), "keep_extra_row") <- keep_extra_row
    attr(attr(data_dp_fin, "define_trips"), "by") <- by
    attr(attr(data_dp_fin, "define_trips"), "birdyear") <- birdyear
    attr(attr(data_dp_fin, "define_trips"), "new_after_gap") <- new_after_gap
    attr(attr(data_dp_fin, "define_trips"), "tripcon_sten") <- tripcon_sten
    attr(attr(data_dp_fin, "define_trips"), "trip_lab_noend") <- trip_lab_noend
    attr(attr(data_dp_fin, "define_trips"), "trip_lab_nostart") <- trip_lab_nostart
  }
  if(GIS){
    attr(attr(data_dp_fin, "define_trips"), "shape_used") <- shape_used # the shape for further use in trip_stats to get centroid for CPF measurement
    attr(attr(data_dp_fin, "define_trips"), "method") <- get("method")
    attr(attr(data_dp_fin, "define_trips"), "Flag") <- Flag
    attr(attr(data_dp_fin, "define_trips"), "Flag_options") <- Flag_options
    attr(attr(data_dp_fin, "define_trips"), "radius") <- radius
    attr(attr(data_dp_fin, "define_trips"), "radius") <- lls
  }
  attr(attr(data_dp_fin, "define_trips"), "p4s") <- p4s

  ### return
  data_dp_fin <- structure(.Data = tibble(data_dp_fin), class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))

  return(data_dp_fin)

}


#' @rdname define_trips
#' @export
make_shape <- function(method = "rect", radius = 1000, lls = NULL, p4s = 3035){

  # returns a simple circle or rectangle for plotting in leaflet_maps to assess trip definition

  if(method == "rect"){
    if(is.null(lls) | length(lls) != 4) stop("wrong length supplied for lat long object")

    ylim = c(lls[4],lls[2])
    xlim = c(lls[1],lls[3])
    sfc = sf::st_sfc(sf::st_point(c(xlim[1],ylim[1])),sf::st_point(c(xlim[1],ylim[2])), sf::st_point(c(xlim[2],ylim[2])), sf::st_point(c(xlim[2],ylim[1])))
    sfc %>% sf::st_bbox() %>%  sf::st_as_sfc() %>% sf::st_as_sf() %>% {. ->> rr}
    rr <- sf::st_set_crs(rr, "epsg:4326")
    rr <- sf::st_transform(rr, crs = p4s)

  }

  if(method == "circ"){

    if(is.null(lls) | length(lls) > 2) stop("wrong length supplied for lat long object")
    if(is.null(radius)) stop("no circle radius supplied")

    bb <- data.frame(x=lls[1],y=lls[2]) # note these HAVE to be WGS 84
    bb <- sf::st_as_sf(bb, coords = c("x", "y"), crs = 4326)
    bb <- sf::st_transform(bb, p4s)
    rr <- sf::st_buffer(bb, radius)

  }
  return(rr)
}



