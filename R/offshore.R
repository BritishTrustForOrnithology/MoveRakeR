#' Add "offshore" marine distinction for GPS telemetry fixes
#'
#' Function \code{offshore} adds a label for each telemetry positional fix denoting whether the point
#' is deemed to be "offshore" or "inland" as a simple binary variable, with the distinction between
#' environments determined by shapefiles provided by the user.
#'
#' @details
#' Traditionally this process has been borne out of gull tracking work at BTO and the need to know
#' how far gull species such as Lesser Black-backed Gulls travel offshore. This is important for
#' species that span both terrestrial and offshore realms.
#'
#' On the face of it, this is a simple GIS task. This function is provided purely for repeatability of process
#' and is concerned primarily with getting the initial shapefile correct with what we think 'marine'
#' represents, i.e. excluding terrestrial but also estuarine and river locations. However, it is up to the user to
#' determine an appropriate shapefile.
#'
#' Offshore distinctions from \code{offshore} function can be used further in trip statistics, e.g.
#' how far a bird has travelled offshore per trip - see \code{trip_stats}, and also can be used together
#' with the \code{dist2coast} function, that as with offshore, adds an additional column for distance points
#' are to the UK/Ireland coastline, again as determined through specific shapes fed into that function.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param plot logical defaults to TRUE as to whether a plot should be returned for the method chosen.
#' @param crop logical defaults to TRUE for whether the maps used should be cropped first prior to overlaps
#' with offshore being assessed, speeding up processing.
#' @param buffer_extent A value to further add around the bird data to circumvent potential issues
#' of edges of the cropped map missing potential true land area; a default value is specified as 10000
#' but this will depend on map units and it is suggested this is checked before hand if using this option.
#' @param use_crs_shape logical defaulting to TRUE whether to use the projection of the shape provided for bird data, otherwise
#' the function will project both bird data and the shape using the p4s argument.
#' @param shape the function expects a single sf shape (sf MULTIPOLYGON or POLGON) representing the coastline.
#' @param p4s The coordinate reference system projection (numeric) required for overlap processing, defaulting to BNG \code{epsg:3035}.
#' @param verbose logical argument as to whether detailed information processing of the function
#' should be displayed, defaults to TRUE.
#'
#' @seealso [MoveRakeR::dist2coast], [MoveRakeR::trip_stats]
#'
#' @return The same dataset object as input with additional column
#' of integer 1 or 0 values labelling telemetry fixes as offshore or onshore.
#'
#' @examples
#'
#' ############### MAP PREPARATION ###############
#' # R packages not bundled with MoveRakeR
#' #remotes::install_github("ropensci/rnaturalearthhires")
#' #install.packages('rnaturalearth')
#' library(rnaturalearthhires)
#' library(rnaturalearth)
#'
#' # https://digimap.edina.ac.uk/marine also useful
#'
#' spdf_world <- ne_download(scale = 10, type = "countries")
#' Europe <- spdf_world[spdf_world$SUBREGION %in% c("Western Europe","Eastern Europe","Northern Europe","Southern Europe"),]
#' Europe_sf = st_crop(Europe, xmin = -25, ymin=32, xmax=35, ymax=70)
#' Europe_sf <- sf::st_transform(Europe_sf, 3035)
#' plot(Europe_sf$geometry)
#'
#' # Note example with seabird colonies in mind, however, the resolution of ne maps does not extend small enough.
#' # This is just an example using open data
#' # It may be more sensible for example to use a bespoke shape and an isopleth e.g. 2 m to more
#' # accurately label non-estuarine / riverine locations as terrestrial onshore
#'
#' # bird data, e.g.
#' data = data %>% clean_GPS(data) %>% offshore(use_crs_shape = TRUE, shape = Europe_sf, crop = TRUE, buffer_extent = 10000)
#'
#' plot_leaflet(data)
#'
#' @export
offshore <- function(data, use_crs_shape = TRUE, plot = FALSE, crop = TRUE, buffer_extent = 10000,
                     shape = NULL, verbose = TRUE, p4s = 3035){


  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  if(is.null(shape)){stop("shape not supplied")}

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  # -------------------- #
  # check for extra rows for trips that may share a start and end - these will need labelling as well
  extra_rows <- NULL
  extra_rows <- attr(attr(data, "define_trips"), "extra_rows")

  # these will need binding for the trip duration and total distance, but note that should not be retained as it distorts the original data
  if(!is.null(extra_rows)){
    if(nrow(extra_rows) > 0){
      message("Extra rows found from define_trips() function run")
      extra_rows$extra_row = 1
      data = data %>% mutate(extra_row = 0) %>% bind_rows(.,extra_rows) %>% arrange(TagID,DateTime)
    }
  }
  # -------------------- #


  #############
  # get projected points for overlap

  b_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

  # project first to whatever the CRS is of the shapefile supplied

  if(verbose){message("Projecting shapes using p4s argument")}
  if(use_crs_shape){
    b_sf <- sf::st_transform(b_sf, sf::st_crs(shape))
  } else{

    b_sf <- sf::st_transform(b_sf, p4s)

    # only then reproject the shape as could slow down run time
    if(!identical(sf::st_crs(shape),sf::st_crs(b_sf))){

      shape <- sf::st_transform(shape, p4s)

    }
  }

  # to make the calculations even quicker....but the user needs to know what they are doing with shapes!
  if(crop){

    if(verbose){message("Using a further crop of the shape based on bird extent:
       --> CAUTION: make sure this doesn't overly crop and classify land as sea! <--")}

    BBox <- sf::st_bbox(b_sf) # box around the GPS point extent

    # could just use a flat extent and st_crop but wanting to dovetail with a buffer approach...
    # i.e. to buffer the extent, we need to make a shape as you can't apply st_buffer to st_bbox
    ylim = c(BBox[2],BBox[4])
    xlim = c(BBox[1],BBox[3])
    sfc = sf::st_sfc(sf::st_point(c(xlim[1],ylim[1])),sf::st_point(c(xlim[1],ylim[2])), sf::st_point(c(xlim[2],ylim[2])), sf::st_point(c(xlim[2],ylim[1])))
    sfc %>% sf::st_bbox() %>%  sf::st_as_sfc() %>% sf::st_as_sf() %>% {. ->> bbox_shape}
    bbox_shape <- sf::st_set_crs(bbox_shape, sf::st_crs(BBox))

    if(!is.null(buffer_extent)){
      if(verbose){message("---- Adding buffer around bird data extent ----
        --> Make sure the scale units in buffer_extent make sense with the proj used <--")}
      bbox_shape <- sf::st_buffer(bbox_shape, buffer_extent)
    }

    #plot(bbox_shape$x)
    #plot(bbox_shape_buf$x,add = T, border = "red")

    sf::st_agr(shape) = "constant" # assumme attribute geometry is consistent throughout (see above)
    mm <- sf::st_crop(shape, bbox_shape)

    #plot(mm$geometry)

  } else{
    mm <- shape
  }

  if(verbose){message("Adding offshore labelling")}
  #### the crop
  mm$overlap = 1

  #plot(b)
  #plot(mm$geometry, add=TRUE)
  mm_join <- sf::st_join(b_sf, mm, join = sf::st_covered_by) #geometric predicate 'within',
  mm_join$overlap <- ifelse(is.na(mm_join$overlap),1,0)
  data$offshore =  mm_join$overlap

  if(plot){
    if(verbose){message("Local device plot check")}
    x11()
    BBox <- sf::st_bbox(b_sf)
    plot(mm$geometry,xlim=c(BBox[1],BBox[3]),ylim=c(BBox[2],BBox[4]),col="lightblue")
    plot(b_sf$geometry, add=TRUE, cex=0.3, pch=21, col="black", bg="black")
    plot(b_sf[which(data$offshore == 1),]$geometry, col="red",add=TRUE,pch=21,bg="red",cex=0.3)

  }

  # --------------------------------------------------------- #
  data <- give_attributes(data, attr_list)

  # --------------------------------------------------------- #
  # new attributes on current function run
  if(is.null(attr(data, "offshore") )){
    attr(data, "offshore") <- "offshore"
  }

  # sub_attributes for arguments
  attr(attr(data, "offshore"), "use_crs_shape") <- use_crs_shape
  attr(attr(data, "offshore"), "crop") <- crop
  attr(attr(data, "offshore"), "buffer_extent") <- buffer_extent
  #attr(attr(data, "offshore"), "shape") <- shape # could get memory hungry!
  attr(attr(data, "offshore"), "p4s") <- p4s

  # -------------------- #
  # update the extra rows slot

  if(is.null(attr(data, "define_trips") )){
    attr(data, "define_trips") <- "define_trips"
  }
  attr(attr(data, "define_trips"), "extra_rows") <- data[data$extra_row == 1,]

  # remove extra rows from the main data
  data <- data[data$extra_row == 0,]

  # -------------------- #

  return(data)

}




