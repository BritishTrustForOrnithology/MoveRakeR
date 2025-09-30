#' Distance of telemetry fixes to nearest mainland coastline
#'
#' Function for calculating the nearest distance to coast both offshore and inland using a specified polygon.
#'
#' We may need to calculate how far animals may have travelled from a nearest coastline. Function \code{dist2coast}
#' fills that need, although this is a single GIS task obviously achievable in many ways outside this function.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param shape the function expects a single sf shape (sf MULTIPOLYGON or POLGON) representing the coastline.
#' @param crop logical defaulting to TRUE as to whether the polygon should be cropped by a square
#' extent (in association with the buffer_extent. This can greatly speed up the process
#' by not having the function attempt to compute nearest distances over an unnecessary large extent,
#' but the user \strong{MUST BE CONFIDENT} that the points bounded by the cropped shape outline will be those closest, and not
#' for instance a separate part of the polygon outside the crop, or the border of the actual crop. A warning
#' directs attention to this issue - it is best to plot first and carefully assess.
#' @param plot logical defaulting to TRUE giving a local pop up plot window.
#' @param use_crs_shape logical defaulting to TRUE whether to use the projection of the shape provided for bird data, otherwise
#' the function will project both bird data and the shape using the p4s argument.
#' @param buffer_extent A value to further add around the bird data to circumvent potential issues
#' of edges of the cropped map missing potential true land area; a default value is specified as 50000
#' but this will depend on map units and it is suggested this is checked before hand if using this option.
#' @param p4s The \code{sf} projection required for overlap processing, defaulting to BNG \code{epsg:3035}.
#' @param verbose logical, defaulting to TRUE, should detailed output messaging be returned on run of the function.
#'
#' @return The same dataset object as input with an additional column of dist2coast in metres.
#'
#' @seealso [MoveRakeR::offshore], [MoveRakeR::trip_stats]
#'
#' @examples
#'
#' ############### MAP PREPARATION ###############
#' # Idea here is to take a European map from mapdata and maps packages
#' # and combine with an even higher res map of UK and Ireland from personal libraries
#' # Using BNG projection for now, and having converted to sp format
#'
#' # R packages not bundled with MoveRakeR
#' #remotes::install_github("ropensci/rnaturalearthhires")
#' #install.packages('rnaturalearth')
#' library(rnaturalearthhires)
#' library(rnaturalearth)
#'
#' spdf_world <- ne_download(scale = 10, type = "countries")
#' Europe <- spdf_world[spdf_world$SUBREGION %in% c("Western Europe","Eastern Europe","Northern Europe","Southern Europe"),]
#' Europe_sf = st_crop(Europe, xmin = -25, ymin=32, xmax=35, ymax=70)
#' Europe_sf <- sf::st_transform(Europe_sf, 3035)
#' plot(Europe_sf$geometry)
#'
#' data = data %>% clean_GPS() %>% dist2coast(use_crs_shape = TRUE, shape = Europe_sf,
#'     crop = TRUE, plot = TRUE, buffer_extent = 50000, p4s = 3035)
#'
#' @export
dist2coast <- function(data, use_crs_shape = TRUE, shape = NULL, crop = TRUE, plot = FALSE,
                      buffer_extent = 50000, p4s = 3035, verbose = TRUE){

  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  #######
  if(verbose){message("Adding distances (m) to coast inland and offshore")}

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

  #######
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

  if(crop){

    if(verbose){message("Using a further crop of the shape based on bird extent:
       --> CAUTION: make sure GPS points are away from edges of the map! <--")}

    BBox <- sf::st_bbox(b_sf) # box around the GPS point extent

    # get the bounding box of the animal sf object, buffer by chosen amount
    bbox_animal <- sf::st_bbox(b_sf)
    bbox_pol <- sf::st_as_sfc(bbox_animal)
    extent_polygon <- sf::st_as_sf(bbox_pol)

    if(!is.null(buffer_extent)){
      if(verbose){message("---- Adding buffer around bird data extent ----
        --> Make sure the scale units in buffer_extent make sense with the proj used <--")}
      extent_polygon <- sf::st_buffer(extent_polygon, buffer_extent)
    }

    ex = sf::st_bbox(extent_polygon)

    sf::st_agr(shape) = "constant" # assumme attribute geometry is consistent throughout (see above)
    mm <- sf::st_crop(shape, ex)

  } else{
    mm <- shape
  }

  ## STRONGLY ADVISED THAT YOU VIEW THE PLOT AND SEE IF ANY POINT GO NEARER THE EXTENT IN WHICH
  ## CASE NEEDS EXPANDING OR RUNNING SEPARATELY
  if(plot){

    x11()
    if(crop){

      plot(mm$geometry, xlim = c(ex[[1]],ex[[3]]), ylim = c(ex[[2]],ex[[4]]) )

    } else{
      plot(mm$geometry)
    }
    # points
    plot(b_sf$geometry, cex=0.2, add=TRUE, pch=21, col="red", bg="red")

  }

  ################### Distance calculations ######################
  ##### distance to nearest polygon - but....only for offshore points lying outside poly in this example
  ##### see: https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

  if(verbose){message("--- Calculating distance of closest point along LINESTRING to each GPS fix ---")}

  start = Sys.time()

  # this is too slow really
  # cast to line string

  mm <- sf::st_union(mm)
  L = sf::st_cast(mm, "MULTILINESTRING")

  b_sf %>% ungroup() %>%
    mutate(
      my_linestring = sf::st_nearest_points(geometry, L),
      closest_point = sf::st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)]
    ) %>%
    {. ->> closest_points}

  # get distance using by_element to bypass original mapply sluggishness: https://github.com/r-spatial/sf/issues/1044
  vv = sf::st_distance(b_sf$geometry, closest_points$closest_point, by_element = TRUE)

  data$dist2coast <- vv

  end = Sys.time()

  val_tke <- as.vector(difftime(end,start, unit = 'secs'))
  units = 'secs'
  if(val_tke > 60){
    val_tke <- val_tke /60
    units = 'mins'
  }
  if(verbose){print(paste0("--- Time ellapsed: ", round(val_tke,3), " ", units, " ---"))}

  # --------------------------------------------------------- #
  data <- give_attributes(data, attr_list)

  # --------------------------------------------------------- #
  # new attributes on current function run
  if(is.null(attr(data, "dist2coast") )){
    attr(data, "dist2coast") <- "dist2coast"
  }

  # sub_attributes for arguments
  attr(attr(data, "dist2coast"), "use_crs_shape") <- use_crs_shape
  attr(attr(data, "dist2coast"), "crop") <- crop
  attr(attr(data, "dist2coast"), "buffer_extent") <- buffer_extent
  #attr(attr(data, "dist2coast"), "shape") <- shape # could get memory hungry!
  attr(attr(data, "dist2coast"), "p4s") <- p4s

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



