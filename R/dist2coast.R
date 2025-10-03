#' Distance of telemetry fixes to nearest mainland coastline
#'
#' Function for calculating the nearest distance to coast both offshore and inland using a specified polygon.
#'
#' We may need to calculate how far animals may have travelled from a nearest coastline. Function \code{dist2coast}
#' fills that need, although this is a single GIS task obviously achievable in many ways outside this function.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param use_crs_shape logical defaulting to TRUE whether to use the projection of the shape provided for bird data, otherwise
#' the function will project both bird data and the shape using the p4s argument.
#' @param shape the function expects a single sf shape (sf MULTIPOLYGON or POLGON) representing the coastline.
#' @param crop logical defaulting to TRUE as to whether the polygon should be cropped by a square
#' extent (in association with the buffer_extent. This can greatly speed up the process
#' by not having the function attempt to compute nearest distances over an unnecessary large extent,
#' but the user \strong{MUST BE CONFIDENT} that the points bounded by the cropped shape outline will be those closest, and not
#' for instance a separate part of the polygon outside the crop, or the border of the actual crop. A warning
#' directs attention to this issue - it is best to plot first and carefully assess.
#' @param simplify TRUE/FALSE defaults to TRUE, using \code{rmapshaper::ms_simplify}.
#' @param keep Parameter of \code{rmapshaper::ms_simplify}, defaults to 0.05 (5-10% points retains).
#' @param plot logical defaulting to TRUE giving a local pop up plot window.
#' @param lon_min Minimum longitude WGS84 for plotting if plot = TRUE (default = 0).
#' @param lon_max Minimum longitude WGS84 for plotting if plot = TRUE (default = 10).
#' @param lat_min Minimum longitude WGS84 for plotting if plot = TRUE (default = 50).
#' @param lat_max Minimum longitude WGS84 for plotting if plot = TRUE (default = 55).
#' @param buffer_extent A value to further add around the bird data to circumvent potential issues
#' of edges of the cropped map missing potential true land area; a default value is specified as 50000
#' but this will depend on map units and it is suggested this is checked before hand if using this option.
#' @param p4s The \code{sf} projection required for overlap processing, defaulting to BNG \code{epsg:3035}.
#' @param verbose logical, defaulting to TRUE, should detailed output messaging be returned on run of the function.
#'
#' @return The same dataset object as input with an additional column of dist2coast in metres and a column of
#' onshore TRUE/ALSE for if points were inside the shape (onshore) or outside the shape (offshore).
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
dist2coast <- function (data, use_crs_shape = TRUE, shape = NULL, crop = TRUE,
                        simplify = TRUE, keep = 0.05, # rshapemapper arguments
                        lon_min = 0, # for plotting
                        lon_max = 10,
                        lat_min = 50,
                        lat_max = 55,
                        plot = FALSE, buffer_extent = 50000, p4s = 3035, verbose = TRUE){

  cond <- ifelse(all(c("TagID", "DateTime", "longitude", "latitude") %in%
                       names(data)), TRUE, FALSE)
  if(!cond) {
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }
  if(verbose) {
    message("Adding distances (m) to coast inland and offshore")
  }
  attr_list <- get_attributes(data)
  extra_rows <- NULL
  extra_rows <- attr(attr(data, "define_trips"), "extra_rows")
  if(!is.null(extra_rows)) {
    if(nrow(extra_rows) > 0) {
      message("Extra rows found from define_trips() function run")
      extra_rows$extra_row = 1
      data = data %>% mutate(extra_row = 0) %>% bind_rows(.,
                                                          extra_rows) %>% arrange(TagID, DateTime)
    }
  }
  if(!inherits(data,"sf")){
    b_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

    if(verbose) {
      message("Projecting shapes using p4s argument")
    }

  } else{
    if(verbose) {
      message("sf data detected, projecting using p4s")
      b_sf <- data
    }
  }

  if(use_crs_shape) {
    b_sf <- sf::st_transform(b_sf, sf::st_crs(shape))
  } else{
    b_sf <- sf::st_transform(b_sf, p4s)
    if (!identical(sf::st_crs(shape), sf::st_crs(b_sf))) {
      shape <- sf::st_transform(shape, p4s)
    }
  }

  if(crop) {
    if(verbose) {
      message("Using a further crop of the shape based on bird extent:\n       --> CAUTION: make sure GPS points are away from edges of the map! <--")
    }
    BBox <- sf::st_bbox(b_sf)
    bbox_animal <- sf::st_bbox(b_sf)
    bbox_pol <- sf::st_as_sfc(bbox_animal)
    extent_polygon <- sf::st_as_sf(bbox_pol)
    if(!is.null(buffer_extent)) {
      if(verbose) {
        message("---- Adding buffer around bird data extent ----\n        --> Make sure the scale units in buffer_extent make sense with the proj used <--")
      }
      extent_polygon <- sf::st_buffer(extent_polygon, buffer_extent)
    }
    ex = sf::st_bbox(extent_polygon)
    sf::st_agr(shape) = "constant"
    mm <- sf::st_crop(shape, ex)
  } else{
    mm <- shape
  }


  if(verbose) {
    message("--- Calculating distance of closest point along LINESTRING to each GPS fix ---")
  }

  start = Sys.time()

  ############## OLD VERY SLOW
  #mm <- sf::st_union(mm)
  #L = sf::st_cast(mm, "MULTILINESTRING")
  #b_sf %>% ungroup() %>% mutate(my_linestring = sf::st_nearest_points(geometry,
  #     L), closest_point = sf::st_cast(my_linestring, "POINT")[seq(2,nrow(.) * 2, 2)]) %>% {closest_points <<- .}
  #vv = sf::st_distance(b_sf$geometry, closest_points$closest_point,
  #                     by_element = TRUE)
  #data$dist2coast <- vv


  ############## NEW FASTER
  # Union land polygons into one geometry
  if(verbose){message("--- Make valid shape and union --- ")}
  mm <- sf::st_make_valid(mm)
  mm <- sf::st_union(mm)

  # Keep only keep*100 to (keep*100)+5% of points
  if(simplify){
    if(verbose){message("--- Simplifying using rmapshaper (keep = ", keep, ", keep_shapes = TRUE) ---")}
    mm_simple <- rmapshaper::ms_simplify(mm, keep = keep, keep_shapes = TRUE)
  } else{
    mm_simple <- mm
  }

  # Coastline as LINESTRING, for distance calcs
  coastline <- st_cast(mm_simple, "MULTILINESTRING")

  # Inside/outside classification
  b_sf <- b_sf %>%
    mutate(
      onshore = lengths(st_within(geometry, mm_simple)) > 0
    )

  # Distance to coast (regardless of in/out)
  b_sf <- b_sf %>%
    mutate(
      dist2coast = st_distance(geometry, coastline)
    )

  b_sf$dist2coast <- as.numeric(b_sf$dist2coast)
  data$dist2coast <- b_sf$dist2coast
  data$onshore <- b_sf$onshore

  ############ PLOT
  if(plot){
    if(verbose){message("--- Plot checker --- ")}
    # Example: your desired lon/lat box
    lonlat_bbox <- st_sfc(
      st_polygon(list(rbind(
        c(lon_min, lat_min),
        c(lon_min, lat_max),
        c(lon_max, lat_max),
        c(lon_max, lat_min),
        c(lon_min, lat_min)
      ))),
      crs = 4326  # WGS84
    )

    # Transform to map CRS
    proj_bbox <- st_transform(lonlat_bbox, st_crs(b_sf))

    # Extract numeric bbox for plotting
    bbox_nums <- st_bbox(proj_bbox)
    ggplot() +
      geom_sf(data = b_sf, aes(color = onshore)) +
      geom_sf(data = coastline, color = "black", size = 0.4) +

      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
      scale_size_continuous(range = c(1, 3)) +
      theme_minimal() +
      coord_sf(xlim = c(bbox_nums["xmin"], bbox_nums["xmax"]),
               ylim = c(bbox_nums["ymin"], bbox_nums["ymax"]))

    #x11()
    #if (crop) {
    #  plot(mm$geometry, xlim = c(ex[[1]], ex[[3]]), ylim = c(ex[[2]],
    #                                                         ex[[4]]))
    #} else{
    #  plot(mm$geometry)
    #}
    #plot(b_sf$geometry, cex = 0.2, add = TRUE, pch = 21,
    #     col = "red", bg = "red")
  }

  #RakeRvis::RakeRvis(Track(data), shapes = list(shape))
  ################

  end = Sys.time()
  val_tke <- as.vector(difftime(end, start, unit = "secs"))
  units = "secs"
  if(val_tke > 60) {
    val_tke <- val_tke/60
    units = "mins"
  }
  if(verbose) {
    print(paste0("--- Time ellapsed: ", round(val_tke, 3),
                 " ", units, " ---"))
  }
  data <- give_attributes(data, attr_list)
  if(is.null(attr(data, "dist2coast"))) {
    attr(data, "dist2coast") <- "dist2coast"
  }
  attr(attr(data, "dist2coast"), "use_crs_shape") <- use_crs_shape
  attr(attr(data, "dist2coast"), "crop") <- crop
  attr(attr(data, "dist2coast"), "buffer_extent") <- buffer_extent
  attr(attr(data, "dist2coast"), "p4s") <- p4s
  if(is.null(attr(data, "define_trips"))) {
    attr(data, "define_trips") <- "define_trips"
  }

  if(exists("extra_row", data)){
    attr(attr(data, "define_trips"), "extra_rows") <- data[data$extra_row == 1, ]
    data <- data[data$extra_row == 0, ]
  }

  return(data)
}

