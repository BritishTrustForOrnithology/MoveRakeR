#' Compute RMS/smoothed trajectory speeds
#'
#' @description
#' The \code{traj_speed_sm} function uses \code{data.table} to compute trajectory speed smoothing for the variable
#' 'traj_speed'. This is done using either three preset root mean square (RMS) 'methods', or by supplying a bespoke version of the
#' users choosing. For built-in methods, these are as follows:
#' 1. `mcconnell`: (default) reproduces the McConnell filter (McConnell et al. 1992)
#'    - This is a RMS smoothing of trajectory speed as also suggested by the \code{trip} package (Sumner et al. 2009, Sumner 2011).
#'    - For a given set of trajectory speeds vij, the Vi smoother is given as:
#'        Vi = sqrt(0.25 x sum(from j = -2 and where j is not eq 0, to j = 2) of (vi,j+1)^2);
#'      where Vi velocity of the "ith" location is determined from vij velocity between successive locations i and j.
#'      This uses:
#'    - a five-point window;
#'    - excludes the center point;
#'    - returns NA at the edges if full window not available.
#' 2. `flexi_clip`: generalises the RMS smoothing above with variable window size
#'    - `points_each_side` controls the number of points before and after;
#'    - `include_centre` optionally includes the center point;
#'    - **clips window at edges** to use only available points (exactly matches original per-window behavior).
#' 3. `flexi_vec`: also generalizes RMS smoothing using **vectorized offsets**
#'    - same `points_each_side` and `include_centre` as flexi_clip but;
#'    - uses `shift()` to gather neighbors;
#'    - **ignores missing neighbors at edges** (`NA`s removed via `rowMeans(..., na.rm = TRUE)`);
#'    - much faster for large datasets, but edge behavior may differ slightly to flexi_clip.
#' 4. `fun` and `fun_args`: bespoke functions supplied
#'    - these override the choices of methods, points and centre-point inclusion as given above, such as rolling means
#'      and medians, weighted moving averages, quantiles, and potentially methods from signal processing and wider spline/LOESS smoothers - see examples.
#'
#' However, \strong{beware of using the smoothing/RMS options DIRECTLY to flag fixes}. This is instead best seen
#' as part of a process to identify where along the trajectory there may be issues. The smoothing process will iron out
#' spikes in the data and can be useful as a tool to consider potential erroneous points - see McConnell et al. (1992).
#'
#' Also beware if identifying spikes in erroneous points to exclude the centre-point. It is included as a flexible option
#' but the McConnell was designed to identify transient spikes in speed, not to smooth over the exact point itself.
#' The idea is to compute a local RMS around a point, but using its neighbors to avoid the spike “contaminating”
#' its own value. Therefore, use the `include_centre` option with caution.
#'
#' @param data data.frame or data.table containing trajectory data (must include TagID and optionally gapsec)
#' @param within_gap logical; whether to group by gapsec
#' @param points_each_side integer, default 2; number of points before/after center for flexible RMS and
#' used to in turn compute window_size as 2 * points_each_side + ifelse(include_centre, 1, 0).
#' @param include_centre logical (default FALSE); whether to include the center point in flexible RMS.
#' @param method one of three options: (1) "mcconnell" (efault) to use the original McConnell 5-point filter,
#' (2) "flexi_clip" to use flexible RMS with per-window clipping and (3) "flexi_vec" to use flexible RMS
#' with vectorized offsets (which is faster, but edges may differ). The default method is flexi_clip, which
#' matches more closely generally the original mcconnell but with less strict boundary conditions, point numbers and centre-point inclusion.
#' @param fun The name of a bespoke function argument supplied, default NULL.
#' @param fun_args Arguments to be passes to `fun` as a list(), default empty list, activated if fun is not NULL.
#' @param verbose logical; print progress messages.
#'
#' @return A data.table with an additional column `traj_speed_sm` containing the smoothed trajectory speed, as well as
#' `traj_speed` as computed from the \code{traj_speed()} function.
#'
#' @references
#'
#' McConnell, B. J. and Chambers, C. and Fedak, M. A. (1992) Foraging ecology of southern elephant seals in
#' relation to the bathymetry and productivity of the southern ocean. \emph{Antarctic Science}, \strong{4}, 393-398.
#'
#' Sumner MD, Wotherspoon SJ, Hindell MA (2009). Bayesian estimation of animal movement from archival and
#' satellite tags. \emph{PLoS ONE}, \strong{4(10)}.
#' \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0007324}.
#'
#' Sumner MD (2011). \emph{The Tag Location Problem}. Ph.D. thesis, University of Tasmania.
#' \url{https://figshare.utas.edu.au/articles/thesis/The_tag_location_problem/23209538}.
#' @seealso [MoveRakeR::speed_filt]
#'
#' @examples
#'
#' # ------------------------------------- #
#' Built in RMS methods
#' # ------------------------------------- #
#' data <- Track(yourdata)
#'
#' # compue gapsections:
#' data <- gap_section(data, GAP = 5*60*60, tol = 0.2)
#'
#' # simple traj_speed:
#' data <- traj_speed(data)
#' data$traj_speed
#'
#' # Or compute the root mean square speed using forward and backward windows over the data
#' # This is similar to the trip package
#' test = traj_speed_sm(data, method = "flexi_clip")
#' test2 = traj_speed_sm(data, method = "flexi_vec")
#' test3 = traj_speed_sm(data, method = "mcconnell")
#'
#' # test one of your animals:
#' anim = "animal1"
#'
#' test_sub = test[test$TagID %in% "animal1",]
#' test2_sub = test2[test2$TagID %in% "animal1",]
#' test3_sub = test3[test3$TagID %in% "animal1",]
#'
#' ## start of vector:
#' test_sub$traj_speedSM[1:5] # NA, NA, 0.1334, 0.0986, 0.0562
#' test2_sub$traj_speedSM[1:5] # 0.1615, 0.0997, 0.0406, 0.0351, 0.0684
#' test3_sub$traj_speedSM[1:5] # NA, NA, 0.1478, 0.1084, 0.0594
#'
#' # flexi_clip: per window clipping: first two points do not have enough neighbors so RMS = NA.
#' # Then clipping applies for subsequent points. Center included.
#' #
#' # flexi_vec: Vectorised offsets ignores NAs. RMS is computed even for first points using whatever
#' # neighbors exist. Result is slightly lower in some positions.
#' #
#' # mcconnell: requires full 5-point window = first 2 points = NA.
#' # Center excluded, RMS computed on x[c(1,2,4,5)].
#'
#' ## end of vector
#' test_sub$traj_speedSM[(nrow(test_sub)-5):nrow(test_sub)] # 0.0431, 0.0414, 0.0399, NA, NA, NA
#' test2_sub$traj_speedSM[(nrow(test2_sub)-5):nrow(test2_sub)] # 0.0491, 0.0387, 0.0323, 0.0357, 0.0434, NaN
#' test3_sub$traj_speedSM[(nrow(test3_sub)-5):nrow(test3_sub)] # 0.0482, 0.0312, 0.0407, NA, NA, NA
#'
#' # flexi_clip: Clipping applies — last 3 points do not have enough neighbors for full window so the last 3 RMS = NA.
#' #
#' # flexi_vec: Vectorized offsets compute RMS on available neighbors, ignoring NAs. Last point becomes
#' # NaN because all neighbors are shifted outside bounds (0/0 mean).
#' #
#' # mcconnell: Last 3 points cannot satisfy full 5-point window so = NA. Center excluded.
#'
#' # ------------------------------------- #
#' # Bespoke functions:
#' # ------------------------------------- #
#' library(data.table)
#'
#' # Rolling mean
#' fun_rollmedian <- function(x, y, k = 5){
#'   frollapply(y, n = k, FUN = median, align = "center", fill = NA)
#' }
#'
#' # Rolling median
#' fun_rollmean <- function(x, y, k = 5){
#'   frollapply(y, n = k, FUN = mean, align = "center", fill = NA)
#' }
#'
#' # Rolling quantile
#' fun_quantile <- function(x, y, k = 5, probs = 0.75){
#'   frollapply(y, n = k, FUN = function(v) quantile(v, probs = probs, na.rm = TRUE), align = "center", fill = NA)
#' }
#' # Exponentially weighted moving average (EWMA)
#' fun_ewma <- function(x, y, alpha = 0.3){
#'    out <- numeric(length(y))
#'    out[1] <- y[1]
#'    for(i in 2:length(y)) out[i] <- alpha * y[i] + (1 - alpha) * out[i-1]
#'    out
#' }
#'
#' # Rolling RMS (user-defined window), as in method = "flexi_clip", variant of RMS but fully flexible:
#' fun_rms <- function(x, y, points_each_side = 2, include_centre = TRUE){
#'   window_size <- 2 * points_each_side + if(include_centre) 1 else 0
#'   frollapply(y, n = window_size, FUN = function(v) sqrt(mean(v^2)), align = "center", fill = NA)
#' }
#'
#' # The McConnell function as coded in method = "mcconnell"
#' fun_mcconnell <- function(x, y) {
#'   n <- length(y)
#'   out <- rep(NA_real_, n)
#'
#'   if(n < 5) return(out)  # not enough points to apply filter
#'
#'   # Apply 5-point filter (i-2, i-1, i+1, i+2)
#'   for(i in 3:(n-2)) {
#'     v <- y[c(i-2, i-1, i+1, i+2)]
#'     out[i] <- sqrt(0.25 * sum(v^2))
#'   }
#'   return(out)
#' }
#'
#' # ------------------------ #
#' # Application in traj_speed_sm()
#'
#' test = traj_speed_sm(data, fun = fun_rollmedian, fun_args = list(k = 7)) # Rolling median
#' test1 = traj_speed_sm(data, fun = fun_rollmean, fun_args = list(k = 7)) # Rolling mean
#' test2 = traj_speed_sm(data, fun = fun_ewma, fun_args = list(alpha = 0.3)) # EWMA
#' test3 = traj_speed_sm(data, fun = fun_quantile, fun_args = list(k = 5, probs = 0.9)) # rolling quantile
#' test4 = traj_speed_sm(data, fun = fun_rms, fun_args = list(points_each_side = 2, include_centre = TRUE)) # rolling RMS
#' test5 = traj_speed_sm(data, method = "flexi_clip", include_centre = TRUE, points_each_side = 2) # the flexi_clip built-in method
#' test6 = traj_speed_sm(data, fun = fun_rms, fun_args = list(points_each_side = 2, include_centre = FALSE)) # the rolling RMS excluding centre
#' test7 = traj_speed_sm(data, method = "flexi_clip", include_centre = FALSE, points_each_side = 2) # the flexi_clip built-in method, excluding centre
#' test8 = traj_speed_sm(data, method = "mcconnell") # using method = mconnell
#' test9 = traj_speed_sm(data, fun = fun_mcconnell) # using supplied function for mcconnell
#'
#' plot(1:100, test$traj_speed[1:100], typ = "l")
#' lines(1:100, test1$traj_speedSM[1:100], col = "red")
#' lines(1:100, test2$traj_speedSM[1:100], col = "blue")
#' lines(1:100, test3$traj_speedSM[1:100], col = "yellow2")
#' lines(1:100, test4$traj_speedSM[1:100], col = "green")  # same as built-in flexi_clip incl. centre below
#' lines(1:100, test5$traj_speedSM[1:100], col = "darkgreen") # SAME as bespoke fun_rms fun incl centre above
#' lines(1:100, test6$traj_speedSM[1:100], col = "cyan") # same as built-in flexi_clip excl. centre below
#' lines(1:100, test7$traj_speedSM[1:100], col = "purple") # SAME as bespoke fun_rms fun excl centre above
#' lines(1:100, test8$traj_speedSM[1:100], col = "darkblue") # same as "mcconnell" fun supplied below
#' lines(1:100, test9$traj_speedSM[1:100], col = "orange") # same as method = "mcconnell" above
#'
#' # Rolling median replaced each traj_speed point with the median of a local window of points.
#' # This reduces the effect of spikes or outliers while preserving the general trend of the trajectory speed.
#' # The rolling median is robust to outliers. Unlike the mean, a single very high or low speed does not
#' # distort the smoothed value.
#' #
#' # The rolling mean, reduces spikes but without median’s robustness, smoothing short-term noise with edges filled with NA.
#' #
#' # The rolling quantile is robust to outliers, useful if you care about high-speed excursions (upper quantile)
#' # instead of median or mean;
#' #
#' # The rolling RMS is covered under 'methods' in the traj_speed_sm() function. This can be the same
#' # as 'flexi_clip', but is fully parameterised so users can adjust points each side or include/exclude center.
#'
#' # The Exponentially weighted moving average (EWMA) gives more weight to recent points, which is
#' # fast, simple, useful for highlighting trends and dampening noise
#'
#' ### Others?
#' # Savitzky-Golay filter
#' # From signal processing — preserves peaks while smoothing:
#' # Keeps peaks and slopes intact
#' # Useful for trajectories with sudden accelerations
#' # But...fits a local polynomial in each window and evaluates it at the center, so can overshoot and go negative
#' # and can give lagged effects
#'
#' install.packages('signal')
#' library(signal)
#' fun_sgolay <- function(x, y, p = 3, n = 7){ # probably not appropriate
#'   signal::sgolayfilt(y, p = p, n = n)
#' }
#'
#' test4 = traj_speed_sm(data, fun = fun_sgolay, fun_args = list(n = 7))
#' plot(1:100, test4$traj_speed[1:100], typ = "l")
#' lines(1:100, test4$traj_speedSM[1:100], col = "red")
#'
#' #Cubic smoothing spline, loess or gam mgcv will smooth over the trajctory so
#' # may result in flattening of variation.
#'
#' @export
traj_speed_sm <- function(data,
                within_gap = FALSE,
                points_each_side = 2,
                include_centre = FALSE,
                method = c("mcconnell", "flexi_clip", "flexi_vec"),
                fun = NULL,
                fun_args = list(),
                verbose = TRUE){

  # Preserve within_gap flag
  wg <- within_gap

  # Ensure data is a data.table
  setDT(data)

  method = method[1]

  # ------- GROUPING LOGIC -------
  if(within_gap){
    if(verbose){message("Checking for existence of gapsec column ...")}

    if(!("gapsec" %in% names(data))){
      if(verbose){message("No gapsec column found. Using TagID only")}
      group_vars <- c("TagID")
    } else{
      if(verbose){message("Using TagID + gapsec")}
      group_vars <- c("TagID", "gapsec")
    }

  } else{
    if(verbose){message("Not checking for gapsec, using TagID only")}
    group_vars <- c("TagID")
  }

  # ------- TRAJECTORY SPEED --------
  df <- traj_speed(data, within_gap = wg) # adds 'traj_speed' column
  setDT(df)

  # If a custom function is provided, override the method
  if(!is.null(fun)){
    if(verbose) message("Using user-supplied custom smoothing function")

    df[
      , traj_speedSM := do.call(
        fun,
        c(list(x = seq_len(.N), y = traj_speed), fun_args)
      ),
      by = group_vars
    ]
    return(Track(df))
  }

  # ------- STRICT McCONNELL MODE -------
  if(method == "mcconnell"){

    if(verbose){message("Using strict McConnell filtering mode")}

    # McConnell uses a 5-point window but excludes the center
    df[
      , traj_speedSM := frollapply(
        traj_speed,
        n = 5,
        FUN = function(x) sqrt(0.25 * sum(x[c(1,2,4,5)]^2)),
        align = "center",
        fill = NA
      ),
      by = group_vars
    ]

  }

  if(method == "flexi_clip"){
    if(verbose){message("Using flexible RMS mode with per-window clipping (edges exact)")}

    # Compute window size
    window_size <- 2 * points_each_side + ifelse(include_centre, 1, 0)

    df[
      , traj_speedSM := frollapply(
        traj_speed,
        n = window_size,
        FUN = function(x) {
          center <- ceiling(length(x) / 2)

          if (include_centre) {
            idx <- c(
              seq_len(points_each_side),
              center,
              (center + 1):(center + points_each_side)
            )
          } else {
            idx <- c(
              seq_len(points_each_side),
              (center + 1):(center + points_each_side)
            )
          }

          # Clip indices at boundaries
          idx <- idx[idx <= length(x)]

          sqrt(mean(x[idx]^2))
        },
        align = "center"
      ),
      by = group_vars
    ]

  }

  if(method == "flexi_vec"){
    if(verbose){message("Using flexible RMS mode with vectorized offsets (fast, edges may differ)")}

    # Create vector of relative offsets
    offsets <- c(-points_each_side:-1, if (include_centre) 0, 1:points_each_side)

    # Process each group
    df[
      , traj_speedSM := {
        n <- .N
        # Preallocate output vector
        out <- numeric(n)
        out[] <- NA_real_

        # Loop over offsets to collect neighbor squared speeds
        mat <- sapply(offsets, function(off) {
          shift(traj_speed, n = off, type = ifelse(off < 0, "lag", "lead"))
        }, simplify = "matrix")

        # Compute RMS row-wise (ignoring NAs for edge padding)
        out <- sqrt(rowMeans(mat^2, na.rm = TRUE))

        out
      },
      by = group_vars
    ]

  }
  return(Track(df))
}




