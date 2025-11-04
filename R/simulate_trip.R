#' Simulate animal movement trips with correlated random walks
#'
#' These functions simulate artificial movement trajectories of animals departing from and returning to a
#' central hub (e.g. a colony or roost), using correlated random walk models.
#'
#' The function \code{simulate_trip()} generates a trajectory to simulate GPS data collected from animals,
#' with distinct behavioral phases (exploration, foraging, return) and adaptive return dynamics.
#' Step lengths and turning correlations differ by phase, and the return phase meanders
#' naturally towards the hub and extra points are added if needed to reach the hub.
#'
#' \code{simulate_trip()} also incorporates a duration of time at the central place between trips, and randomises the
#' components related to number of points per trip (points_per_trip), DateTimes (dt), and the measures of straightness and
#' angle noise for each of the distributions for exploratory, foraging and return commute phases. These choices can be customised.
#'
#' @param n Integer. Total number of steps in the simulated trip (excluding any additional steps added for return to the hub), within \code{sim_data()}, default = 50.
#' @param hub_lat Numeric, latitude of the central hub, within \code{sim_data()} and \code{simulate_trip()}, default = 40.
#' @param hub_lon Numeric, longitude of the central hub, within \code{sim_data()} and \code{simulate_trip()}, default = 0.
#' @param dt Numeric, time difference between consecutive GPS fixes, for \code{sim_data()} and \code{simulate_trip()}, default = c("A001", "B002", "C003").
#' @param rho_explore Numeric between 0 and 1. Directional autocorrelation for the exploratory (outbound) phase. Higher values produce straighter paths, default 0.94 within \code{simulate_trip()}.
#' @param rho_forage Numeric between 0 and 1. Directional autocorrelation for the area-restricted search / foraging phase. Lower values produce more tortuous paths, default 0.4 \code{simulate_trip()}.
#' @param rho_return Numeric between 0 and 1. Directional autocorrelation for the return / commuting phase. Higher values produce straighter paths toward the hub, default 0.98, \code{simulate_trip()}.
#' @param step_frac_explore Numeric. Fraction of maximum step length to use in the exploratory phase. Controls relative movement speed/length, within \code{simulate_trip()}, default = 0.7.
#' @param step_frac_forage Numeric. Fraction of maximum step length to use in the foraging phase. Usually smaller to simulate local search behavior, within \code{simulate_trip()}, default = 0.3.
#' @param step_frac_return Numeric. Fraction of maximum step length to use in the return phase. Usually larger to generate straighter, faster commuting movement, within \code{simulate_trip()}, default = 1.0.
#' @param step_sd_explore Numeric. Fractional SD for step length in exploratory phase (step length multiplier), within \code{simulate_trip()}, default = 0.2.
#' @param step_sd_forage Numeric. Fractional SD for step length in foraging phase (step length multiplier), within \code{simulate_trip()}, default = 0.5.
#' @param step_sd_return Numeric. Fractional SD for step length in return phase (step length multiplier), within \code{simulate_trip()}, default = 0.3.
#' @param angle_noise_explore Numeric. Standard deviation of angular noise (radians) in the exploratory phase; controls path curviness, within \code{simulate_trip()}, default = pi/15.
#' @param angle_noise_forage Numeric. Standard deviation of angular noise (radians) in the foraging phase; controls path curviness, within \code{simulate_trip()}, default = pi.
#' @param angle_noise_return Numeric. Standard deviation of angular noise (radians) in the return phase; controls path curviness, within \code{simulate_trip()}, default = pi/20.
#' @param min_return_steps Integer. Minimum number of steps to complete the return phase, regardless of distance to the hub, within \code{simulate_trip()}, default = 10.
#' @param interpolate_equal_space Logical. If TRUE, additional points are interpolated to produce a track with more uniform spacing, within \code{simulate_trip()}, default = FALSE.
#' @param dt Numeric. Time step between successive locations, in seconds. Used for approximate speed and DateTime timestamp calculation, within \code{sim_data()} and \code{simulate_trip()}, default = 300 s.
#' @param tol Numeric, relative jitter for dt per fix to simulate irregular tag sampling (0 = fixed dt), used within \code{sim_data()}, default = 0.05.
#' @param max_speed Numeric, maximum speed in meters/second; used to constrain movement, within \code{sim_data()} and \code{simulate_trip()}, default = 50 m/s.
#' @param animals Character, concatenated string for ID of animals, for \code{sim_data()}, default of three animals "A001", "B001" and "C001".
#' @param points_per_trip Integer, for number of points per trip to simulate, default 50, within \code{sim_data()}.
#' @param n_trips, Integer, number of trips to simulate, default = 5, within \code{sim_data()}.
#' @param seed Integer for seed to set for reproducibility, within \code{sim_data()}, default = NULL (random seed).
#' @param cp_time_mean Numeric, mean central-place stay duration in hours between trips, default = 6 hours, within \code{sim_data()}.
#' @param cp_time_sd Numeric, standard deviation of central-place stay duration in hours, default = 1, within \code{sim_data()}.
#' @param points_variation Numeric (0–1), proportional variation in points per trip, default = 0.3, within \code{sim_data()}.
#' @param cp_time_jitter Numeric (hours), additional random jitter added to CP stay times, default = 0.5, within \code{sim_data()}.
#' @param default_ranges Named list of numeric vectors of length 2 specifying min and max values for trip-level parameters.
#'   Each trip randomly samples a value within the range for each parameter. Defaults are:
#'   \describe{
#'     \item{rho_explore}{c(0.6, 0.8) directional persistence during exploration phase.}
#'     \item{rho_forage}{c(0.2, 0.5) directional persistence during foraging phase.}
#'     \item{rho_return}{c(0.8, 0.99) directional persistence during return phase.}
#'     \item{step_frac_explore}{c(0.6, 0.8) proportion of max step length used in exploration.}
#'     \item{step_frac_forage}{c(0.2, 0.4) proportion of max step length used in foraging.}
#'     \item{step_frac_return}{c(0.9, 1.0) proportion of max step length used in return.}
#'     \item{angle_noise_explore}{c(pi/20, pi/4) angular noise (radians) during exploration.}
#'     \item{angle_noise_forage}{c(pi/2, pi) angular noise (radians) during foraging.}
#'     \item{angle_noise_return}{c(pi/50, pi/10) angular noise (radians) during return.}
#'   }
#'   Any parameters omitted from the list will fall back to these defaults. Any parameters omitted from the list
#'   will fall back to default ranges and dach trip randomly samples a value within the range for each parameter.
#' @param ... Additional parameters passed to \code{simulate_trip()} from \code{sim_data()}, e.g.,
#'   \code{rho_explore}, \code{rho_forage}, \code{rho_return}, \code{step_frac_explore},
#'   \code{step_frac_forage}, \code{step_frac_return}, \code{angle_noise_explore},
#'   \code{angle_noise_forage}, \code{angle_noise_return}, \code{min_return_steps}.
#'
#' @return From \code{simulate_trip()}, a \code{data.frame} with two columns:
#' \describe{
#'   \item{latitude}{Simulated latitude positions.}
#'   \item{longitude}{Simulated longitude positions.}
#' }
#'
#' From \code{sim_data()}, A tibble (or \code{Track} object if \code{Track()} is applied) containing:
#'   \describe{
#'     \item{TagID}{Animal identifier.}
#'     \item{trip}{Trip number.}
#'     \item{DateTime}{POSIXct timestamp of each fix.}
#'     \item{longitude}{Longitude of the point.}
#'     \item{latitude}{Latitude of the point.}
#'     \item{dist_m}{Distance (m) from previous point.}
#'     \item{dt_s}{Time difference (s) from previous point.}
#'     \item{speed_m_s}{Observed speed (m/s) between consecutive points.}
#'     \item{pdop}{Simulated GPS PDOP (1–9).}
#'     \item{nsats}{Simulated number of satellites (3–12).}
#'     \item{speed_basic}{Binary speed flag (0 = <300 m/s, 1 = >=300 m/s).}
#'   }
#'
#' @examples
#' # Simulate 100 trips for three animals with default parameters
#' fake_data <- simulate_trip(n = 100, hub_lat = 40, hub_lon = 0)
#'
#' plot(fake_data$longitude, fake_data$latitude, type = "l", col = as.numeric(as.factor(fake_data$TagID)))
#' #fake_data[fake_data$TagID == "A001",]
#' #fake_data %>% group_by(TagID, trip) %>% summarise(start_trip = min(DateTime))
#' #fake_data[fake_data$TagID == "A001",]
#'
#' plot_leaflet(fake_data)
#'
#'
#' @export
simulate_trip <- function(
    n = 50,
    hub_lat=40,
    hub_lon=0,
    rho_explore = 0.94,
    rho_forage = 0.4,
    rho_return = 0.98,
    step_frac_explore = 0.7,
    step_frac_forage = 0.3,
    step_frac_return = 1.0,
    step_sd_explore = 0.2,
    step_sd_forage  = 0.5,
    step_sd_return  = 0.3,
    angle_noise_explore = pi/15,
    angle_noise_forage  = pi/1,
    angle_noise_return  = pi/20,
    min_return_steps = 10,
    interpolate_equal_space = FALSE,
    dt = 300,
    max_speed = 50
){

  # --- Convert max speed to max step length (in degrees) ---
  step_max_m <- max_speed * dt
  step_max_deg <- step_max_m / 111320

  # Assign typical proportions of max step length by phase
  step_explore <- step_max_deg * step_frac_explore
  step_forage  <- step_max_deg * step_frac_forage
  step_return  <- step_max_deg * step_frac_return

  x <- numeric(n)
  y <- numeric(n)
  x[1] <- hub_lon
  y[1] <- hub_lat

  # Phases
  n_explore <- floor(n*0.25)
  n_forage <- floor(n*0.5)
  n_return <- n - n_explore - n_forage
  theta <- runif(1, 0, 2*pi)

  # Outbound exploratory
  for(i in 2:n_explore){
    step <- rnorm(1, step_explore, step_explore*step_sd_explore)

    # this is quite spiralled around the central place
    #theta <- rho_explore*theta + sqrt(1 - rho_explore^2)*rnorm(1, 0, angle_noise_explore)
    #
    # as is this slight directional bias
    #theta <- rho_explore*theta + (1 - rho_explore)*(atan2(y[i-1] - hub_lat, x[i-1] - hub_lon)) +
    #  sqrt(1 - rho_explore^2)*rnorm(1, 0, angle_noise_explore)
    #

    # vector from hub to current location
    dx <- x[i-1] - hub_lon
    dy <- y[i-1] - hub_lat
    outward_theta <- atan2(dy, dx)
    ## directional autocorrelation with outward bias
    theta <- rho_explore*theta + (1 - rho_explore)*outward_theta + sqrt(1 - rho_explore^2)*rnorm(1, 0, angle_noise_explore)

    # angle pointing directly away from hub
    #outward_theta <- atan2(y[i-1] - hub_lat, x[i-1] - hub_lon)
    #
    # limit the turning angle relative to outward
    #max_turn <- pi/8   # smaller = straighter
    #theta_diff <- runif(1, -max_turn, max_turn)
    #
    #theta <- outward_theta + theta_diff

    # update position
    x[i] <- x[i-1] + step*cos(theta)
    y[i] <- y[i-1] + step*sin(theta)
  }
  # Foraging / area-restricted search
  for(i in (n_explore+1):(n_explore+n_forage)){
    step <- rnorm(1, step_forage, step_forage*step_sd_forage)
    theta <- rho_forage*theta + sqrt(1 - rho_forage^2)*rnorm(1, 0, angle_noise_forage)
    x[i] <- x[i-1] + step*cos(theta)
    y[i] <- y[i-1] + step*sin(theta)
  }
  # Compute adaptive return
  last_x <- x[n_explore+n_forage]
  last_y <- y[n_explore+n_forage]
  dist_to_hub <- sqrt((hub_lon - last_x)^2 + (hub_lat - last_y)^2)
  n_return <- max(n_return, ceiling(dist_to_hub / step_return), min_return_steps)

  # Generate return steps
  for(i in 1:n_return){
    dx <- hub_lon - last_x
    dy <- hub_lat - last_y
    target_theta <- atan2(dy, dx)

    ## directional autocorrelation with bias toward hub
    #theta <- rho_return*theta + (1 - rho_return)*target_theta + sqrt(1 - rho_return^2)*rnorm(1, 0, angle_noise_return)

    ## limit angular deviation from hub to prevent veering away
    #max_angle_dev <- pi/4  # max deviation from hub
    #angle_diff <- atan2(sin(theta - target_theta), cos(theta - target_theta))
    #angle_diff <- pmin(pmax(angle_diff, -max_angle_dev), max_angle_dev)
    #theta <- target_theta + angle_diff

    # OR....just directly bias toward hub with small random noise
    theta <- target_theta + rnorm(1, 0, angle_noise_return)

    # step limited so we don’t overshoot hub
    step <- min(rnorm(1, step_return, step_return*step_sd_return), sqrt(dx^2 + dy^2))

    # update position
    last_x <- last_x + step*cos(theta)
    last_y <- last_y + step*sin(theta)

    idx <- n_explore + n_forage + i
    if(idx <= n){
      x[idx] <- last_x
      y[idx] <- last_y
    }
  }

  #n_explore + n_forage + n_return

  val = length(y)
  #plot(y ~ x)
  #lines(y ~ x)


  ##################################################################
  # If the animal is not yet at the central place - add in extra path of return phase

  # --- Commuting segment with correct spacing ---
  hub <- c(hub_lon, hub_lat)
  x_threshold <- 50       # meters considered "close enough"

  # compute mean speed of the main track
  track_df <- data.frame(
    longitude = x,
    latitude  = y,
    DateTime  = seq.POSIXt(from = as.POSIXct("2025-01-01 00:00"), by = dt, length.out = length(x))
  )
  distances <- geosphere::distHaversine(
    cbind(lag(track_df$longitude, default = first(track_df$longitude)),
          lag(track_df$latitude, default = first(track_df$latitude))),
    cbind(track_df$longitude, track_df$latitude)
  )
  dt_s <- as.numeric(difftime(track_df$DateTime, lag(track_df$DateTime, default = first(track_df$DateTime)), units = "secs"))
  speeds <- ifelse(dt_s > 0, distances / dt_s, 0)
  mean_speed <- mean(speeds, na.rm = TRUE)

  # distance to hub
  dist_to_hub <- geosphere::distHaversine(c(last_x, last_y), hub)

  if(dist_to_hub > x_threshold){

    # determine number of commuting points based on mean speed
    n_commute <- max(1, ceiling(dist_to_hub / (mean_speed * dt)))

    for(i in 1:n_commute){
      dx <- hub_lon - last_x
      dy <- hub_lat - last_y
      target_theta <- atan2(dy, dx)

      # directional autocorrelation
      theta <- rho_return*theta + (1 - rho_return)*target_theta + sqrt(1 - rho_return^2)*rnorm(1, 0, angle_noise_return)

      # step length in meters: mean speed per fix
      step_m <- dist_to_hub / (n_commute - i + 1)  # divide remaining distance evenly
      step_deg <- step_m / 111320  # convert to degrees

      # update position
      last_x <- last_x + step_deg*cos(theta)
      last_y <- last_y + step_deg*sin(theta)

      # append new point
      x <- c(x, last_x)
      y <- c(y, last_y)

      # recompute remaining distance
      dist_to_hub <- geosphere::distHaversine(c(last_x, last_y), hub)
    }

    # snap final point exactly to hub
    x[length(x)] <- hub_lon
    y[length(y)] <- hub_lat
  }

  #length(y)

  ###################################################
  if(interpolate_equal_space){


    # compute step lengths in meters
    coords <- cbind(x, y)
    steps_m <- geosphere::distHaversine(coords[-nrow(coords), ], coords[-1, ])

    # compute mean step length (for the main track)
    mean_step <- mean(steps_m, na.rm = TRUE)

    # threshold for adding intermediate points (e.g., 2x mean step)
    threshold <- 2 * mean_step

    # create vectors for new points
    new_x <- x[1]
    new_y <- y[1]
    new_datetime <- seq.POSIXt(from = as.POSIXct("2025-01-01 00:00"), by = dt, length.out = 1)

    for(i in 2:length(x)){
      step_m <- steps_m[i-1]
      n_extra <- max(0, ceiling(step_m / mean_step) - 1)

      # original start and end
      x0 <- x[i-1]
      y0 <- y[i-1]
      x1 <- x[i]
      y1 <- y[i]

      # original times
      t0 <- new_datetime[length(new_datetime)]
      t1 <- t0 + dt

      if(n_extra > 0){
        # interpolate positions
        xi <- seq(x0, x1, length.out = n_extra + 2)[-c(1, n_extra+2)]
        yi <- seq(y0, y1, length.out = n_extra + 2)[-c(1, n_extra+2)]
        # interpolate times
        ti <- seq(t0, t1, length.out = n_extra + 2)[-c(1, n_extra+2)]

        new_x <- c(new_x, xi)
        new_y <- c(new_y, yi)
        new_datetime <- c(new_datetime, ti)
      }

      # append the original end point
      new_x <- c(new_x, x1)
      new_y <- c(new_y, y1)
      new_datetime <- c(new_datetime, t1)
    }

    # replace old track with smoothed track
    x <- new_x
    y <- new_y
    DateTime <- new_datetime
  }


  # plot the smoothed track
  #plot(y ~ x, type = "l", col = "blue", lwd = 2)
  #points(y ~ x, pch = 16, cex = 0.5)

  #plot(y ~ x)
  #lines(y ~ x)


  #data.frame(latitude = y, longitude = x)

  # number of total points generated
  n_total <- length(x)

  # original planned counts
  #phase_counts <- c(n_explore, n_forage, n_return)

  # how many points are “extra” beyond the planned n
  n_total <- length(x)
  phase_vec <- c(
    rep("explore", n_explore),
    rep("forage", n_forage),
    rep("return", n_total - n_explore - n_forage)
  )

  # sanity check: make sure lengths match
  stopifnot(length(phase_vec) == n_total)

  # construct tibble
  tibble(
    longitude = x,
    latitude  = y,
    phase = phase_vec
  )
}

#' @rdname simulate_trip
#' @export
sim_data <- function(
    animals = c("A001", "B002", "C003"),
    n_trips = 5,
    points_per_trip = 50,
    hub_lat = 40,
    hub_lon = 0,
    dt = 300,
    tol = 0.05,
    cp_time_mean = 6,   # hours
    cp_time_sd = 1,
    points_variation = 0.3,  # proportion of variability in points per trip
    cp_time_jitter = 0.5,    # additional random jitter in hours
    max_speed = 50,           # m/s
    seed = NULL,
    min_return_steps = 10,
    default_ranges = NULL,
    ...
) {
  if(is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
  set.seed(seed)

  # internal default ranges if user doesn't supply
  internal_defaults <- list(
    rho_explore = c(0.6, 0.8),
    rho_forage  = c(0.2, 0.5),
    rho_return  = c(0.8, 0.99),
    step_frac_explore = c(0.6, 0.8),
    step_frac_forage  = c(0.2, 0.4),
    step_frac_return  = c(0.9, 1.0),
    angle_noise_explore = c(pi/20, pi/4),
    angle_noise_forage  = c(pi/2, pi),
    angle_noise_return  = c(pi/50, pi/10)
  )

  # merge user-supplied ranges with defaults
  if (is.null(default_ranges)) {
    default_ranges <- internal_defaults
  } else {
    for (nm in names(internal_defaults)) {
      if (!nm %in% names(default_ranges)) {
        default_ranges[[nm]] <- internal_defaults[[nm]]
      }
    }
  }

  fake_data <- list()

  for(a in animals) {
    # Random initial start time for this animal
    last_trip_end <- as.POSIXct("2025-01-01 00:00", tz = "UTC") + runif(1, -3600, 3600)

    for(trip in 1:n_trips) {
      # Randomize points per trip
      min_points <- floor(points_per_trip * (1 - points_variation))
      max_points <- ceiling(points_per_trip * (1 + points_variation))
      n_points <- sample(min_points:max_points, 1)

      # Randomize trip-specific behavioral parameters

      # Sample trip parameters from ranges
      sample_param <- function(rng) runif(1, rng[1], rng[2])
      rho_explore_trip <- sample_param(default_ranges$rho_explore)
      rho_forage_trip  <- sample_param(default_ranges$rho_forage)
      rho_return_trip  <- sample_param(default_ranges$rho_return)
      step_frac_explore_trip <- sample_param(default_ranges$step_frac_explore)
      step_frac_forage_trip  <- sample_param(default_ranges$step_frac_forage)
      step_frac_return_trip  <- sample_param(default_ranges$step_frac_return)
      angle_noise_explore_trip <- sample_param(default_ranges$angle_noise_explore)
      angle_noise_forage_trip  <- sample_param(default_ranges$angle_noise_forage)
      angle_noise_return_trip  <- sample_param(default_ranges$angle_noise_return)

      #rho_explore_trip <- runif(1, 0.6, 0.8)
      #rho_forage_trip  <- runif(1, 0.2, 0.5)
      #rho_return_trip  <- runif(1, 0.8, 0.99)

      #step_frac_explore_trip <- runif(1, 0.6, 0.8)
      #step_frac_forage_trip  <- runif(1, 0.2, 0.4)
      #step_frac_return_trip  <- runif(1, 0.9, 1.0)

      #angle_noise_explore_trip <- runif(1, pi/20, pi/4)
      #angle_noise_forage_trip  <- runif(1, pi/2, pi)
      #angle_noise_return_trip  <- runif(1, pi/50, pi/10)

      # Simulate the trip
      df <- simulate_trip(
        n = n_points,
        hub_lat = hub_lat,
        hub_lon = hub_lon,
        dt = dt,
        max_speed = max_speed,
        rho_explore = rho_explore_trip,
        rho_forage = rho_forage_trip,
        rho_return = rho_return_trip,
        step_frac_explore = step_frac_explore_trip,
        step_frac_forage = step_frac_forage_trip,
        step_frac_return = step_frac_return_trip,
        angle_noise_explore = angle_noise_explore_trip,
        angle_noise_forage = angle_noise_forage_trip,
        angle_noise_return = angle_noise_return_trip,
        min_return_steps = min_return_steps,
        ...
      )

      # Trip start time: after previous trip + nest stay
      nest_time <- rnorm(1, cp_time_mean, cp_time_sd) + runif(1, -cp_time_jitter, cp_time_jitter)
      trip_start <- last_trip_end + nest_time * 3600  # convert hours to seconds

      # Add DateTimes for this trip with per-fix jitter
      dt_jitter <- dt * runif(nrow(df), 1 - tol, 1 + tol)
      df$DateTime <- trip_start + cumsum(c(0, dt_jitter[-1]))

      #df$DateTime <- seq.POSIXt(from = trip_start, by = dt, length.out = nrow(df))

      df$TagID <- a
      df$trip <- trip

      # Update last_trip_end
      last_trip_end <- max(df$DateTime)

      fake_data[[length(fake_data) + 1]] <- df
    }
  }

  fake_data <- bind_rows(fake_data)

  # --- Add derived fields ---
  fake_data <- fake_data %>%
    arrange(TagID, DateTime) %>%
    group_by(TagID, trip) %>%
    mutate(
      dist_m = geosphere::distHaversine(
        cbind(lag(longitude, default = first(longitude)),
              lag(latitude, default = first(latitude))),
        cbind(longitude, latitude)
      ),
      dt_s = as.numeric(difftime(DateTime, lag(DateTime, default = first(DateTime)), units = "secs")),
      speed_m_s = ifelse(dt_s > 0, dist_m / dt_s, 0),
      pdop = round(pmin(9, pmax(1, rbeta(n(), 2, 5) * 8 + 1)), 1),
      nsats = round(pmin(12, pmax(3, rbeta(n(), 5, 3) * 9 + 3)))
    )

  fake_data$speed_basic <- ifelse(fake_data$speed_m_s < 300, 0, 1)

  return(fake_data)
}

