#' Simulate animal movement trips with correlated random walks
#'
#' These functions simulate artificial movement trajectories of animals departing from and returning to a
#' central hub (e.g. a colony or roost), using correlated random walk models.
#'
#' \code{simulate_behavioral_trip()} generates a trajectory with distinct behavioral
#' phases (exploration, foraging, return) and adaptive return dynamics.
#'
#' #' Generates an animal movement track with exploratory, foraging, and return phases.
#' Step lengths and turning correlations differ by phase, and the return phase meanders
#' naturally towards the hub. Extra points are added if needed to reach the hub.
#'
#' \code{simulate_smooth_crw_bridge()} generates a simpler correlated random walk trajectory that departs
#' from the hub, wanders, and is "bridged" back to the hub with spline smoothing.
#'
#' @param n Integer. Number of locations to simulate.
#' @param hub_lat Numeric, latitude of the central hub, within \code{sim_data()} default = 40.
#' @param hub_lon Numeric, longitude of the central hub, within \code{sim_data()} default = 0.
#' @param dt Numeric, time difference between consecutive GPS fixes, for \code{sim_data()}, default = c("A001", "B002", "C003").
#' @param animals Character, concatenated string for ID of animals, for \code{sim_data()}.
#' @param points_per_trip Integer, for number of points per trip to simulate, default 50, within \code{sim_data()}.
#' @param n_trips, Integer, number of trips to simulate, default = 5, within \code{sim_data()}.
#' @param seed Integer for seed to set for reproducibility, within \code{sim_data()}, default = 39.
#' @param rho_explore Numeric (0–1). Correlation in turning angle parameter during exploration (higher values = straighter movement).
#' @param rho_forage Numeric (0–1). Correlation in turning angle parameter during foraging (lower values = more tortuous movement).
#' @param rho_return Numeric (0–1). Correlation in turning parameter during return phase.
#' @param step_explore Numeric. Mean step length during exploration.
#' @param step_forage Numeric. Mean step length during foraging.
#' @param step_return Numeric. Mean step length during return.
#' @param min_return_steps Integer. Minimum number of steps allowed during return phase.
#' @param step_mean Numeric. Mean step length for \code{simulate_smooth_crw_bridge()}.
#' @param step_sd Numeric. Standard deviation of step length for \code{simulate_smooth_crw_bridge()}.
#' @param rho Numeric (0–1). Correlation parameter for \code{simulate_smooth_crw_bridge()}.
#'
#' @return A \code{data.frame} with two columns:
#' \describe{
#'   \item{latitude}{Simulated latitude positions.}
#'   \item{longitude}{Simulated longitude positions.}
#' }
#'
#' @examples
#' # Simulate a behavioral trip with default parameters
#' trip1 <- simulate_behavioral_trip(n = 100, hub_lat = 40, hub_lon = 0)
#' plot(trip1$longitude, trip1$latitude, type = "l")
#'
#' # Simulate a smooth CRW trajectory
#' trip2 <- simulate_smooth_crw_bridge(n = 100, hub_lat = 40, hub_lon = 0)
#' lines(trip2$longitude, trip2$latitude, col = "red")
#'
#' @export
simulate_behavioral_trip <- function(n, hub_lat, hub_lon, rho_explore = 0.9, rho_forage = 0.3,
                                     rho_return = 0.8, step_explore = 0.05, step_forage = 0.01,
                                     step_return = 0.03, min_return_steps = 10){
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
    step <- rnorm(1, step_explore, step_explore*0.2)
    theta <- rho_explore*theta + sqrt(1 - rho_explore^2)*rnorm(1, 0, pi/8)
    x[i] <- x[i-1] + step*cos(theta)
    y[i] <- y[i-1] + step*sin(theta)
    }
  # Foraging / area-restricted search
  for(i in (n_explore+1):(n_explore+n_forage)){
    step <- rnorm(1, step_forage, step_forage*0.5)
    theta <- rho_forage*theta + sqrt(1 - rho_forage^2)*rnorm(1, 0, pi/2)
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
    theta <- rho_return*theta + (1 - rho_return)*target_theta + sqrt(1 - rho_return^2)*rnorm(1, 0, pi/8)
    step <- min(rnorm(1, step_return, step_return*0.3), sqrt((hub_lon - last_x)^2 + (hub_lat - last_y)^2))
    last_x <- last_x + step*cos(theta)
    last_y <- last_y + step*sin(theta)
    idx <- n_explore + n_forage + i
    if(idx <= n){
      x[idx] <- last_x
      y[idx] <- last_y }
  }
  x[n] <- hub_lon
  y[n] <- hub_lat
  data.frame(latitude = y, longitude = x)
 }



#' @rdname simulate_behavioral_trip
#' @export
simulate_smooth_crw_bridge <- function(n, hub_lat, hub_lon, step_mean = 0.05, step_sd = 0.02, rho = 0.9){
  x <- numeric(n)
  y <- numeric(n)

  x[1] <- hub_lon
  y[1] <- hub_lat
  x[n] <- hub_lon
  y[n] <- hub_lat

  theta <- runif(1, 0, 2*pi)  # initial heading
  for(i in 2:(n-1)){
    step <- rnorm(1, mean = step_mean, sd = step_sd)
    theta <- rho * theta + sqrt(1 - rho^2) * rnorm(1, 0, pi/8)
    x[i] <- x[i-1] + step * cos(theta)
    y[i] <- y[i-1] + step * sin(theta)
  }

  # Apply linear bridge adjustment to hit hub exactly
  for(i in 2:(n-1)){
    frac <- (i-1)/(n-1)
    x[i] <- x[i] + frac * (hub_lon - x[n-1])
    y[i] <- y[i] + frac * (hub_lat - y[n-1])
  }

  # Optional spline smoothing
  x_smooth <- spline(x, n = n)$y
  y_smooth <- spline(y, n = n)$y
  data.frame(latitude = y_smooth, longitude = x_smooth)
}


#' @rdname simulate_behavioral_trip
#' @export
sim_data <- function(hub_lat = 40, hub_lon = 0, dt = 300, animals = c("A001", "B002", "C003"),
                      points_per_trip = 50, n_trips = 5, seed = 39){

  set.seed(seed)

  # Generate data for all animals
  fake_data <- bind_rows(lapply(animals, function(a) {
    bind_rows(lapply(1:n_trips, function(trip) {
      df <- simulate_behavioral_trip(points_per_trip, hub_lat, hub_lon, rho_explore = 0.9, rho_forage = 0.3, rho_return = 0.8)
      df$TagID <- a
      df$trip <- trip
      df
    }))
  })) %>%
    group_by(TagID) %>%
    mutate(DateTime = seq.POSIXt(from = as.POSIXct("2025-01-01 00:00"),
                                 by = dt, length.out = n()))

  fake_data <- fake_data %>%
    group_by(TagID, trip) %>%
    arrange(TagID, DateTime) %>%
    mutate(
      dist_m = geosphere::distHaversine(
        cbind(lag(longitude, default = first(longitude)), lag(latitude, default = first(latitude))),
        cbind(longitude, latitude)
      ),
      dt_s = as.numeric(difftime(DateTime, lag(DateTime, default = first(DateTime)), units = "secs")),
      speed_m_s = ifelse(dt_s > 0, dist_m / dt_s, 0),
      pdop = round(pmin(9, pmax(1, rbeta(n(), 2, 5)*8 + 1)), 1),
      nsats = round(pmin(12, pmax(3, rbeta(n(), 5, 3)*9 + 3)))

    )

  fake_data$speed_basic <- ifelse(fake_data$speed_m_s < 300, 0, 1)

  fake_data <- Track(fake_data)
  return(fake_data)

}




