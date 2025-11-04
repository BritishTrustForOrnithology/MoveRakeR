# ---- Fake/demo data ----

if (!exists("userData")) {
  userData <- NULL
}
fake_data <- sim_data(n_trips = 10, points_per_trip = 50, animals = c("A001", "B002", "C003"),
                      dt = 300, hub_lat = 40, hub_lon = 0)
fake_data$speed_basic <- ifelse(fake_data$speed_m_s < 300, 0, 1)


