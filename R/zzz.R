.onLoad <- function(libname, pkgname) {
  reset_options_mR() # Run this so that get_options is available always on package load

  ### load sp option
  options("sp_evolution_status"=2)

  #packageStartupMessage(cat(paste0("\033[0;", 36, "m","-------------------","\n",
  #                      "Welome to MoveRakeR! Be sure to browseVignettes()","\n",
  #                      "and check out ?MoveRakeR for a full list of functions. Enjoy!", "\n",
  #                      "-------------------", "\033[0m", "\n")))
}


.onAttach <- function(libname, pkgname) {

  ### load sp option
  options("sp_evolution_status"=2)

  reset_options_mR() # Run this so that get_options is available always on package attach
}

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".", "..cols", "ActiveMon", "Beh", "Col", "DateTime", "DateTime_1", "DateTime_Track",
  "DateTime_orig", "DistCoastMax_off", "DistCoastMax_on", "DistMax", "DistMax_off",
  "DistMax_on", "DistTotal", "DistalPointOff", "Dur", "End", "G1", "G2", "HDOP", "N",
  "NA_latlong", "NA_sats", "PDOP", "Prop", "Sec", "Sec2", "Sec_no_1", "Sec_no_2",
  "Sec_no_2_pairbefore", "Start", "TagID", "TagID_num", "TotalMon", "TripDur", "Type",
  "Value", "X", "X1", "X1X2", "X2", "XX", "Y", "Y1", "Y2", "YY",
  "algorithm.marked.outlier", "all_anim", "altitude", "altitude_agl", "angle_rm",
  "as.formula", "battery_charging_current", "boot", "burst_size", "cc", "check",
  "col_lat", "col_lon", "colorRampPalette", "combined_rm", "combn", "cond0", "d",
  "date_end", "date_start", "db", "direction", "dist", "dist_m", "dt", "dt_", "dt_gap",
  "dt_s", "dtt", "dupDT_flag", "en", "end", "extra_row", "first_col_row", "future_flag",
  "gap", "gap0", "gap_num", "gapsec", "gps.hdop", "gps.pdop", "gps_fixtime", "gps_hdop",
  "gps_pdop", "h_accuracy", "head", "hist", "incompl", "is", "lat_jit", "latitude",
  "legend", "likely_rate", "login", "long_jit", "longitude", "lower_h_w", "lower_ham",
  "lower_hinge", "lower_q", "lower_q_w", "lower_whisker", "lr_", "mad", "max_col_row",
  "max_d", "max_lat", "max_lat_d2c", "max_lon", "max_lon_d2c", "max_val", "maxx_bad",
  "maxy_bad", "median", "mid", "min_d", "min_val", "minx_bad", "miny_bad",
  "multTagIDcomb", "n_active", "n_lower_h_w", "n_lower_q_w", "n_out", "n_out_b_lower",
  "n_out_b_upper", "n_out_h", "n_out_h_lower", "n_out_h_upper", "n_out_ia", "n_out_lower",
  "n_out_lower_q", "n_out_q", "n_out_q_lower", "n_out_q_upper", "n_out_upper", "n_u_max_w",
  "n_upper_h_w", "n_upper_q_w", "na.omit", "nn", "onshore", "outlier", "overlap_others",
  "pair", "pair1", "par", "period", "pnum", "present", "pressure", "pressure_avg",
  "prop_h", "prop_n", "prop_other_birds_active", "prop_q", "quantile", "rate_", "rate",
  "rbeta", "real", "real_max_rate", "rn", "rn2", "rn_", "rnorm", "runif", "satellites_used",
  "sats_lt_n", "sd", "sec_id", "second_max_val", "segments", "setNames", "sf_col", "sf_dat",
  "speed_2d", "speed_3d", "speed_accuracy", "st", "start", "stend", "sunrise", "sunset",
  "tag_voltage", "tail", "temp_avg", "temperature", "temporal_flag", "top_lev", "traj.speed",
  "traj_speed_diff", "traj_speed_jit", "tripNo", "trip_check", "txt", "u_max_w",
  "upper_h_w", "upper_ham", "upper_hinge", "upper_q", "upper_q_w", "upper_whisker",
  "v_accuracy", "var", "w_out_b_lower", "w_out_b_upper", "w_out_h_lower", "w_out_h_upper",
  "w_out_q_lower", "w_out_q_upper", "which_max", "which_second_max", "x1", "x11",
  "x2", "x_factor", "x_speed", "y", "y1", "y2", "y_speed", "z_speed"
))
