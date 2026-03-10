#' Summarise Track Data by Animal
#'
#' A function using the S3 `summary` method for `Track` objects to summarise GPS tracking data
#'
#' Summaries of GPS tracking data are generated for individual animals, with
#' a combined population-level row. The `summary` function supports simple summaries,
#' full summaries, and extended summaries including all numeric variables.
#' Gap summaries (time differences between consecutive fixes) are included.
#'
#' At very least the \code{summary} gives basic information on the start and end times available for each TagID, the
#' number of GPS fixes, and the x and y latitude and longitude WGS84 range of the data. Missing DateTimes are
#' counts per animal are given as n_na_DT, with further prop_na_lat and prop_na_lon for any NA lat long data.
#' This \code{summary} provides an easy indication as to whether data match
#' the user's expectation, such as upon read in from databases, or after manipulation stages,
#' e.g. checking the date ranges per animal are as expected if start and end times were specified in
#' \code{\link{read_track_MB}} and \code{\link{read_track_UvA}}.
#'
#' @rdname summary
#' @param data A data frame or tibble containing at least the columns:
#'   \code{TagID} (character/factor), \code{DateTime} (POSIXct),
#'   \code{latitude} and \code{longitude} (numeric). Extra numeric columns
#'   are optionally included in \code{full_plus} mode.
#' @param stat Character. "mean" or "median". Determines how central tendency
#'   is computed for numeric summaries.
#' @param dispersion Character. "MAD" or "IQR". Determines the dispersion measure
#'   used with median statistics.
#' @param digits Integer. Number of decimal places for formatted numeric output.
#' @param mode Character. One of "simple", "full", "full_plus".
#'   - "simple": basic start/end/n_fixes and bounding box.
#'   - "full": includes gaps, min/max, ± summaries for latitude/longitude.
#'   - "full_plus": adds all other numeric variables with ± summaries,
#'     and optionally ranges via \code{add_ranges}.
#' @param add_ranges Logical. If TRUE and \code{mode = "full_plus"}, adds
#'   min and max columns for all extra numeric variables.
#' @param gap_units Character. One of "seconds", "minutes" or "hours" controlling the returned
#' format of min_dt, max_dt, modal_dt and dt columns.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble of class \code{TrackSummary} with one row per TagID
#'   plus a combined "ALL" row. Columns include:
#'   \describe{
#'     \item{n_records, n_fixes, n_na_DT}{Counts of records and missing DateTime.}
#'     \item{start, end, dur}{Start, end, and duration of tracking period.}
#'     \item{gap, min_gap, max_gap, modal_gap}{Gap summaries in seconds.}
#'     \item{longitude, latitude}{± SD or ± MAD/IQR formatted summaries.}
#'     \item{xmin, xmax, ymin, ymax, area}{Spatial extents.}
#'     \item{prop_na_DT, prop_na_lat, prop_na_lon}{Proportion of missing values.}
#'     \item{Extra numeric columns}{Formatted ± summaries, and optionally min/max ranges.}
#'   }
#'   The gap column always appears before longitude and latitude.
#' @examples
#' \dontrun{
#' summary2(data, mode = "full_plus", add_ranges = TRUE, stat = "median", dispersion = "IQR")
#' }
#' @export
#' @method summary Track
summary.Track <- function(data,
                          stat = c("mean", "median"),
                          dispersion = c("MAD", "IQR"),
                          digits = 2,
                          mode = c("full", "simple", "full_plus"),
                          add_ranges = FALSE,
                          gap_units = c("seconds", "minutes", "hours"),
                          ...) {

  stat       <- match.arg(stat)
  dispersion <- match.arg(dispersion)
  mode       <- match.arg(mode)
  gap_units  <- match.arg(gap_units)

  # ------------------------------------------- #
  # Conversion factor for gap units
  # ------------------------------------------- #
  gap_factor <- switch(gap_units,
                       seconds = 1,
                       minutes = 60,
                       hours   = 3600)

  # ------------------------------------------- #
  # Internal summary formatter
  # ------------------------------------------- #
  .format_summary <- function(x, stat, dispersion, digits) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    if (stat == "mean") {
      sprintf(paste0("%.", digits, "f ± %.", digits, "f"),
              mean(x), sd(x))
    } else {
      disp <- if (dispersion == "MAD") mad(x) else IQR(x)
      sprintf(paste0("%.", digits, "f ± %.", digits, "f"),
              median(x), disp)
    }
  }

  # ------------------------------------------- #
  # Helper for modal gap
  # ------------------------------------------- #
  .modal_gap <- function(x, tol = 1){
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    x_round <- round(x / tol) * tol
    ux <- unique(x_round)
    ux[which.max(tabulate(match(x_round, ux)))]
  }

  # ------------------------------------------- #
  # Helper to format numeric columns globally
  # ------------------------------------------- #
  .format_numeric <- function(df, digits){
    num_cols <- vapply(df, is.numeric, logical(1))
    num_cols <- num_cols & !vapply(df, inherits, logical(1), what = c("POSIXt", "difftime"))
    df[num_cols] <- lapply(df[num_cols], function(x) {
      x_formatted <- x
      x_formatted[is.finite(x)] <- as.numeric(sprintf(paste0("%.", digits, "f"), x[is.finite(x)]))
      x_formatted
    })
    df
  }

  # ------------------------------------------- #
  # Safe min/max functions
  # ------------------------------------------- #
  safe_min <- function(x){
    x <- x[!is.na(x)]
    if(length(x)==0) return(NA_real_)
    min(x)
  }
  safe_max <- function(x){
    x <- x[!is.na(x)]
    if(length(x)==0) return(NA_real_)
    max(x)
  }

  # ------------------------------------------- #
  # SIMPLE MODE
  # ------------------------------------------- #
  if(mode == "simple"){
    per_animal <- data %>%
      dplyr::group_by(TagID) %>%
      dplyr::summarise(
        n_fixes = dplyr::n_distinct(DateTime),
        start = min(DateTime, na.rm = TRUE),
        end   = max(DateTime, na.rm = TRUE),
        xmin = min(longitude, na.rm = TRUE),
        xmax = max(longitude, na.rm = TRUE),
        ymin = min(latitude, na.rm = TRUE),
        ymax = max(latitude, na.rm = TRUE),
        .groups = "drop"
      )
    all_row <- per_animal %>%
      dplyr::summarise(
        TagID = "ALL",
        dplyr::across(-TagID, min, na.rm = TRUE)
      )
    out <- dplyr::bind_rows(per_animal, all_row)
    class(out) <- c("TrackSummary", class(out))
    attr(out, "gap_units") <- gap_units
    return(out)
  }

  # ------------------------------------------- #
  # Identify dynamic numeric cols
  # ------------------------------------------- #
  core_numeric <- c("latitude", "longitude")
  extra_numeric <- if(mode=="full_plus"){
    data %>% dplyr::select(where(is.numeric)) %>% dplyr::select(-any_of(core_numeric)) %>% names()
  } else character(0)

  # ------------------------------------------- #
  # PER-ANIMAL SUMMARY
  # ------------------------------------------- #
  per_animal <- data %>%
    dplyr::group_by(TagID) %>%
    dplyr::summarise(
      n_records = dplyr::n(),
      n_fixes   = dplyr::n_distinct(DateTime),
      n_na_DT   = sum(is.na(DateTime)),

      start = min(DateTime, na.rm = TRUE),
      end   = max(DateTime, na.rm = TRUE),
      dur   = end - start,

      xmin = safe_min(longitude),
      xmax = safe_max(longitude),
      ymin = safe_min(latitude),
      ymax = safe_max(latitude),
      area = (xmax - xmin) * (ymax - ymin),

      prop_na_DT  = sum(is.na(DateTime)) / n_records,
      prop_na_lat = sum(is.na(latitude)) / n_records,
      prop_na_lon = sum(is.na(longitude)) / n_records,

      gaps = list(diff(sort(DateTime[!is.na(DateTime)]))),
      min_gap = safe_min(unlist(gaps))/gap_factor,
      max_gap = safe_max(unlist(gaps))/gap_factor,
      modal_gap = .modal_gap(unlist(gaps))/gap_factor,

      longitude = .format_summary(longitude, stat, dispersion, digits),
      latitude  = .format_summary(latitude, stat, dispersion, digits),
      gap       = .format_summary(as.numeric(unlist(gaps))/gap_factor, stat, dispersion, digits),

      .groups = "drop"
    ) %>% dplyr::select(-gaps)

  # ------------------------------------------- #
  # EXTRA NUMERIC (full_plus)
  # ------------------------------------------- #
  if(length(extra_numeric)>0){
    extra <- data %>%
      dplyr::group_by(TagID) %>%
      dplyr::summarise(
        dplyr::across(all_of(extra_numeric), ~ .format_summary(.x, stat, dispersion, digits), .names="{.col}"),
        .groups="drop"
      )
    per_animal <- dplyr::left_join(per_animal, extra, by="TagID")
    if(add_ranges){
      ranges <- data %>%
        dplyr::group_by(TagID) %>%
        dplyr::summarise(
          dplyr::across(all_of(extra_numeric),
                        list(min = ~ safe_min(.x),
                             max = ~ safe_max(.x))),
          .groups="drop"
        )
      per_animal <- dplyr::left_join(per_animal, ranges, by="TagID")
    }
  }

  # ------------------------------------------- #
  # ALL ROW
  # ------------------------------------------- #
  all_gaps <- unlist(lapply(split(data$DateTime, data$TagID),
                            function(x) diff(sort(x[!is.na(x)]))))

  all_row <- data %>% dplyr::summarise(
    TagID = "ALL",
    n_records = n(),
    n_fixes = dplyr::n_distinct(DateTime),
    n_na_DT = sum(is.na(DateTime)),
    start = min(DateTime, na.rm = TRUE),
    end   = max(DateTime, na.rm = TRUE),
    dur   = end - start,
    xmin = safe_min(longitude),
    xmax = safe_max(longitude),
    ymin = safe_min(latitude),
    ymax = safe_max(latitude),
    area = (xmax - xmin)*(ymax - ymin),
    prop_na_DT  = mean(is.na(DateTime)),
    prop_na_lat = mean(is.na(latitude)),
    prop_na_lon = mean(is.na(longitude)),
    longitude = .format_summary(longitude, stat, dispersion, digits),
    latitude  = .format_summary(latitude, stat, dispersion, digits),
    gap       = .format_summary(as.numeric(all_gaps)/gap_factor, stat, dispersion, digits),
    modal_gap = .modal_gap(all_gaps)/gap_factor,
    min_gap   = safe_min(all_gaps)/gap_factor,
    max_gap   = safe_max(all_gaps)/gap_factor
  )

  if(length(extra_numeric)>0){
    extra_all <- data %>% dplyr::summarise(
      dplyr::across(all_of(extra_numeric), ~ .format_summary(.x, stat, dispersion, digits), .names="{.col}")
    )
    all_row <- dplyr::bind_cols(all_row, extra_all)
    if(add_ranges){
      extra_ranges_all <- data %>% dplyr::summarise(
        dplyr::across(all_of(extra_numeric), list(min = ~ safe_min(.x), max = ~ safe_max(.x)))
      )
      all_row <- dplyr::bind_cols(all_row, extra_ranges_all)
    }
  }

  # ------------------------------------------- #
  # Combine, format numeric, add units
  # ------------------------------------------- #
  out <- dplyr::bind_rows(per_animal, all_row)
  out <- .format_numeric(out, digits)
  class(out) <- c("TrackSummary", class(out))
  attr(out, "gap_units") <- gap_units

  # Move gap before longitude
  col_order <- names(out)
  if(all(c("gap","longitude","latitude") %in% col_order)){
    gap_idx <- which(col_order=="gap")
    lon_idx <- which(col_order=="longitude")
    col_order <- col_order[-gap_idx]
    col_order <- append(col_order, "gap", after=lon_idx-1)
    out <- out[, col_order]
  }


  out
}

#' @export
print.TrackSummary <- function(x, n = 10, ...){
  gap_units <- attr(x, "gap_units")
  if(is.null(gap_units)) gap_units <- "seconds"

  df <- as.data.frame(x) # ensure safe printing

  # Add units to gap columns
  for(col in c("gap","min_gap","max_gap","modal_gap")){
    if(col %in% names(df)){
      df[[col]] <- paste0(df[[col]], " (", gap_units, ")")
    }
  }

  cat("TrackSummary object:\n")
  cat(sprintf("Rows: %d, Columns: %d\n", nrow(df), ncol(df)))
  cat(sprintf("Gap units: %s\n\n", gap_units))
  print(utils::head(df, n = n), row.names = FALSE)
  if(nrow(df) > n) cat(sprintf("\n... %d more rows\n", nrow(df) - n))
  invisible(x)
}

