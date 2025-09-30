#' Add parameters from R package \code{suncalc} to data
#'
#' @description
#' A wrapper calling R package \code{suncalc} to add needed parameters of sun position
#' sunlight phase, moon positions and lunar phases.
#'
#' @details
#' Although the process of using suncalc is super easy and can be done by the user as they wish, if you are after
#' swift easy calculation of simple (e.g. sunrise, sunset) values then this function
#' just does it for you, rather than applying afresh each time with user-specific code.
#'
#' @param data A \code{Track} family, \code{tbl}, or \code{Track2} object.
#' @param ... further arguments passed to or from other methods.


#' @rdname sun_calc
#' @param keep Corresponding parts of the returned \code{suncalc} outputs to retain; see
#' the \code{suncalc} help files for more information; specified as a concatenation of character vectors
#' defaults to c("sunrise","sunset"). Note, this "keep" is for sub-setting of the Track
#' object and by default all variables are computed in \code{suncalc} to estimate
#' finest level categorisation of points by twilight grouping. Options of "all"
#' can also retain all the variables from \code{suncalc} and "none" drops all, after
#' categorisation of fixes.
#'
#' @return
#' The same Track-style dataset with additional columns retained from the specification of the
#' keep argument. A further column of \strong{"dnt"} is included with levels of
#' - DA, full daylight
#' - NI, full night
#' - AT, Astronomical twilight
#' - NT, Nautical twilight
#' - CT, Civil twilight
#' - SR, Sunrise (between sun appearing on horizon and to fully above)
#' - SS, Sunset, (between sun first touching horizon to fully below)
#' - suffixes of "m" and "e" are added to "AT", "NT" and "CT" denoting the side of the day, "morning" or "evening"
#' - \emph{Note}: Astronomical twilight often ends up as NA, meaning that points after nautical
#' twilight will be placed in the "NI" night category, rightly or wrongly! There will likely be
#' quirks with this. However, of course the user can return the columns and make bespoke
#' calculations if need be retaining columns from \code{suncalc} using the "keep" argument.
#'
#' @references
#' Thieurmel, B. & Elmarhraoui, A. (2019). suncalc: Compute Sun Position, Sunlight Phases, Moon Position and Lunar Phase. R package version 0.5.0. \url{https://CRAN.R-project.org/package=suncalc}
#'
#' @examples
#'
#' # this should be installed already on load in of MoveRakeR:
#' #install.packages("suncalc")
#' #library(suncalc)
#'
#' data <- sun_calc(data, keep = c("sunrise", "sunset"))
#' table(data$dnt)
#'
#' @export
sun_calc <- function(data, keep = c("sunrise", "sunset")){

  # remove any existing column names from previous run
  nml <- c("sunrise","sunset","solarNoon","nadir","sunriseEnd","sunsetStart","dawn","dusk","nauticalDawn","nauticalDusk","nightEnd","night","goldenHourEnd","goldenHour")

  data <- data[!names(data) %in% nml]

  dat <- data.frame(date = lubridate::date(data$DateTime),
                    lat = data$latitude,
                    lon = data$longitude)

  getem <- suncalc::getSunlightTimes(data = dat)

  # work out all classifications
  data <- Track(cbind(data,getem[!names(getem) %in% c("date","lat","lon")]))

  # astronomical twilight; if night and nightEnd are not present, you can't
  # estimate this, so has to go as "night"
  # Astronomical twilight = nightEnd to nauticalDawn and nauticalDusk to night
  # nautical twilight = nautical dawn-dawn and dusk to nautical dusk
  # civil twilight = dawn-sunrise and sunset to dusk

  if(exists("dnt", data)){
    data$'_dnt_' <- data$dnt
  }
  data$dnt <-
    ifelse(!is.na(data$nightEnd) & !is.na(data$nauticalDawn) & data$DateTime >= data$nightEnd & data$DateTime < data$nauticalDawn, "ATm",
           ifelse(!is.na(data$nauticalDawn) & !is.na(data$dawn) & data$DateTime >= data$nauticalDawn & data$DateTime < data$dawn, "NTm",
                  ifelse(data$DateTime >= data$dawn & data$DateTime <= data$sunrise, "CTm",
                         ifelse(data$DateTime > data$sunrise & data$DateTime < data$sunriseEnd, "SR",
                                ifelse(data$DateTime >= data$sunriseEnd & data$DateTime <= data$sunsetStart, "DA",
                                       # THe other side
                                       ifelse(data$DateTime > data$sunsetStart & data$DateTime < data$sunset, "SS",
                                              ifelse(data$DateTime >= data$sunset & data$DateTime < data$dusk, "CTe",
                                                     ifelse(!is.na(data$dusk) & !is.na(data$nauticalDusk) & data$DateTime >= data$dusk & data$DateTime < data$nauticalDusk, "NTe",
                                                            ifelse(!is.na(data$nauticalDusk) & !is.na(data$night) & data$DateTime >= data$nauticalDusk & data$DateTime < data$night, "ATe",
                                                                   ifelse(!is.na(data$night) & data$DateTime >= data$night, "NI",
                                                                          ifelse(!is.na(data$nightEnd) & data$DateTime < data$nightEnd, "NI",
                                                                                 NA)))))))))))

  # I think NAs are inserted when it's not feasible to estimate Astron Twil so add "NI" to replace the NAs.....I think.....kindof mindboggling
  data$dnt <- ifelse(is.na(data$dnt),"NI",data$dnt)

  if(!"all" %in% keep){
    data <- subset(data,select = names(data)[!names(data) %in% nml[!nml %in% keep]])
  }

  if(!"none" %in% keep){
    data <- subset(data,select = names(data)[!names(data) %in% nml])
  }

  return(data)

}



















