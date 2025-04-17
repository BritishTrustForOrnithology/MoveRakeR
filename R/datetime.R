#' Wrapper to convert date-times to \code{POSIXct} format
#'
#' @description
#' Wraps the \code{base::as.POSIXct} function to enable easy character transformation
#' for routinely-used formats.
#'
#' @details
#' Often it is a task to transform character vector DateTimes to POSIX format for
#' proper manipulation. This function gives a very basic wrap of this to allow automatic
#' detection of the format provided for SOME formats, or allow the user to specify,
#' and to be applied of listed Track-style objects. Acceptable "automatic" detections
#' are only provided for: "YYYY-mm-dd", "yy-mm-dd", "dd/mm/YYYY", "dd/mm/yy" dates and
#' "HH:MM:SS", "HH:MM" times in all combinations.
#'
#' @param DateTime The single or vector  of character DateTimes to be converted.
#' @param format If auto is FALSE, then the format argument is used to provide a specific
#' format for \code{as.POSIXct} to accept, defaulting to "%d/%m/%Y %H:%M:%S".
#' @param tz Similar to format, if auto is FALSE, then the time zone argument for
#' \code{as.POSIXct} can be provided, defaulting to "UTC".
#' @param auto Logical for whether autamatic detection of DateTimes are to be used, or if
#' specific format and tz arguments are to be used instead, defaults to TRUE.
#'
#' @return
#' A converted vector of DateTimes in \code{POSIXct} format. NA will be returned if there
#' is a mismatch in the character vector and the required DateTime format conversion.
#'
#' @export
datetime <- function(DateTime, auto = TRUE, format = "%d/%m/%Y %H:%M:%S", tz = "UTC", na.omit = TRUE){

  # WARNING auto = TRUE is set up for European time format standards with
  # assumption that year month day or day month year as convention NOT
  # day year month. If the latter is the format then set auto to FALSE and provide
  # own format argument. ALSO, this assumes only ONE set of time-formats for the
  # DateTime argument - mix and matches are not supported.

  if(na.omit == TRUE){DateTime <- na.omit(DateTime)}

  # first DateTime
  DateTime1 <- na.omit(DateTime)[1]

  numbers_only <- function(x){
    suppressWarnings(!is.na(as.numeric(as.character(x))))
  }


  if(auto){

    # 12/04/.....
    if(substr(DateTime1,3,3) == "/"){
      # 12/04/2020 .....
      if(numbers_only(substr(DateTime1,7,10))){
        # 12/04/2020 14:27:08
        if(nchar(DateTime1) == 19){
          DateTime <- as.POSIXct(DateTime,format="%d/%m/%Y %H:%M:%S", tz=tz)
        }
        # 12/04/2020 14:27
        if(nchar(DateTime1) == 16){
          DateTime <- as.POSIXct(DateTime,format="%d/%m/%Y %H:%M", tz=tz)
        }

      }
      # 12/04/20 .....
      if(!numbers_only(substr(DateTime1,7,10))){
        # 12/04/20 14:27:08
        if(nchar(DateTime1) == 17){
          DateTime <- as.POSIXct(DateTime,format="%d/%m/%y %H:%M:%S", tz=tz)
        }
        # 12/04/20 14:27
        if(nchar(DateTime1) == 14){
          DateTime <- as.POSIXct(DateTime,format="%d/%m/%y %H:%M", tz=tz)
        }
      }

    }

    # 2020-04-12 ....
    if(substr(DateTime1,5,5) == "-"){
      # 2020-04-12 14:27:08
      if(nchar(DateTime1) == 19){
        DateTime <- as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S", tz=tz)
      }
      # 2020-04-12 14:27
      if(nchar(DateTime1) == 14){
        DateTime <- as.POSIXct(DateTime,format="%Y-%m-%d %H:%M", tz=tz)
      }
    }

    # 20-04-12 ....
    if(substr(DateTime1,3,3) == "-"){
      # 20-04-12 14:27:08
      if(nchar(DateTime1) == 17){
        DateTime <- as.POSIXct(DateTime,format="%y-%m-%d %H:%M:%S", tz=tz)
      }
      # 20-04-12 14:27
      if(nchar(DateTime1) == 14){
        DateTime <- as.POSIXct(DateTime,format="%y-%m-%d %H:%M", tz=tz)
      }
    }

  }

  if(auto){

    DateTime <- as.POSIXct(DateTime,format=format, tz=tz)

  }

  return(DateTime)


}
