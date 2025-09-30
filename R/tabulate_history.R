#' Tabulated datetime summary history file of raw tracking data
#'
#' @description
#' A simple presentation of start/end of data spans, and number of fixes for Track objects.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param by Additional vector of characters referring to other levels to evaluate a summary by.
#' If "year" is chosen and year is not in the data, then this is added via \code{lubridate::year()}.
#' @return A \code{tbl} of TagID, start, end and number of fixes, duration (end minus start) and duration of monitoring,
#' using gapsections as determined through the user's selection earlier in the process e.g. \code{clean_GPS}. This
#' function is also used in \code{plot_leaflet_trips} for one of the tabs in the Shiny app.
#'
#' @examples
#'
#' # summarise the data
#' data_summary <- tabulate_history(data) # ignores any by variable supplied and evaluates by TagID
#'
#' tabulate_history2(data, by = c("year")) # by year - if year not found, will add in column using lubridate::year()
#'
#' tabulate_history2(data, by = c("year", "gapsec")) # if wanting any number of further levels, note gapsec also used to evaluate "Dur_days_mon" for true monitoring between gaps in the data
#'
#' @export
tabulate_history <- function(data, by = NULL){

  # active monitoring, as assessed through gapsections
  if(!exists("gapsec",data)){
    warning("Needs gapsec column from clean_GPS or gap_section functions (active monitoring), assuming no gaps in the record")
    data$gapsec <- 1 # i.e. will just be the same as Dur_days

  }

  if(!is.null(by)){
    if("year" %in% by){
      if(!exists("year",data)){
        data$year <- lubridate::year(data$DateTime)
      }
    }
  }

  sum1 <- tibble(data) %>% group_by(TagID, !!!syms(by)) %>% # hoping bang-bang-bang is ok in packages!? tried .data but seemed workable with one level...
    summarise(Start = min(DateTime),
              End = max(DateTime),
              Dur_days = as.vector(difftime(End, Start, units = 'days')),
              nfixes = length(DateTime), .groups = "keep"
              )

  by2 = unique(c(by, "gapsec"))

  sum2 <- tibble(data) %>% group_by(TagID, !!!syms(by2)) %>%
      summarise(Start = min(DateTime),
                End = max(DateTime),
                Dur = as.vector(difftime(End, Start, units = 'days')), .groups = "keep") %>%
      group_by(TagID, !!!syms(by))  %>%
      summarise(
        Dur_days_mon = sum(Dur), .groups = "keep"
      )

  sum3 <- left_join(sum1, sum2, by = c("TagID",by)) # needs the quotes for the by arg

  if(is.null(attr(sum3, "tabulate_history") )){
    attr(sum3, "tabulate_history") <- "tabulate_history"
  }

  # sub_attributes for arguments
  attr(attr(sum3, "tabulate_history"), "by") <- by

  return(sum3)

}
