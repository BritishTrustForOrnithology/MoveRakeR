#' Flexible progress bars with estimate time
#'
#' \code{progress_estimated2} is a replacement for dplyr::progress_estimated that was deprecated
#' in v 1.0.0 and inspired by that approach.
#' This is a simple light-weight progress bar that prints a single line progress
#' indicator with estimated remaining time. \code{progress_estimated2} also works well with nested loops and
#' optionally, a label can be used to indicate the current time ETA with print of the levels
#' from within loops.
#'
#' @param total_steps Integer. Total number of steps for the progress bar.
#'
#' @return A list with a \code{tick} function. Call \code{pb$tick(n = 1, label = NULL)}
#'   to advance the progress bar by \code{n} steps and optionally display a label.
#'
#' @examples
#' # Define a progress bar for a nested loop
#' tag_ids <- 1:3
#' vars_tag <- c("speed", "turn")
#' n_boot <- 5
#' gapsecs <- c(1,2)
#' seg_starts <- 1:4
#'
#' total_steps <- length(tag_ids) * length(vars_tag) * n_boot *
#'                length(gapsecs) * length(seg_starts)
#' pb <- progress_estimated(total_steps)
#'
#' for(ti in seq_along(tag_ids)){
#'   for(vi in seq_along(vars_tag)){
#'     for(b in seq_len(n_boot)){
#'       for(gap in gapsecs){
#'         for(i in seq_along(seg_starts)){
#'           Sys.sleep(0.05)  # simulate work
#'           pb$tick(label = sprintf("Animal %d / Var %s / Boot %d", tag_ids[ti], vars_tag[vi], b))
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' # Or tick without a label
#' for(ti in seq_along(tag_ids)){
#'   for(vi in seq_along(vars_tag)){
#'     for(b in seq_len(n_boot)){
#'       for(gap in gapsecs){
#'         for(i in seq_along(seg_starts)){
#'           Sys.sleep(0.05)  # simulate work
#'           pb$tick()
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' @export
progress_estimated2 <- function(total_steps){

  # Helper to format ETA nicely
  format_eta <- function(seconds){
    if(seconds < 60){
      sprintf("%ds", seconds)
    } else if(seconds < 3600){
      sprintf("%dm %02ds", seconds %/% 60, seconds %% 60)
    } else if(seconds < 86400){
      sprintf("%dh %02dm", seconds %/% 3600, (seconds %% 3600) %/% 60)
    } else{
      sprintf("%dd %02dh", seconds %/% 86400, (seconds %% 86400) %/% 3600)
    }
  }

  pb <- list(
    total = total_steps,
    current = 0,
    start_time = Sys.time()
  )

  pb$tick <- function(n = 1, label = NULL){
    pb$current <<- pb$current + n
    pct <- pb$current / pb$total

    # ETA calculation
    elapsed <- as.numeric(Sys.time() - pb$start_time, units = "secs")
    remaining <- if(pb$current > 0) round(elapsed / pb$current * (pb$total - pb$current)) else 0
    eta_text <- format_eta(remaining)

    # Determine bar width dynamically based on terminal width
    term_width <- getOption("width", 80)
    reserved <- nchar(sprintf(" | 100%% ~%s remaining", eta_text))
    if(!is.null(label)) reserved <- reserved + nchar(paste0(" | ", label))
    bar_width <- max(term_width - reserved, 10)

    filled <- min(round(pct * bar_width), bar_width)
    empty_width <- bar_width - filled
    bar <- paste0(rep("=", filled), collapse = "")
    empty <- paste0(rep(" ", empty_width), collapse = "")

    # Build the full progress line
    line <- sprintf("|%s| %3.0f%% ~%s remaining", paste0(bar, empty), pct*100, eta_text)
    if(!is.null(label)) line <- paste0(line, " | ", label)

    cat("\r", line, sep = "")
    flush.console()

    if(pb$current >= pb$total) cat("\n")

    invisible(pb)
  }

  return(pb)
}




