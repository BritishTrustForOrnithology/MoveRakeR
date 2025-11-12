#' Provides a deeper dive into likely tag sampling rates
#'
#' A deluxe version of \code{assign_rates} that provide richer summaries and visual representation
#'
#' @details
#' The function \code{assign_rates2} is a wrapper function for \code{assign_rates} that provides further
#' assessment of tag sampling rate summaries. #' One of the main reasons for developing this \code{assign_rates2} wrapper was to provide more detailed
#' messaging back to the user as to the features of the data regarding likely sampling rates.
#' In that regard, \code{assign_rates2} assesses which of the rates (supplied as
#' breaks in the data) are most represented across animals, and if some animals were on different rates
#' the function summarises which animals had >x% (\emph{pctn_rate_thresh}) at those coarser rates. The findings
#' are given as a messaged print in the R console. Similarly, other features of the data are described, including
#' had either of their top two rates greater than \emph{pctn_rate_thresh} at likely coarser rates.
#'
#' Further, a quick assessment is made as to whether time steps between GPS fixes were detected beyond the maximum
#' likely rate as given under the \emph{breaks} argument, and thus inform if gaps may be present. This
#' also flags up if any animals that had greatest percentage of fixes above the max rate. \code{assign_rates2}
#' does not annotate the data but provides summaries; \code{assign_rates} should still be used for annotating the
#' original data with likely expected GPS sampling rates. \code{assign_rates2} is called from \code{rake}
#' as part of assessment 'rake' through the potential sources of error.
#'
#' The \code{assign_rates} and \code{assign_rates2} functions are both less relevant evaluations if you already know the
#' sampling rate tags were set to, and when any temporal switches in rates for individual animals in settings occurred.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param by As in \code{assign_rates}: An optional by argument (default to NULL i.e. only assessing by TagID); can be entered as c(x,y,z)
#' for levels to be assessed in the data, e.g. by year (if year is selected and is not present in the data it
#' is automatically added); out="summary" reflects the by variable choice.
#' @param split As in \code{assign_rates}: logical defaulting to FALSE for whether a split tibble is returned by TagID and the other by variables.
#' @param group_after_gap As in \code{assign_rates}: logical defaulting to FALSE for whether to summarise over gapsections if supplied as a by variable.
#' @param breaks As in \code{assign_rates}: A vector of binned choices of potential rates: the default here is for 11 categories:
#' 10, 120, 300, 600, 900, 1200, 1800, 3600, 7200 and 10800s, and the upper value beyond,
#' labelled as <max dt. This function operates independent of and gapsecs that may have been
#' chosen in \code{gap_section}, i.e. dts are assessed consecutively regardless of gaps, unless
#' supplied specifically within the by argument.
#' @param tol As in \code{assign_rates}: A vector of tolerances that are added to the breaks argument to account for
#' imperfect GPS sampling rates in data obtained; this is set to: c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)).
#' The tol argument can also be a single vector that is then applied equally to all break categories.
#' @param out As in \code{assign_rates}: By default the function outputs a \code{summary} count of fixes per TagID binned to the the
#' breaks chosen, and will result in a split tibble df per animal with start and end being the
#' breaks between subsequent categories and likely_rate = breaks. However, if out is \code{NULL}, then
#' the function will return the tibble with 'rate' added as a column to the data supplied.
#' The maximum category split will always be the maximum dt in the data with likely rate <max per animal.
#' @param pctn_rate_thresh The percentage value threshold above which assessment for data collection at coarser
#' or finer sampling rates is presented in the messaged outputs, e.g. For animal(s) X and Y: Z% or more data was collected at coarser/finer rates of 'S' s.
#' @param max_show The maximum number of individual animals to show in the messaging print out.
#' @param version *** However, note no by options or split = TRUE are available for assign_rates2 if selecting the extended version option.
#' @param plot Boolean (defaults FALSE) whether an output graphic visually showing the proportional rate breakdown per animal
#' should be plotted on function completion. Note this graphic is stored in attributes attr(,"plot") of the extended function output regardless.
#' @param messages Whether printed console messages are to be returned to the user, a Boolean defaulting to TRUE.
#' @param msg_col A numeric value for the colour to be used in console graphics.
#' The colour of the print out in the console, depending on your taste and whether black or white backgrounds
#' are currently being used within R Studio. Defaults to 35 (pink).
#' @param verbose Logical argument if run-time messages are wanted.
#'
#' @returns
#' The outputs returned depend on the argument 'version', and are a list of two. The first list element
#' relates to the original \code{assign_rates} output, and the second relates to the 'extended' version
#' referring to further output from \code{assign_rates2}; this extended output is a list of three, and includes a fix rate count cross-
#' tabulation and percent amount of rates per animal, and an additional optional \code{ggplot2::geom_smooth} graphic
#' showing per animal the rates for a visual interpretation.
#'
#' @seealso [MoveRakeR::assign_rates], [MoveRakeR::clean_GPS], [MoveRakeR::rake]
#'
#' @examples
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' data_with_rates = assign_rates2(data,
#'   by = NULL,
#'   split = FALSE,
#'   group_after_gap = FALSE,
#'   breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800),
#'   tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)),
#'   out = "summary",
#'   # extra assign_rates2 things
#'   pctn_rate_thresh = 20,
#'   max_show = 3,
#'   version = "extended",
#'   plot = FALSE,
#'   messages = TRUE,
#'   msg_col = 35,
#'   verbose=FALSE)
#'
#' data_with_rates[[2]]
#' attr(data_with_rates[[2]],"plot") # the extended list slot [[2]] has the plot attribute, even if plot = FALSE
#'
#' # if the "original" version was selected, data_with_rates[[1]] would contain assign_rates original output.
#'
#' @export
assign_rates2 <- function(data, by = NULL,
                          split = FALSE,
                          group_after_gap = FALSE,
                          breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800),
                          tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)),
                          out = "summary",

                          # extra assign_rates2 things
                          pctn_rate_thresh = 20,
                          max_show = 3,
                          version = c("extended","original"),
                          plot = FALSE,
                          messages = TRUE,
                          msg_col = 35,
                          verbose = FALSE){

  #output <- output[1]
  rates0 <- NULL

  # --------------------------------------------------- #
  if("original" %in% version){

    if(verbose){
      message("Running the assign_rates() function with original argument options...")
    }

    if(messages){
      if(nrow(data) > 50000){
        cat(paste0("\033[",msg_col,"m",".... Note this can be a bit slower for large data ....","\033[0m\n"))
      }
    }
    # call the original assign_rates() function
    rates0 = assign_rates(data,
                         by = by,
                         split = split,
                         group_after_gap = group_after_gap,
                         breaks = breaks,
                         tol = tol, out = out)


  }

  # --------------------------------------------------- #
  if("extended" %in% version){

    if(verbose){
      message("Extended option; Running the assign_rates() function ...")
    }
    if(messages){

      cat(rep(" ",  getOption("width")), "\n", sep = "")
      cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
      rule_centre("Checking tag sampling rates", col = msg_col, char = "*")
      cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
      cat(rep(" ",  getOption("width")), "\n", sep = "")

      if(nrow(data) > 50000){
        cat(paste0("\033[",msg_col,"m",".... Note this can be a bit slower for large data ....","\033[0m\n"))
      }
      cat(rep(" ",  getOption("width")), "\n", sep = "")

    }


    # note no by options (all at TagID level) or split = TRUE are available for assign_rates2()
    rates = assign_rates(data,
                         by = NULL,
                         split = FALSE,
                         group_after_gap = group_after_gap,
                         breaks = breaks,
                         tol = tol, out = "summary")

    # pivot wider
    rates2 = rates %>% dplyr::select(c(-start,-end)) %>% tidyr::pivot_wider(names_from = TagID, values_from = N)
    mr = max(as.numeric(rates2[!grepl("<",rates2$likely_rate),]$likely_rate))
    rates3 <- rates2[grepl("<",rates2$likely_rate),]

    # get a single row of sample rates beyond what the user selected - if there is one
    df <- NULL
    if(nrow(rates3) > 0){
      df = tibble(likely_rate = paste0(mr,"+"), as.data.frame(t(apply(rates3[-1], 2, sum, na.rm = TRUE))))
      rates = rbind(rates2[!grepl("<",rates2$likely_rate),], df)
    } else{
      rates = rates2
      rates$likely_rate = as.character(rates$likely_rate)
    }

    # bird level proportions
    rates_norm <- rates %>% mutate(across(-1, ~ (.x / sum(.x, na.rm = TRUE)*100)))

    # overall proportions
    rates_all_anim <- rates %>%
      rowwise() %>%
      mutate(all_anim = sum(c_across(-1)))

    rates_norm_all <- rates_all_anim %>%
      select(likely_rate, all_anim) %>% ungroup() %>%
      mutate(across(-1, ~ (.x / sum(.x, na.rm = TRUE)*100)))

    ######
    rates_long <- rates %>%
      pivot_longer(
        cols = -1,
        names_to = "TagID",
        values_to = "Value"
      ) %>%
      group_by(TagID) %>%
      mutate(Prop = Value / sum(Value, na.rm = TRUE)) %>%
      ungroup()

    if(!is.null(df)){

      rates_long <- rates_long %>% # then reorder x-axis: numeric first, then "+" category at the end
        mutate(
          x_factor = factor(
            !!sym(names(rates)[1]),
            levels = c(sort(as.numeric(sub("\\+", "", rates$likely_rate[rates$likely_rate != paste0(mr,"+")]))), paste0(mr,"+"))
          )
        )

    } else{
      rates_long <- rates_long %>% # then reorder x-axis: numeric first, then "+" category at the end
        mutate(
          x_factor = factor(
            !!sym(names(rates)[1]),
            levels = sort(rates$likely_rate)
          )
        )
    }

    if(verbose){
      message("Generating plot graphics (to print or store for later) ...")
    }
    g1 <- ggplot(rates_long, aes(x = x_factor, y = Prop, group = TagID, fill = TagID)) +
      #geom_area(alpha = 0.3, position = "identity") +
      #geom_line(aes(color = Variable), size = 1) +
      geom_smooth(aes(color = TagID), se = FALSE, span = 0.3) +
      labs(x = "Likely rate (s)", y = "Proportion", fill = "TagID", color = "TagID") +
      theme_minimal()

    if(plot){ print(g1) }

    # then provide that back as an output
    # better look at a proportion and anything above that value, show the rates?

    if(messages){

      ### max and second max values per column
      max_vals <- rates_norm %>% cbind(., rates_norm_all[-1]) %>%
        reframe(across(-1, ~ sort(.x, decreasing = TRUE, na.last = NA)[1:2]))

      # and corresponding TagIDs for max and second max
      which_vals <- rates %>%
        reframe(across(-1, ~ rates[[1]][order(.x, decreasing = TRUE)[1:2]]))

      top_all = rates_all_anim %>% select(likely_rate, all_anim) %>% arrange(-all_anim) %>% .$likely_rate
      which_vals = which_vals %>% cbind(.,tibble(all = top_all[1:2]))

      # Combine into a tibble
      summary_tbl <- tibble(
        TagID = colnames(rates_all_anim)[-1],
        max_val = sapply(max_vals, `[`, 1),
        second_max_val = sapply(max_vals, `[`, 2),
        which_max = sapply(which_vals, `[`, 1),
        which_second_max = sapply(which_vals, `[`, 2)
      )

      # # # # # # Bug fix 12/11/2025 # # # # # # #

      # above a certain proportion?
      #pctn_rate_thresh = 20

      # reframe stopped working as anticiapted in most recent dplyr:
      #max_vals_fixed <- rates_norm %>% cbind(., rates_norm_all[-1]) %>%
      #  reframe(across(-1, ~ which(.x > pctn_rate_thresh)))
      #i = 2
      #max_vals_fixed2 <- max_vals_fixed
      #for(i in 1:nrow(max_vals_fixed)){
      #  max_vals_fixed2[i,] <- rates_norm$likely_rate[unlist(max_vals_fixed[i,],use.names=TRUE)]
      #}

      ###### NEW SECTION
      max_vals_fixed <- rates_norm %>% cbind(., rates_norm_all[-1])

      # For each column (except first), get indices above threshold
      idx_list <- max_vals_fixed %>%
        select(-1) %>%  # exclude first column
        map(~ {
          w <- which(.x > pctn_rate_thresh)
          if(length(w) == 0) w <- NA_integer_  # handle no matches
          w
        })

      # determine maximum number of rows needed
      max_len <- max(map_int(idx_list, length))

      # Expand each column to match max_len
      idx_expanded <- map_dfc(idx_list, ~ rep(.x, length.out = max_len))

      # Replace indices with the actual likely_rate values
      max_vals_fixed2 <- idx_expanded %>%
        mutate(
          across(
            everything(),  # replace values in all columns including indices
            ~ ifelse(
              is.na(.x),
              NA_real_,
              as.numeric(gsub("\\+", "", rates_norm$likely_rate[.x]))
            )
          )
        )

      # # # # # # End Bug fix 12/11/2025 # # # # # # #

      # get the minimum rates where 20% more data at rate
      # assess the coarsest rate

      # ------------------------------------------------------------------- #
      most_freq_all <- unname(summary_tbl[summary_tbl$TagID == "all_anim",]$which_max,)
      most_freq_all_val <- sprintf("%.2f",unname(summary_tbl[summary_tbl$TagID == "all_anim",]$max_val,))

      next_most_freq_all <- unname(summary_tbl[summary_tbl$TagID == "all_anim",]$which_second_max,)
      next_most_freq_all_val <- sprintf("%.2f",unname(summary_tbl[summary_tbl$TagID == "all_anim",]$second_max_val,))

      cat(paste0("\033[",msg_col,"m","Across animals, the top two sampling rates are:","\033[0m\n"))
      cat(paste0("\033[",msg_col,"m","1. ",most_freq_all," s"," (",most_freq_all_val,"% GPS fixes)\033[0m\n"))
      cat(paste0("\033[",msg_col,"m","2. ",next_most_freq_all," s"," (",next_most_freq_all_val,"% GPS fixes)\033[0m\n"))
      cat(rep(" ",  getOption("width")), "\n", sep = "")

      # between animal top two rates - not a good idea to summarise all the data for big data!

      i = 1
      y <- list()
      for(i in 1:ncol(max_vals_fixed2)){
        y[[i]] <- min((unique(max_vals_fixed2[,i])))
      }
      y <- unlist(y)

      anims_coarse = names(max_vals_fixed2)[y == max(y)]
      anims_coarse <- anims_coarse[!anims_coarse == "all_anim"]
      if(length(anims_coarse) > max_show) {
        anims_coarse <- c(anims_coarse[1:max_show], "...")
      }
      anims_coarse = paste(anims_coarse, collapse = ", ")

      anims_fine = names(max_vals_fixed2)[y == min(y)]
      anims_fine <- anims_fine[!anims_fine == "all_anim"]
      if(length(anims_fine) > max_show) {
        anims_fine <- c(anims_fine[1:max_show], "...")
      }
      anims_fine = paste(anims_fine, collapse = ", ")

      cat(paste0("\033[",msg_col,"m","For animal(s) ",anims_coarse,": ",pctn_rate_thresh,"% or more data was collected at coarser rates of ", max(y) ," s \033[0m\n"))
      cat(paste0("\033[",msg_col,"m","For animal(s) ",anims_fine,": ",pctn_rate_thresh,"% or more data was collected at finer rates of ", min(y) ," s \033[0m\n"))
      cat(rep(" ",  getOption("width")), "\n", sep = "")

      ############# Any other coarser rates in top two or > 'pctn_rate_thresh'%?

      # # # # # # Bug fix2 12/11/2025 # # # # # # #
      summary_tbl$which_max2 <- as.numeric(gsub("\\+", "", summary_tbl$which_max))
      summary_tbl$which_second_max2 <- as.numeric(gsub("\\+", "", summary_tbl$which_second_max2))


      fin_check_max = as.numeric(summary_tbl[summary_tbl$TagID != "all_anim",]$which_max2)
      fin_check_next_max = as.numeric(summary_tbl[summary_tbl$TagID != "all_anim",]$which_second_max2)


      # ----------------------------------------- #
      # Other rates?

      covered_so_far <- unique(c(as.numeric(most_freq_all), as.numeric(next_most_freq_all), max(y), min(y)))
      max_feq_missed <- tid_missed <- NULL

      df_1 <- df_2 <- NULL
      if(!all(fin_check_max %in% covered_so_far)) {
        max_feq_missed = fin_check_max[!fin_check_max %in%
                                         covered_so_far]
        tid_missed = paste(names(which(summary_tbl$which_max ==
                                         max_feq_missed)), collapse = ", ")
        df_1 <- tibble(tid_missed, max_feq_missed) %>% rename(TagID = tid_missed, which_max = max_feq_missed)

      }
      next_max_feq_missed <- tid_missed2 <- NULL
      if (!all(fin_check_next_max %in% covered_so_far)) {
        next_max_feq_missed = fin_check_next_max[!fin_check_next_max %in%
                                                   covered_so_far]
        tid_missed2 = paste(names(which(summary_tbl$which_second_max ==
                                          next_max_feq_missed)), collapse = ", ")
        df_2 <- tibble(tid_missed2, next_max_feq_missed) %>% rename(TagID = tid_missed2, which_max = next_max_feq_missed)

      }

      df_l <- unique(rbind(df_1,df_2))

      #rate_miss = unique(c(max_feq_missed, next_max_feq_missed))
      #tid_miss = unique(c(tid_missed, tid_missed2))
      #df_l <- tibble(tid_miss, rate_miss) %>% rename(TagID = tid_miss, which_max = rate_miss)

      if(!is.null(df_l)) {

        bds = summary_tbl[summary_tbl$TagID %in% tid_miss,]
        a = subset(bds, select = c("TagID", "max_val",
                                   "which_max2")) %>% rename(which_max = which_max2)
        b = subset(bds, select = c("TagID", "second_max_val",
                                   "which_second_max2")) %>% rename(max_val = second_max_val,
                                                                    which_max = which_second_max2)

        d = rbind(a, b)
        d = merge(d, df_l, by = c("TagID", "which_max"))
        cat(paste0("\033[", msg_col, "m", "Some animals also had either their top two rates or > ",
                   pctn_rate_thresh, "% fixes at likely coarser rates: ",
                   "\033[0m\n"))
        msg = d %>% group_by(TagID) %>% summarise(txt = paste0(which_max,
                                                               " s, ", sprintf("%.2f", max_val), " %") %>%
                                                    paste(collapse = "; "), .groups = "drop") %>%
          mutate(txt = paste0(TagID, " (", txt, ")")) %>%
          summarise(msg = paste(txt, collapse = ", ")) %>%
          pull(msg)
        cat(paste0("\033[", msg_col, "m - ", msg, "\033[0m\n"))
        cat(rep(" ", getOption("width")), "\n", sep = "")

        if (length(msg) > max_show) {
          msg <- c(msg[1:max_show], "...")
        }

      }

      # # # # # # End Bug fix2 12/11/2025 # # # # # # #


      ############ finally, if any fixes were above the max rate expected (i.e. gaps)
      # df = if not length zero will be the extra rates above the max rate specified

      if(nrow(df)>0){
        cat(paste0("\033[",msg_col,"m","Some time steps detected beyond the max likely rate (",mr," s), i.e. gaps?\033[0m\n"))

        bad_f = rowSums(df[-1])
        tot_f = sum(rates_all_anim$all_anim)

        cat(paste0("\033[",msg_col,"m - ",sprintf("%.2f",(bad_f / tot_f)*100),"% fixes above the max rate\033[0m\n"))

        # highest proportion for:
        high = names(which.max(rates_norm[nrow(rates_norm),][-1]))
        w = names(rates_norm[nrow(rates_norm),][-1]) %in% high
        this = rates_norm[nrow(rates_norm),][-1][w]

        cat(paste0("\033[",msg_col,"m - Animal: ",names(this), " has the greatest percentage (",sprintf("%.2f",this),"%) fixes above the max rate\033[0m\n"))

      } else{
        cat(paste0("\033[",msg_col,"m - No fixes detected beyond the max rate specified\033[0m\n"))
      }

    }

    out <- vector("list", 2)
    names(out) <- c("rate_fix_count","rate_pcnt")

    #if(!is.null(xra)){
      out[[1]] <- rates_all_anim
    #}
    #if(!is.null(xra)){
      out[[2]] <- tibble(cbind(rates_norm,rates_norm_all[-1]))
    #}

    attr(out, "plot") <- g1


  }



  # --------------------------------------------------- #
  # final delivery
  out_fin <- vector("list", 2)
  names(out_fin) <- c("assign_rates","assign_rates2")

  if("original" %in% version){

    # the original output, with option to return the full annotated data of course,
    # everything you do in the original function
    # but note if summary = TRUE there will be duplication in this delivery method! Could be improved.

    out_fin[[1]] <- rates0


  }

  if("extended" %in% version){

    # add in the second position the 'out' elements from above extended option
    out_fin[[2]] <- out

  }

  return(out_fin)

}




