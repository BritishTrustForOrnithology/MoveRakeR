#' Simple outlier detection for covariates within telemetry data
#'
#' Assess the spread of data and existence of potential outliers for numeric/integer covariates alongside telemetry data
#'
#' @details
#' The \code{rake_outlie} function is a simple test for outliers within a chosen variable. This calculation is performed
#' for individual animals and then in relation to the total population. The resultant output therefore gives the user
#' an indication of outlier presence within individual in relation to the population.
#'
#' Outlier detection is not always straightforward an there are many approaches available, and can depend on
#' the distribution of your data, e.g. statistical tests can requires assumptions of normality, such as z-scores. I would recommend this
#' interesting \href{https://statsandr.com/blog/outliers-detection-in-r/#fn4}{vignette} (Soetewey 2020) that describes many of the available
#' methods and other R packages available, such as \code{outliers} (Komsta 2022), \code{DMwR::lofactor} (Torgo 2010),
#' \code{mvoutlier} (Filzmoser & Gschwandtner 2021), \code{odetector} (Cebeci et al. 2022), \code{Rlof} (Hu et al. 2022)
#' and probably many more.
#'
#' Outler detection is an important process within proposed workflows of
#' dealing with error in animal telemetry data (Fleming et al. 2020). Another R package that can help in this regard is
#' the \code{ctmm} package (Fleming & Calbrese 2020), specifically \code{ctmm:outlie} that specifically incorporates
#' GPS telemetry data error within its calculations.
#'
#' The \code{rake_outlie} function is concerned with the overall distribution of covariate data, which
#' may include data such as speed derived from telemetry processes, or some other independent covariate that may require investigation.
#' Following the review by Soetewey (2020) above, the \code{rake_outlie} implements three simple approaches, "quantile", "boxplot", and "Hampel".
#' Of course the use of outlier tests will depend on your underlying data, as tests will
#' typically detect 'some' data points as being outlying even for 'good' data.
#'
#' The 'quantile' approach used the quantile function using defined lower and upper percentiles of the distribution
#' that direct the function to label data points as outlying beyond each bound, e.g. lower 1% or upper 99%.
#' The 'boxplot' approach uses the boxplot function and the definitions of outliers that contains. The \code{rake_outlie}
#' function assumes outliers are those identified through the boxplot function, i.e. 1.5 x lower and upper quartiles.
#' The outliers for the Hampel filter are identified by the interval comprised of the median plus or minus three absolute
#' standard deviations and similarly to the other two methods, labelled as outlying beyond the Hampel range.
#'
#' The \code{rake_outlie} function also allows the user to specify an upper perceived threshold more generically through
#' the \emph{u_max} argument that directs the function to look at the proportion of data beyond that bound.
#'
#' A \emph{plot} argument can return the plot to the user, but this is accessible anyway after function run through the
#' attribute: \code{attr(, "plot_data")}. Here, the red dots are those data points above the \strong{population} upper bound
#' used (depending on the method specified), and any points above the individual animal upper bound are those assocaiated
#' with the individual animal's range.
#'
#' This leads on to how \code{rake_outlie} summarises the outlying information within and between animals, and was the original
#' motivation for construction of the function. It is useful to know whether the distribution of the individual animal's covariate data is inherently more or less
#' outlying to that of the overall population across animals. This is provided as above visually, but is further detailed
#' in the returned data summary. This summary (see Value section below), contains a basic summary of the proportion of data
#' that was outlying for the individual animal distribution and further columns of proportion of outliers of the individual animal's data
#' was in relation to the population-level distribution. The function can then further annotate the original data supplied
#' by the outlier detection for specific variable tested. The data summary, however is a separate process that can
#' be called for with annotate as well, in that case being an attribute of the returned annotated dataset: \code{attr(attr(., "rake_outlie"), "summary")}.
#'
#' Note at present only one variable can be supplied to the function at a time, and only one method per covariate can
#' be specified. We would stress again that care should be taken in the use of \code{rake_outlie} and that individual methods
#' ideally should be tested alongside others and full plotting and scrutiny of the data distibutions should be carried out.
#'
#' The \code{ShinyRakeR} interactive Shiny app can be used to flag potential outlying data points, but at present does not
#' incorporate the processes here in \code{rake_outlie}, and thus before running the app, \code{rake_outlie} would need to be run first and
#' potential outlying variables translated to a 1,0 binary format (i.e. yes or no for an outlier), so that variables can be
#' visible to the app head of time.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param var The name of the variable to perform outlier assessment on, defaults to 'dt' (time steps).
#' @param u_max The upper maximum perceived limit to the variable, beyond which an estimation of outlying points is made,
#' defaults to 300 (s) matching the default 'dt' var. Note this is a separate process to 'method'.
#' @param q For quantile and Hampel methods, the lower and upper percentile values to be used (vector of length two), for which data
#' is considered outlying beyond these bounds; defaults to c(0.01,0.99).
#' @param method One of three methods "quantile", "boxplot" or "Hampel".
#' @param plot TRUE/FALSE (default FALSE) whether a ggplot plot is to be printed to the user on function completion,
#' showing the median, quartile bounds, and 'outlying' data points, with red point being outlying from the upper or lower bound
#' of the population, black points being within the animal's distribution.
#' @param annotate TRUE/FALSE (defaulting to FALSE) whether to provide a fully annotated dataset, with outliers flagged.
#' @param summary TRUE/FALSE (defaulting to TRUE) whether to provide a summary data.frame of the proportion of outliers per animal and in relatio to the population.
#' @param verbose TRUE (default) or FALSE whether to provide within-function messaging.
#'
#' @seealso [MoveRakeR::rake], [MoveRakeR::ShinyRakeR]
#'
#' @return
#' This depends on the specification of the annotate and summary arguments; if summary = TRUE and annotate = FALSE,
#' then the function will return a data.frame for all animals, with outlier detection within the individual
#' for the variable chosen. The summary layout varies slightly depending on the method chosen. The format is as
#' follows (with brackets for columns specific to method):
#' - TagID = animal identifier, with an 'all' row across animals;
#' - n = the number of fixes for the animal;
#' - min_val (quantile, Hampel) = the minimum value of the covariate;
#' - lower_q (quantile) = the lower 25% quartile from the boxplot function;
#' - median (quantile, boxplot, Hampel) = the median value of the distribution;
#' - upper_q (quantile) = the upper 75% quartile from the boxplot function;
#' - lower_ham (Hampel) = the lower bound of the Hampel interval (median - 3.MAD)
#' - upper_ham (Hampel) = the upper bound of the Hampel interval (median + 3.MAD)
#' - lower_whisker (Boxplot) = q0.25 - 1.5*IQR
#' - lower_hinge (Boxplot) Tukey lower quartile
#' - upper_hinge (Boxplot) Tukey upper quartile
#' - upper_whisker (Boxplot) = = q0.75 + 1.5*IQR
#'
#' Columns regardless of method used:
#' - n_out = number of points beyond the lower and/or upper bounds per individual (or population for the 'all' data row).
#' The bounds taken are the lower_q:upper_q (quantile), the lower_whisker:upper_whisker (boxplot) or lower_ham:upper_ham (Hampel);
#' - prop_out = proportion of individual animal data points outside the bounds of the individual animal's data range bound (as above per method);
#' - n_out_ia = number of data point for the 'individual' that are outside the bods of the 'all' total population (i.e. 'ia');
#' - prop_out_ia = proportion of individual animal data points outside the bounds of the population;
#' - out_ratio = ration of n_out_ia/n_out, if this value is far above 1, then for that animal there are more fixes identified
#' outside the method-specific bounds than the bounds of the the all (population-level) data. Again, the function should be used
#' with caution as for so called 'good' data with no real outliers, the methods may differ in their summaries.
#'
#' For annotated data, up to three additional columns are added, one for annotation if the method flagged data points outlying
#' the individual (column name format: 'outlier_i_VARNAME_METHOD[1]'), if points for the individual were outlying the population
#' distribution (column name format: 'outlier_ia_VARNAME_METHOD[1]'), and if the u_max argument is
#' not NULL, whether fixes were flagged as outlying the upper bound ('u_max' column). For boxplot the individual column name
#' outlier_i_VARNAME_METHOD[1] is flagged as "i" (either upper or lower outlier), and for Hampel and quantile methods,
#' further distinction is made for "i_u" and "i_l" for upper and lower. For the population-level 'outlier_ia_VARNAME_METHOD[1]',
#' all methods have upper ('ia_u') and lower ('ia_l') distinction, and in all cases zeros are those wors not flagged.
#'
#' @references
#'
#' Cebeci, Z., Cebeci, C., Tahtali, Y. and Bayyurt, L. 2022. Two novel outlier detection
#' approaches based on unsupervised possibilistic and fuzzy clustering. PeerJ Computer Science 8:e1060.
#' \url{https://doi.org/10.7717/peerj-cs.1060}.
#'
#' Filzmoser P, Gschwandtner M (2021). mvoutlier: Multivariate Outlier Detection Based on Robust Methods.
#' R package version 2.1.1, \url{https://CRAN.R-project.org/package=mvoutlier}.
#'
#' Fleming, C.H., Drescher-Lehman, J.. Noonan, M.J., Akre, T.S.B., Brown, D.J., Cochrane, M.M., Dejid, N., DeNicola, V.,
#' DePerno, C.S., Dunlop, J.N., Gould, N.P., Harrison, A.-L., Hollins, J., Ishii, H., Kaneko, Y., Kays, R., Killen, S.S.,
#' Koeck, B., Lambertucci, S.A., LaPoint, S.D., Medici, E.P., Meyburg, B.-U., Miller, T.A., Moen, R.A., Mueller, T.,
#' Pfeiffer, T., Pike, K.N., Roulin, A., Safi, K., Séchaud, R., Scharf, A.K., Shephard, J.M., Stabach, J.A., Stein, K.,
#' Tonra, C.M., Yamazaki, K., Fagan, W.F. & Calabrese, J.M. (2020) A comprehensive framework for handling location error
#' in animal tracking data. bioRxiv 2020.06.12.130195; doi: \url{https://doi.org/10.1101/2020.06.12.130195}.
#'
#' Fleming C.H. & Calabrese J.M. (2025) ctmm: Continuous-Time Movement Modeling.
#' R package version 1.3.0. \url{https://CRAN.R-project.org/package=ctmm}.
#'
#' Hu Y, Murray W, Shan Y, Australia. (2022). Rlof: R Parallel Implementation of Local Outlier Factor(LOF).
#' R package version 1.1.3. \url{https://CRAN.R-project.org/package=Rlof}.
#'
#' Komsta L (2022). outliers: Tests for Outliers. R package version 0.15. \url{https://CRAN.R-project.org/package=outliers}.
#'
#' Soetewey, A. (2020) Outliers detection in R, Stats and R. \url{https://statsandr.com/blog/outliers-detection-in-r/#fn4}.
#'
#' Torgo, L. (2010). Data Mining with R, learning with case studies Chapman and Hall/CRC. \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}.
#'
#' @examples
#' # assessing outlier potential in the 'traj.speed' (trajectory speed) variable of data
#'
#' ### returning the summary data with no annotation:
#'
#' q <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = "quantile",
#'                        plot = FALSE, annotate = FALSE, summary = TRUE, verbose = FALSE)
#'
#' b <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = c("boxplot"),
#'                        plot = FALSE, annotate = FALSE, summary = TRUE, verbose = FALSE)
#'
#' h <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = c("Hampel"),
#'                        plot = FALSE, annotate = FALSE, summary = TRUE, verbose = FALSE)
#'
#' # plot data retained in the summary output as an attribute:
#' attr(q, "plot_data")
#'
#' ### annotated data (retaining summary)
#' q2 <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = "boxplot",
#'                        plot = FALSE, annotate = TRUE, summary = TRUE, verbose = FALSE)
#'
#' # accessing the summary data attribute
#' attr(attr(q2, "rake_outlie"), "summary") # equivalent to above with annotate = FALSE
#'
#' # access the plot regardless of plot argument
#' attr(attr(q2, "rake_outlie"), "plot_data")
#'
#' ### check:
#' b2 <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = c("boxplot"),
#'                        plot = FALSE, annotate = TRUE, summary = TRUE, verbose = FALSE)
#'
#' # length of outlier column labelled here (e.g. for a column called 'traj.speed' boxplot method in this example:
#' nrow(b2[b2$outlier_i_traj.speed_b == "i",]) # <---
#' # nrow(q2[q2$outlier_i_traj.speed_q %in% c("i_u", "i_l"),]) # or this for quantile example
#'
#' # should match sum here in summary excluding all population row:
#' a = attr(attr(b2,"rake_outlie"), "summary")
#' sum(a[a$TagID != "all",]$n_out) # <---
#'
#' ################# see also ###########
#' library(ctmm)
#' library(MoveRakeR)
#' data(buffalo)
#'
#' # Outliers in Cilla
#' OUT <- outlie(buffalo[[1]])
#'
#' # convert a Track object to move format for use in ctmm::outlie
#' data_2move <- MoveRakeR::Track2move(data)
#'
#' test_ctmm <- ctmm::as.telemetry(data_2move)  # convert to ctmm from move
#' OUT <- ctmm::outlie(test_ctmm, plot = FALSE)
#'
#' #####################
#' # another option is to use dbscan::lof() nearest neighbour clustering
#' #install.packages('dbscan')
#' library(dbscan)
#' install.packages("Rlof")
#' library(Rlof)
#'
#' # Assuming data has latitude and longitude columns
#' dat1 <- data[!is.na(data$longitude) & !is.na(data$latitude),]
#'
#' # LOF score: k = number of neighbors to consider
#' lof_scores <- lof(coords, minPts = 6)
#' lof_scores <- Rlof::lof(as.matrix(dat1[, c("longitude", "latitude")]), k = 5)
#' # Add LOF scores to your data
#' dat1$lof_score <- lof_scores
#' # Flag extreme outliers (say top 1% of LOF)
#' threshold <- quantile(lof_scores, 0.99) # still using a quantile though here
#' dat1$lof_outlier <- lof_scores > threshold
#'
#' #plot_leaflet(testdat[which(dat1$lof_outlier),])
#'
#' ##### speed up?
#' # Compute k nearest neighbors (say k = 5)
#' coords <- dat1[, c("longitude", "latitude")]
#' knn_res <- dbscan::kNN(coords, k = 5)
#'
#' # Compute LOF using precomputed kNN
#' lof_scores <- lof(coords, minPts = 6, search = knn_res)
#'
#' # Attach scores to data
#' dat1$lof_score <- lof_scores
#'
#' @export
rake_outlie <- function(data, var = "dt", u_max = 300, q = c(0.01,0.99),
                     method = c("quantile","boxplot","Hampel"),
                     plot = TRUE, annotate = TRUE, summary = TRUE, verbose = TRUE){

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  # ------------- setup up ------------- #
  method = method[1]

  if(verbose){message("--- Outlier estimation using ", method, " method")}


  if(!annotate & !summary){
    stop("Nothing for the function to do")
  }

  ## add row_numbers
  data <- tibble(data) %>% group_by(TagID) %>%  mutate(rn = row_number()) %>% ungroup()

  # add dt and traj_speed per bird - note this will not consider gaps at all

  if(var == "traj.speed" | var == "traj_speed" | var == "trajectory.speed" | var == "trajectory_speed" |
     var == "traj.velocity" | var == "traj_velocity" | var == "trajectory.velocity" | var == "trajectory_velocity"){

    if(!var %in% names(data)){
      if(verbose){
        message("Calculating ", var ," variable, as not found in data
        - Warning: this will calculate differences between timestamps over gaps
        - Use function gap_section() to consider gaps")
      }
      data <- data %>% group_by(TagID) %>%
        mutate(
          dt_ = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
          dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
          !!var := dist / dt_,
          !!var := lag(!!sym(var), default= 1)
        ) %>% select(-dt_, -dist) %>% ungroup()

    } else{
      if(verbose){message(var, " found in data, proceeding....")}
    }

  } else if(var == "dt"){

    if(!var %in% names(data)){
      if(verbose){
        message("Calculating ", var ," variable, as not found in data
        - Warning: this will calculate differences between timestamps over gaps
        - Use function gap_section() to consider gaps")
      }

      data <- data %>% group_by(TagID) %>%
        mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>% ungroup()

    } else{
      if(verbose){message(var, " found in data, proceeding....")}
    }
  } else{

    if(!var %in% names(data)){
      stop(message(var, " not found in data."))
    } else{
      if(verbose){message(var, " found in data, proceeding....")}
    }
  }

  # --------------------------------------- #
  # .rake_outlie internal function to find row numbers for outlying fixes and
  # count them, grouped by TagID or across all animals (grouper)

  .rake_outlie <- function(grouper = "TagID"){

    # summary for dt
    data_out = data %>%
      {if(!is.null(grouper)) group_by(., !!sym(grouper)) else data} %>%
      summarise(boxplot= list(
        setNames(boxplot.stats(!!sym(var))$stats, c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker'))
        #setNames(boxplot.stats(!!sym(var))$out, c("outlier"))
      )
      ) %>%
      tidyr::unnest_wider(boxplot) %>%
      {if(!is.null(grouper)) group_by(., !!sym(grouper)) else .}


    data_out3 = data %>%
      {if(!is.null(grouper)) group_by(., !!sym(grouper)) else{data}} %>%

      summarise(boxplot= list(
        {if(length(which(!!sym(var) %in% boxplot.stats(!!sym(var))$out)) > 0) # if there are NO outliers, then we need a way of capturing that
          unname(setNames(which(!!sym(var) %in% boxplot.stats(!!sym(var))$out), c("outlier")) ) # so this stores what values they are
          else
            NULL
        }
      )
      ) %>%
      mutate(n_out = purrr::map_int(boxplot, length)) %>%
      #left_join(.,data_out, by = "TagID")
      {if(!is.null(grouper)) left_join(.,data_out, by = "TagID")  else cross_join(.,data_out)}

    # see also the rstatix::identify_outliers function

    # referencing this useful resource: https://statsandr.com/blog/outliers-detection-in-r/#hampel-filter
    # with thanks to Antoine Soetewey
    # note here we are not doing statistical tests given potential is high here for data to be non-normally distributed...
    # user is suggested to read the above resource and other outlier detection texts to inform about the data
    # this function is a basic first 'rake' of the data
    data_len <- data %>%
      {if(!is.null(grouper)) group_by(., !!sym(grouper)) else data} %>%
      summarise(n = n(),
                # mins and maxes
                #min_val = min(!!sym(var), na.rm = TRUE),
                #max_val = max(!!sym(var), na.rm = TRUE),

                min_val = if (all(is.na(!!sym(var)))) NA_real_ else min(!!sym(var), na.rm = TRUE),
                max_val = if (all(is.na(!!sym(var)))) NA_real_ else max(!!sym(var), na.rm = TRUE),

                # lower and upper quartiles based on user selection, and which they are as IDs in data
                lower_q = quantile(!!sym(var), q[1], na.rm=TRUE),
                upper_q = quantile(!!sym(var), q[2], na.rm=TRUE),
                upper_q_w = list( which(!!sym(var) > upper_q)),
                lower_q_w = list( which(!!sym(var) < lower_q)),

                # lower and upper bounds based on the Hampel filter
                lower_ham = median( !!sym(var), na.rm=TRUE ) - 3 * mad( !!sym(var) , constant = 1, na.rm=TRUE),
                upper_ham = median( !!sym(var), na.rm=TRUE ) + 3 * mad( !!sym(var) , constant = 1, na.rm=TRUE),
                upper_h_w = list( which(!!sym(var) > upper_ham)),
                lower_h_w = list( which(!!sym(var) < lower_ham))
      ) %>%
      mutate(
        # summed number of quartile outliers based on user selection
        n_upper_q_w = purrr::map_int(upper_q_w, length),
        n_lower_q_w = purrr::map_int(lower_q_w, length),
        n_out_q = n_upper_q_w + n_lower_q_w,

        # summed number of outliers based on Hampel filter
        n_upper_h_w = purrr::map_int(upper_h_w, length),
        n_lower_h_w = purrr::map_int(lower_h_w, length),
        n_out_h = n_upper_h_w + n_lower_h_w

      ) %>%
      #select(c(-n_upper_q_w,-n_lower_q_w, -n_lower_h_w, -n_upper_h_w)) %>% # these would in fact be needed for the main total beyond the individual levels...
      {if(!is.null(grouper)) left_join(.,data_out3, by = "TagID")  else cross_join(.,data_out3)} %>%
      mutate(
        prop_n = n_out / n,
        prop_q = n_out_q / n,
        prop_h = n_out_h / n
      )

    # additional for max value supplied...
    if(!is.null(u_max)){
      data_len = data %>%
        {if(!is.null(grouper)) group_by(., !!sym(grouper)) else{data}} %>%
        summarise(
          # which are greater than upper u supplied
          u_max_w = list( which(!!sym(var) >= u_max))
        ) %>% mutate(
          n_u_max_w = purrr::map_int(u_max_w, length)
        ) %>%
        {if(!is.null(grouper)) left_join(.,data_len, by = "TagID")  else cross_join(.,data_len)}

    }

    return(data_len)

  }

  # ----------------------------------------- #
  if(verbose){message("--- calculating outliers ---")}
  rk_gp = .rake_outlie()
  rk = .rake_outlie(grouper = NULL)
  # ----------------------------------------- #

  rk$TagID <- "all"
  rk <- rk %>% relocate(TagID, .before = "n")
  rk_gp <- rbind(rk_gp, rk)
  rk_gp$ind_n_tot <- (rk_gp$n / rk$n) * 100 # proportion of individual contribution to total data

  # what we really want is what the upper bounds are in relation to the total...
  data_next <- data %>%
    group_by(TagID) %>%

    # this is TOO COMPLICATED - alupper_whisker# this is TOO COMPLICATED - all we really need are the fixes per bird that lie OUTSIDE the bounds of the ALL RANGE
    summarise(
      # quantiles
      w_out_q_upper = list(
        which(!!sym(var) > rk$upper_q) #### GREATER THAN UPPER
      ),
      w_out_q_lower = list(
        which(!!sym(var) < rk$lower_q) #### LOWER THAN LOWER
      ),

      # boxplot
      w_out_b_upper = list(
        which(!!sym(var) > rk$upper_whisker) #### GREATER THAN UPPER
      ),
      w_out_b_lower = list(
        which(!!sym(var) < rk$lower_whisker) #### LOWER THAN LOWER
      ),

      # Hampel
      w_out_h_upper = list(
        which(!!sym(var) > rk$upper_ham) #### GREATER THAN UPPER
      ),
      w_out_h_lower = list(
        which(!!sym(var) < rk$lower_ham) #### LOWER THAN LOWER
      )

    )  %>%
    mutate(

      n_out_q_upper = purrr::map_int(w_out_q_upper, length),
      n_out_q_lower = purrr::map_int(w_out_q_lower, length),

      n_out_b_upper = purrr::map_int(w_out_b_upper, length),
      n_out_b_lower = purrr::map_int(w_out_b_lower, length),

      n_out_h_upper = purrr::map_int(w_out_h_upper, length),
      n_out_h_lower = purrr::map_int(w_out_h_lower, length)

    ) %>%
    full_join(.,rk_gp, by = "TagID")

  # ------------------------------------------------------------------------------------ #
  # SUMMARY
  # ------------------------------------------------------------------------------------ #

  #str(data_next)

  if(verbose){message("--- preparing summary ---")}

  if(method == "boxplot"){
    # ------------------------ #
    # BOXPLOT
    # ------------------------ #

    # boxplot f
    .get_matrices0 <- function(nm = "boxplot"){
      data_b4 <- data %>%
        group_by(TagID) %>%
        mutate(rn = row_number()) %>%
        select(c(TagID, !!sym(var), rn)) %>%
        filter(rn %in%  boxp[boxp$TagID %in% cur_group(),][,which(names(boxp[boxp$TagID %in% cur_group(),]) %in% nm)][[1]][[1]])
      return(data_b4)
    }

    #boxp <- subset(data_next, select = c(TagID,lower_whisker,lower_hinge,median,upper_hinge,upper_whisker,boxplot,w_out_upper_b_g_u, w_out_lower_b_g_u,  w_out_upper_b_g_l, w_out_lower_b_g_l, w_out_lower_b_g_u_u, w_out_upper_b_g_l_l))
    boxp <- subset(data_next, select = c(TagID,lower_whisker,lower_hinge,median,upper_hinge,upper_whisker,boxplot, w_out_b_upper, w_out_b_lower))

    data_b <- .get_matrices0("boxplot")

    g1 <- ggplot(boxp, aes(x = TagID, ymin=lower_whisker, lower=lower_hinge, middle = median, upper = upper_hinge, ymax = upper_whisker)) +
      geom_boxplot(stat="identity") +
      geom_point(data = data_b, aes(y = dt, x = TagID),inherit.aes = FALSE) +
      coord_flip()

    data_b4 <- .get_matrices0("w_out_b_upper")
    data_b5 <- .get_matrices0("w_out_b_lower")
  }

  if(method == "quantile"){

    # ------------------------ #
    # QUANTILE
    # ------------------------ #
    # quartiles 0.01, 0.99 f
    .get_matrices <- function(nm = "w_out_upper_q_g_u"){
      data_b4 <- data %>%
        group_by(TagID) %>%
        mutate(rn = row_number()) %>%
        select(c(TagID, !!sym(var), rn)) %>%
        filter(rn %in%  boxp2[boxp2$TagID %in% cur_group(),][,which(names(boxp2[boxp2$TagID %in% cur_group(),]) %in% nm)][[1]][[1]])
      return(data_b4)
    }
    boxp2 <- subset(data_next, select = c(TagID,min_val,lower_q,median,upper_q,max_val,upper_q_w,lower_q_w, w_out_q_upper, w_out_q_lower))

    data_b2 <- .get_matrices("upper_q_w")
    data_b3 <- .get_matrices("lower_q_w")

    g1 <- ggplot(boxp2, aes(x = TagID, ymin=min_val, lower=lower_q, middle = median, upper = upper_q, ymax = max_val)) +
      geom_boxplot(stat="identity") +
      geom_point(data = data_b2, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE) +
      geom_point(data = data_b3, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE) +
      coord_flip()

    # how to handle the fixes individual in relation to total
    data_b4 <- .get_matrices("w_out_q_upper")
    data_b5 <- .get_matrices("w_out_q_lower")
  }

  if(method == "Hampel"){
    # ------------------------ #
    # HAMPEL
    # ------------------------ #
    .get_matrices2 <- function(nm = "w_out_upper_h_g_u"){
      data_b4 <- data %>%
        group_by(TagID) %>%
        mutate(rn = row_number()) %>%
        select(c(TagID, !!sym(var), rn)) %>%
        filter(rn %in%  boxp3[boxp3$TagID %in% cur_group(),][,which(names(boxp3[boxp3$TagID %in% cur_group(),]) %in% nm)][[1]][[1]])
      return(data_b4)
    }

    boxp3 <- subset(data_next, select = c(TagID,min_val,lower_ham ,median,upper_ham,max_val, upper_h_w,lower_h_w, w_out_h_upper, w_out_h_lower))

    data_b2 <- .get_matrices2("upper_h_w")
    data_b3 <- .get_matrices2("lower_h_w")

    g1 <- ggplot(boxp3, aes(x = TagID, ymin=min_val, lower=lower_ham, middle = median, upper = upper_ham, ymax = max_val)) +
      geom_boxplot(stat="identity") +
      geom_point(data = data_b2, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE) +
      geom_point(data = data_b3, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE) +
      coord_flip()

    # how to handle the fixes individual in relation to total

    data_b4 <- .get_matrices2("w_out_h_upper")
    data_b5 <- .get_matrices2("w_out_h_lower")

  }

  g2 <- g1 + geom_point(data = data_b4, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE, col = "red", size = 1) +
    geom_point(data = data_b5, aes(y = !!sym(var), x = TagID),inherit.aes = FALSE, col = "darkred", size = 1)

  #out_sum <- data.table(rbind(data_b4,data_b6))
  #out_sum <- rbind(data_b4,data_b6) %>% group_by(TagID) %>% summarise(n = n()) # why id drop=FALSE not working here?
  #m <- unique(data$TagID)[which(!unique(data$TagID) %in% out_sum$TagID)]
  #if(length(m) > 0){
  #  out_sum <- rbind(out_sum,tibble(TagID = m, n = 0))
  #}

  #str(data_next)

  # for final summary stating which birds have X data beyond the global etc for outlier fixes BEYOND the global all animal limits

  if(method == "boxplot"){
    dat <- subset(data_next, select = c(TagID,n,lower_whisker,lower_hinge,median,upper_hinge,upper_whisker,n_out,prop_n,n_out_b_upper, n_out_b_lower))  %>% rename(n_out_upper = n_out_b_upper, n_out_lower = n_out_b_lower, prop_out = prop_n)
  }
  if(method == "quantile"){
    dat <- subset(data_next, select = c(TagID,n,min_val,lower_q,median,upper_q,max_val,n_out_q,prop_q, n_out_q_upper, n_out_q_lower)) %>% rename(n_out_upper = n_out_q_upper, n_out_lower = n_out_q_lower, n_out = n_out_q, prop_out = prop_q)
  }
  if(method == "Hampel"){
    dat <- subset(data_next, select = c(TagID,n,min_val,lower_ham ,median,upper_ham,max_val,n_out_h,prop_h,n_out_h_upper, n_out_h_lower)) %>% rename(n_out_upper = n_out_h_upper, n_out_lower = n_out_h_lower, n_out = n_out_h, prop_out = prop_h)
  }

  # max_u

  #u_max_w n_u_max_w
  if(!is.null(u_max)){
    dat0 <- data_next %>% select(c(TagID,n_u_max_w)) %>% rename(n_u_max = n_u_max_w)
    dat <- cbind(dat,dat0[,2])
    dat$prop_u_max <- dat$n_u_max / dat$n
  }

  # this is the summary
  dat <- dat %>% mutate(n_out_ia = n_out_upper + n_out_lower,
                        prop_out_ia = n_out_ia / n,
                        out_ratio = n_out_ia/n_out) %>%
    select(c(-n_out_upper, -n_out_lower))
  dat$method = method
  dat$var = var

  attr(dat, "plot_data") <- g2 # always store the plot data in the summary

  # the above data_b2 to data_b4 contain the actual row numbers you can label fixes as

  # FULL JOIN, bad use of memory
  #data_b4 %>% select(-dt) %>% mutate(w_out_h_upper = 1) %>% full_join(., data, by = "TagID")

  # select the data from the original data via subset and rn
  # do a with and without, and then rbind

  # -------------------------------------------- #
  # PLOT OUT
  # -------------------------------------------- #

  if(plot){g2}

  # -------------------------------------------- #
  # ANNOTATE
  # -------------------------------------------- #

  if(annotate){

    if(verbose){message("--- annotating data for chosen outlier criteria (", method, ") ---")}

    if(method %in% c("Hampel", "quantile")){
      w_out_upper = data_b2 %>% select(-!!sym(var)) %>% mutate(outlier = "i_u") # for individual birds: upper
      w_out_lower = data_b3 %>% select(-!!sym(var))  %>% mutate(outlier = "i_l") # for individual birds: lower
    } else if(method %in% "boxplot"){

      # if boxplot....
      w_out = data_b %>% select(-!!sym(var))  %>% mutate(outlier = "i") # for individual birds
    }

    w_out_upper_ia = data_b4 %>% select(-!!sym(var))  %>% mutate(outlier = "ia_u") # for individual birds: upper in relation to the overall
    w_out_lower_ia = data_b5 %>% select(-!!sym(var))  %>% mutate(outlier = "ia_l") # for individual birds: lower in relation to the overall

    # --------------------------------- #
    if(method %in% c("Hampel", "quantile")){
      # UPPER WITHIN BIRD
      i_u = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
        filter(!rn %in% w_out_upper[w_out_upper$TagID %in% cur_group(),]$rn) %>%
        mutate(outlier = "0") %>% select(TagID,rn,outlier) %>% rbind(.,w_out_upper) %>% arrange(TagID, rn)

      # LOWER WITHIN BIRD
      i_l = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
        filter(!rn %in% w_out_lower[w_out_lower$TagID %in% cur_group(),]$rn) %>%
        mutate(outlier = "0") %>% select(TagID,rn,outlier) %>% rbind(.,w_out_lower) %>% arrange(TagID, rn)

      i_ul <- i_u %>% select(-outlier)
      i_ul$outlier <- ifelse(i_u$outlier == "0" & i_l$outlier == "0","0",
                             ifelse(i_u$outlier != "0" & i_l$outlier == "0", i_u$outlier,
                                    ifelse(i_u$outlier == "0" & i_l$outlier != "0", i_l$outlier, "error")))

      #any(i_ul$outlier == "error")
    } else if(method %in% "boxplot"){

      i_ul = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
        filter(!rn %in% w_out[w_out$TagID %in% cur_group(),]$rn) %>%
        mutate(outlier = "0") %>% select(TagID,rn,outlier) %>% rbind(.,w_out) %>% arrange(TagID, rn)


    }
    # --------------------------------- #
    # UPPER ACROSS TOTAL PER BIRD
    ia_u = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
      filter(!rn %in% w_out_upper_ia[w_out_upper_ia$TagID %in% cur_group(),]$rn) %>%
      mutate(outlier = "0") %>% select(TagID,rn,outlier) %>% rbind(.,w_out_upper_ia) %>% arrange(TagID, rn)

    # LOWER ACROSS TOTAL PER BIRD
    ia_l = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
      filter(!rn %in% w_out_lower_ia[w_out_lower_ia$TagID %in% cur_group(),]$rn) %>%
      mutate(outlier = "0") %>% select(TagID,rn,outlier) %>% rbind(.,w_out_lower_ia) %>% arrange(TagID, rn)

    ia_ul <- ia_u %>% select(-outlier)
    ia_ul$outlier <- ifelse(ia_u$outlier == "0" & ia_l$outlier == "0","0",
                            ifelse(ia_u$outlier != "0" & ia_l$outlier == "0", ia_u$outlier,
                                   ifelse(ia_u$outlier == "0" & ia_l$outlier != "0", ia_l$outlier, "error")))

    #any(ia_ul$outlier == "error")

    # add outliers (append on the variable we are using??)
    data = data %>% arrange(TagID, rn)
    data$outlier_i <- i_ul$outlier
    data$outlier_ia <- ia_ul$outlier

    names(data)[which(names(data) %in% c("outlier_i"))] <- paste0("outlier_i_",var,"_",substr(method,1,1))
    names(data)[which(names(data) %in% c("outlier_ia"))] <- paste0("outlier_ia_",var, "_",substr(method,1,1))

    ################ ADD IN FIXES BYOND THE UPPER u_max

    if(!is.null(u_max)){
      if(verbose){message("--- annotating data for u_max ---")}
      data_10 <- data %>%
        group_by(TagID) %>%
        mutate(rn = row_number()) %>%
        select(c(TagID, !!sym(var), rn)) %>%
        filter(rn %in%  data_next[data_next$TagID %in% cur_group(),][,which(names(data_next[data_next$TagID %in% cur_group(),]) %in% "u_max_w")][[1]][[1]])


      w_out_u_max = data_10 %>% select(-!!sym(var)) %>% mutate(u_max = 1) # for individual birds: lower in relatio to the overall

      # BEYOND_u_max
      i_umax = data %>% group_by(TagID) %>% mutate(rn = row_number()) %>%
        filter(!rn %in% w_out_u_max[w_out_u_max$TagID %in% cur_group(),]$rn) %>%
        mutate(u_max = 0) %>% select(TagID,rn,u_max) %>% rbind(.,w_out_u_max) %>% arrange(TagID, rn)

      data$u_max <- i_umax$u_max
    }

    # --------------------------------------------------------- #
    # assign attributes that may have been present at the start
    data <- give_attributes(data, attr_list)

    # ------------------------------------------------------ #
    # new attributes on current function run
    if(is.null(attr(data, "rake_outlie") )){
      attr(data, "rake_outlie") <- "rake_outlie"
    }

    attr(attr(data, "rake_outlie"), "var") <- var
    attr(attr(data, "rake_outlie"), "method") <- method
    attr(attr(data, "rake_outlie"), "u_max") <- u_max

    if(method == "quantile"){
      attr(attr(data, "rake_outlie"), "q") <- q
    } else{
      attr(attr(data, "rake_outlie"), "q") <- NA
    }

    if(summary){
    attr(attr(data, "rake_outlie"), "summary") <- dat # which in turn contains the plot graphic ggplot data
    }
    #else{
    attr(attr(data, "rake_outlie"), "plot_data") <- g2 # then store the plot data thus?
    #}

    data <- data %>% group_by(TagID)
    data <- structure(.Data = data, class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))

    return(data)

  }

  if(!annotate & summary){
    # then just return the summary and plot...

    attr(dat, "plot_data") <- g2 # then store the plot data thus?

    return(dat)
  }

}



