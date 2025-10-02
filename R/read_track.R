#' Reading in animal tracking data from Movebank
#'
#' Connect and download tracking data directly from Movebank repositories.
#'
#' At the core of this function is the desire to have wrapped flexibility
#' to extract sections of data across multiple individuals from specific
#' repositories. \code{read_track_MB} is therefore designed so that a "control file"
#' i.e. with static start and end date-times for TagIDs can be used as a lookup table
#' together with the repository to extract the necessary data with ease.
#'
#' \code{read_track_MB} is built around R package \code{move}. The user may need to
#' access repositories that are not public and so will need the necessary
#' privileges for repositories they are trying to access. The use of
#' \code{read_track_MB} also assumes that the user has specified their login in the
#' global environment. The function makes calls to  \code{move::getMovebankAnimals}
#' and \code{move::getMovebankLocationData} to extract data. WARNING: \code{read_track_MB}
#' makes it possible to extract large amounts of data. However, it is therefore advisable
#' to make as few direct calls to the databases and extract the data needed to be stored
#' and then read in again for subsequent analytical steps.
#'
#' NOTE: \code{read_track_MB} has recently been
#' updated to improve speed and efficiency; it was previously making repeated calls to
#' the \code{move} package, but now the function downloads all data requested and subsets time periods
#' afterwards within the routine. This also allows a little more flexibility for \code{read_track_MB} such that the length of
#' "repos" supplied does not need to be neatly balanced to each animal supplied, e.g. can be multiple TagIDs
#' and one repo. Also NULL can be used for start and ends (as is default) as indicated below,
#' which just downloads all data for that TagID from the repo. Should TagIDs need different start/ends
#' these must be balanced, so length(TagID) must equal length(start) and length(end), but character-format
#' "NULL" can be used within that bird-specific for start and ends i.e. similar to above.
#'
#' \code{read_track_MB} along with it's sister function \code{\link{read_track_UvA}}
#' are designed to extract GPS positional data on the fly in an efficient manner and to directly coerce
#' the resultant \code{base::data.frames} objects to \code{Track}, \code{TrackStack} and \code{TrackMultiStack} classes.
#' The user can also specify \emph{start} and and \emph{end} DateTimes (see below)
#' to specify subsections from the entire dataset available in repositories for individual
#' animals. R library \code{adehabitatLT:as.:traj} is also called to add a trajectory speed to the dataset
#' between fixes, along with a "dt" time difference (secs) and distance (m) between
#' fixes. Note that occasionally the UvA database can go down, and recently the RODBC connection
#' was not working on multiple computers. It is best to email the Dev, or email \code{UvA-BiTS"at"uva.nl}.
#'
#' There are other arguments included in this function for cleaning, but these have
#' been transferred to the function \code{\link{clean_GPS}} function although they
#' are retained here are set to FALSE (see Arguments).
#'
#' @param TagID The individual tag identifier from the Movebank database.
#' @param start The start time to delineate subsets of the total data for the individual in UTC.
#' @param end The end time to delineate subsets of the total data for the individual in UTC.
#' The start and end times must be given in the format "2014-06-25 15:20:23".
#' Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
#' of the animal and must be given in' \emph{character()} format, which are then converted to
#' \code{base::POSIXct} format within the \code{read_track_MB} function. Functionality is also
#' available for situations where only one or other of start and end need to be specified,
#' with the the other defaulting to NULL.
#' @param repo The Movebank repository where tracking data are stored. This is to
#' direct the function to look up animals and their data from Movebank in the right place.
#' @param dropsat Logical defaults to FALSE. This indicates whether to drop GPS fixes with satellites below
#' the \emph{dropsats} threshold. It is suggested the user do not change these
#' from FALSE but instead use the function \code{\link{clean_GPS}}.
#' @param dropsats Numeric value indicating a threshold for satellite retention.
#' @param option One of \code{"BTOTT"} or \code{"BTOTT_MB"} directing the function to return either a subsetted
#' aligned dataset in option "BTOTT" for combining with UvA data, or, "BTOTT_MB", containing the
#' required columns for displaying as a Track object in \code{MoveRakeR}, but doing no other changes
#' to the columns sourced from Movebank, i.e. cannot then be combined with UvA straight off; the latter is useful
#' if variables differ from those standard ones in MB or bespoke ones were added, or you have no idea
#' what UvA data is in which case this bit is probably meaningless, so just select option "BTOTT_MB". That said,
#' the "BTOTT" option will still search for missing column names that are needed for aligning with UvA
#' and if not found will insert NAs anyway BUT this may then get rid of further columns in MoveBank you want to keep.
#' The end result for both options is a \code{Track} object. If you don't want a \code{Track}, then just source
#' data from Movebank using \code{move::getMovebankLocationData}.
#' @param flt_switch logical argument, set to FALSE whether to retain
#' Movebank data erroneous flt_switch values, instead being dealt with in the
#' \code{\link{clean_GPS}} function.
#' @param mindata This specifies a minimum number of rows of data for the individual
#' to be retained and is a form of cleaning, but here is retained to exclude reading
#' in data from animals with too few data deemed unusable for analysis.
#' Defaults to mindata = 5. It can of course be set to zero if a completely raw dataset.
#' @param p4s A coordinate reference system argument using \code{sf::st_crs()}, thus the user just needs to enter in an epsg value, default = 27700.
#' @param reduced TRUE/FALSE (default TRUE) for if a subset of predefined names should be returned; use with caution
#' as Movebank data structures can vary; for example this argument assumes a column called 'altitude' is present on Movebank,
#' which is not always the case.
#' The lat-longs extracted from the Movebank database will be projected via this CRS and stored in two new columns
#' \emph{X} and \emph{Y} (note capitilisation). Note however, that further functions
#' give options to change this if you wish in due course, and the user of course do all point-level manipulation
#' outside the functions if they so wish anyway. Argument Defaults to BNG using epsg string \code{CRS("+init=epsg:27700")}.
#' @param verbose logical (default TRUE) for whether detailed messaging is required.
#'
#' @return The final data outputted for individual animals is a \code{Track and data.frame} object.
#' The output of the function depends on the "option" selection. If "BTOTT" is chosen, and reduced = TRUE, then this will be
#' of 29 variables for each animal read in
#' with column names: Type, TagID, DateTime, speed_2d, speed_3d, traj_speed, latitude, longitude,
#' X, Y, dist, dt, satellites_used, gps_fixtime, gps.pdop, gps.hdop, altitude,
#' altitude_agl, h_accuracy, v_accuracy, pressure, temperature, x_speed, y_speed,
#' z_speed, speed_accuracy, direction, location and flt_switch.
#' Beware that if you have different named columns for variables such as altitude (e.g. height.above.msl perhaps)
#' or instantaneous speed (e.g. ground.speed), then it is best to either use reduced = TRUE or option = "BTOTT_MB".
#' If option = "BTOTT_MB" then the returned data will be whatever is stored in the Movebank database, but note,
#' there may then be
#' issues if combining TWO repos with DIFFERENT data formats so beware in that instance as it may
#' need individual repo read ins and further alignment.
#'
#' For those using both University of Amsterdam and Movebank downloads and then combining datasets (rather niche)
#' using option "BTOTT", data will have columns of all NAs for some variables
#' that were in one database but not another. This alignment has meant the
#' inclusion of missing variables in those cases. For example UvA has a gps.pdop
#' value but Movetech (for data used for initial function build) had gps.hdop.
#' Note also variables may differ to their original truly "raw" naming convention in stored databases.
#' The output for option "BTOTT" here is to align and simplify for further analysis.
#'
#' All time units are in seconds and distances in metres, speeds in m/s. The variable "location" refers to the
#' individual row.name identifier in respective databases, so that no matter what
#' data are extracted in this function, the saved data can be matched back and if
#' necessary recreated through the location variable if need be. This is important
#' for data that is continually being updated remotely and particularly if you
#' are working with the full data downloaded from animals up to very present.
#' Data at the individual level are always provided as a \code{Track} class, and for multiple
#' TagIDs, a \code{TrackStack} object is returned. Note, read in functions do not currently support
#' \code{TrackMultiStack} objects (i.e. the higher level listed class of \code{TrackStack} objects).
#' For \code{TrackMultiStack} objects, \code{TrackStack} objects should be concatenated and coerced using \code{\link{track_multi_stack}}.
#' Names of each animal are provided for \code{Track} lists.
#' **** NOTE TO PACKAGE MAINTAINER: Need to add flexibility in this naming, e.g. should
#' multiple breakdowns of the same animal be needed, or if TagIDs are duplicated in different databases.
#'
#' @references
#'
#' Kranstauber, B. Smolla, M. & Scharf, A.K. (2020). move:
#' Visualizing and Analyzing Animal Track Data. R package version 3.3.0.
#' \url{https://CRAN.R-project.org/package=move}
#'
#' @examples
#' login <- move::movebankLogin("username","password")
#' repo = "repo" # Your MB repository name
#' TagID = c("A","B","C")
#' start = c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23")
#' end = c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45")
#'
#' dataMB <- read_track_MB(TagID,repo=repo,start=start,end=end)
#'
#' # example also reading in full datasets with NULL start and end times defined.
#' dataMB2 <- read_track_MB(TagID,repo=repo) # testing reading in all the data of animals with no specific start ends (NULL be default)
#'
#' @seealso [MoveRakeR::read_track_UvA], [MoveRakeR::Track2move]
#'
#' @export
read_track_MB <- function(TagID=NULL,start=NULL,end=NULL,repo=NULL,
                          option = "BTOTT",dropsat = FALSE, dropsats=3, flt_switch=TRUE, mindata = 5,
                          p4s = 3035, reduced=TRUE,
                          verbose = TRUE){

  if(!is.null(TagID)){
    message(paste0("Processing MB tags ", paste(TagID, collapse = " ")))
  }

  if(is.null(repo)){stop("No MoveBank repository specified")}

  # First assess the birds in the repo(s) supplied
  repo2 <- unique(repo)

  # get the tags from the repo here and set up the list to run through
  if(verbose){message("Checking supplied repo(s) using move::getMovebankAnimals, for animal IDs provided.....")}
  i <- 1
  anilist <- list()
  for(i in 1:length(repo2)){
    anilist[[i]] <- move::getMovebankAnimals(study=repo2[i],login=login)
    anilist[[i]]$repo = repo2[i]
  }
  anilist <- do.call('rbind',anilist)
  anilist <- anilist[anilist$number_of_events > 0,] # take out TagIDs with no data / no deploys

  if(is.null(TagID)){
    TagID <- anilist$local_identifier
    message(paste0("Processing all MB tags in repos ", paste(TagID, collapse = " ")))
  }

  # note this also means you can just list the X repos for the bunch of animals
  # so above warnings are TOO STRICT, make the function fall over / warn if it finds no matches!

  TagID_u <- unique(TagID)
  checklist <- list()
  for(i in 1:length(TagID_u)){
    checklist[[i]] <- grep(TagID_u[i], anilist$local_identifier, fixed=TRUE)
  }

  tags_not_in_repos <- TagID_u[unlist(lapply(checklist,function(x) length(x)==0))]
  tags_in_repos <- TagID_u[unlist(lapply(checklist,function(x) length(x)==1))]

  if(length(tags_not_in_repos) > 0){
    warning(paste0("No data in Movebank repos for TagIDs: ", paste(tags_not_in_repos, collapse = ", ")))
  }

  if(length(tags_not_in_repos) == length(TagID_u)){
    stop(paste0("No data in Movebank repos supplied, for any TagIDs provided"))
  }

  # check if any repos supplied did not have any data for tags supplied
  anilist2 <- anilist[unlist(checklist),]
  ani_repo <- unique(anilist2$repo)

  if(any(!repo2 %in% ani_repo)){
    warning(paste("TagIDs found for some repos but repo:", repo2[!repo2 %in% ani_repo], collapse = " ,"), " had none from those specified")
  } else{
    if(verbose == TRUE){message("....OK")}
  }

  # now need to check the animals supplied are in the repo(s) as requested

  if(verbose == TRUE){message("Calling move::getMovebankLocationData")}

  ###########################
  # also checking for extra column of algorithm.marked.outlier that occurs in some
  first_data <- list()
  for(i in 1:length(ani_repo)){
    TagID_extract <- anilist2[anilist2$repo == ani_repo[i],]$local_identifier

    # check for pure numerics - move package can't handle that
    #check_num <- grepl('^-?[0-9.]+$', test)
    # actually this only applies if the whole set of tags is numeric!
    # if any numeric, make character
    if(is.numeric(TagID_extract)){
      TagID_extract <- as.character(TagID_extract)
    }

    first_data[[i]] <- move::getMovebankLocationData(study=ani_repo[i], sensorID=653, animalName = TagID_extract, login = login)
    if(exists("algorithm.marked.outlier",first_data[[i]])){
      first_data[[i]] <- first_data[[i]][first_data[[i]]$algorithm.marked.outlier == "",]
      first_data[[i]] <- subset(first_data[[i]],select = -algorithm.marked.outlier)
    }
  }

  # as it is assumed the data being extracted is to be ONE STACK of data frames
  # regardless of repo bird was in (e.g. may be multiples across colonies) etc
  # rbind all, and put as a Trackstack by TagID
  # to save rbinding, first_data, suggest go through each and do next functions
  # remove the times not wanted for IDs, this is where we need logic for start and ends
  # for start and ends varying by birds, need to make sure they are present for each TagID
  # so for the lists coming out of the first_data above.....
  # this is the list of birds in first:

  #i = 1
  #### ADJUST THE TAG LOCAL ID TO THAT FED INTO THE FUNCTION (TagID column is made in .finalise_tracks)
  for(i in 1:length(first_data)){
    tid = unique(first_data[[i]]$individual.local.identifier)

    # get rid of square brackets, they are a pain with gsub and regex, with existing process

    #j = 1
    aa <- list()
    for(j in 1:length(tid)){
      #print(j)
      aa[[j]] <- first_data[[i]][first_data[[i]]$individual.local.identifier == tid[j],]

      aa[[j]]$individual.local.identifier <- gsub("\\[","(", tid[j])
      aa[[j]]$individual.local.identifier <- gsub("\\]",")", aa[[j]]$individual.local.identifier)

      #print( unique(aa[[j]]$individual.local.identifier ))

      # lookup the TagID supplied and replace local identifier in MB file
      mm <- list()

      TagID_u_regex <- gsub("\\[","(", TagID_u)
      TagID_u_regex <- gsub("\\]",")", TagID_u_regex)

      TagID_u_regex <- gsub("([()])","\\\\\\1", TagID_u_regex) # bug fix 14/01/2022 load error#2 query GaryC

      #k = 1
      for(k in 1:length(TagID_u)){
        mm[[k]] <- grep(TagID_u_regex[k],tid[j], value = FALSE)
        if(length(mm[[k]]) == 1)  mm[[k]] <- TRUE else  mm[[k]] <- FALSE
      }

      # if ALL false, then have a wierd situation where things are the other way around and the
      # tid is already as we want it?? Or just take as it is if not found.
      # MESSY, a bit between things aligning for our own analyses and reading as is from MB
      # trying to do too much probably with this.
      # i.e.  Belfast "0833" grep trying to be found in "833"
      #my_values_new1 <- sub("^0+", "", my_values)
      #grep("833","0833", value = FALSE)

      ## if all FALSE, take as it is
      if(any(unlist(mm))){
        aa[[j]]$individual.local.identifier <- TagID_u[unlist(mm)]
      }

      #print(unique(aa[[j]]$individual.local.identifier))

      # Previous Bug fix - curlew data not matching the tagID in the local identifier
      #> grep(TagID_u[k],tid[j], value = FALSE)
      #integer(0)
      #> TagID_u[k]
      #[1] "Yf(0E)O/-:Y/m"
      #> tid[j]
      #[1] "Yf(0E)O/-:Y/m"

      #> TagID_u[k]
      #[1] "May_1167_T:95W"
      #> tid[j]
      #[1] "May_1167_T:95W"
      #> grep(TagID_u[k],tid[j], value = FALSE)
      #[1] 1

      # this is because there are brackets in the character string
      # to be honest I forget exactly why we had to do this now, I think in
      # some MB repos there were differences in animal local ID and the
      # indiv local identifier or something so was making sure
      # all aligned but this is a bit verbose!
      # needed a regex neat insertion to account for parentheses
      #grep(TagID_u_regex[k],tid[j], value = FALSE)
      #grep("May_1167_T:95W","May_1167_T:95W", value = FALSE)


    }

    first_data[[i]] <- do.call('rbind', aa)

  }



  ###### for each TagID, need to look for start/ends
  # 1. options if NULL for start or end, then we do no adjustments
  # 2. if ONE VALUE is given for a start or end in the function, then we want to remove data across ALL birds
  # 3. Otherwise, if start is greater than 1, then the user has to have specified balanced st/en data matching specific cases for each TagID

  # error capture
  if(!is.null(start) & length(TagID) > 1 & length(start) > 1 & length(TagID) != length(start)){
    stop("Multiple animals detected with multiple starts, but length(start) != length(TagID)")
  } else
    if(!is.null(end) & length(TagID) > 1 & length(end) > 1 & length(TagID) != length(end)){
    stop("Multiple animals detected with multiple ends, but length(end) != length(TagID)")
  } else
    if(length(end) > 1 & length(start) > 1 & length(end) != length(start)){
    stop("Multiple animals but start and end lengths do not match") # probably can't get to this error from above but just in case
  } else
    if(length(end) == 1 & length(start) > 1){
    stop("Function does not currently handle unbalanced start/end matrix (end == 1, start > 1)") # probably can't get to this error from above but just in case
  } else
    if(length(end) > 1 & length(start) == 1){
    stop("Function does not currently handle unbalanced start/end matrix (end > 1, start == 1)") # probably can't get to this error from above but just in case
  } else{

    # (that's why in above it's >1 because start[1] or end[1] is allowed across all birds)

    #TagID <- c(253,  202, 5377, 4032, 5026,  868,  919, 868)
    #start <- c("25/05/2016 18:15:24","28/05/2016 17:49:28", "10/06/2016 01:00:54","15/05/2016 08:14:01", "10/06/2016 10:57:30", "25/06/2018 11:25:00", "14/06/2018 13:00:00", "01/07/2018 10:14:00")
    #end <- c("30/05/2016 03:19:02","10/06/2016 19:48:20", "22/06/2016 09:06:20","25/05/2016 08:14:01", "15/06/2016 10:57:30",  "30/06/2018 06:45:22", "28/06/2018 16:45:03", "05/07/2018 05:16:19")

    #TagID <- c(253,  202, 5377, 4032, 5026,  868,  919, 868)
    #start <- c("25/05/2016 18:15:24","NULL", "10/06/2016 01:00:54","15/05/2016 08:14:01", "10/06/2016 10:57:30", "25/06/2018 11:25:00", "14/06/2018 13:00:00", "01/07/2018 10:14:00")
    #end <- c("NULL","10/06/2016 19:48:20", "22/06/2016 09:06:20","25/05/2016 08:14:01", "15/06/2016 10:57:30",  "30/06/2018 06:45:22", "28/06/2018 16:45:03", "05/07/2018 05:16:19")

    #TagID <- c(253,  202, 5377, 4032, 5026,  868,  919, 868)
    #start <- "25/05/2016 18:15:24"
    #end <- "25/08/2018 18:15:24"

    #### cycling through first_data[[i]] i.e. stemming from potential multiple repos above

    # make a new list in case multiple tagIDs are requested st/ends
    second_data <- list() #for(u in 1:length(first_data)){second_data[[u]] <- NA}
    for(i in 1:length(first_data)){

      ### CASE 2a starts and ENDS == 1, meaning same across birds (using datetime function allowing different formats of datetimes in simple fashion)
      if(!is.null(start) & length(start) == 1 & !is.null(end) & length(end) == 1){
        if(length(TagID) > 1){
          if(verbose == TRUE){message(paste0("Taking a single start and end time for all TagIDs"))}
        } else{if(verbose == TRUE){message("Using tag-specific start and end times")}}
        second_data[[i]] <- first_data[[i]][first_data[[i]]$timestamp >= datetime(start, auto=TRUE),]
        second_data[[i]] <- second_data[[i]][second_data[[i]]$timestamp <= datetime(end, auto=TRUE),]
     }

      ### CASE 2b starts == 1, ENDS = NULL
      if(!is.null(start) & length(start) == 1 & is.null(end)){
        if(length(TagID) > 1){
          if(verbose == TRUE){message(paste0("Taking a single start time for all TagIDs, end = NULL"))}
        } else{if(verbose == TRUE){message("Using tag-specific start times")}}
        second_data[[i]] <- first_data[[i]][first_data[[i]]$timestamp >= datetime(start, auto=TRUE),]
      }

      ### CASE 2c starts == NULL, ENDS = length 1
      if(is.null(start) & !is.null(end) & length(end) == 1){
        if(length(TagID) > 1){
          if(verbose == TRUE){message(paste0("Taking a single end time for all TagIDs, start = NULL"))}
        } else{if(verbose == TRUE){message("Using tag-specific end times")}}
        second_data[[i]] <- second_data[[i]][second_data[[i]]$timestamp <= datetime(end, auto=TRUE),]
      }

      # note all boundary conditions treated as inclusive
      # also the user may have incorrectly specified tagid numbers (dealt with above)
      # but needs the routine to select the right start and end from lookup

      # DEV NOTE I THINK BELOW WORKS ALSO FOR THE SINGLE CONDITIONS ABOVE

      ### CASE 3a (note from above error catch it's not possible at this point for starts and ends not to be balanced AND match the TagID length)
      if(!is.null(start) & length(start) > 1 & !is.null(end) & length(end) > 1){
        if(verbose == TRUE){
          if(i == 1){message("Using tag-specific start and/or end times")}
          }

        # obviously have balanced inputs: build a lookup data.frame
        lookup_df <- data.frame(TagID,start,end) # note this could allow multiple TagID and date times??
        lookup_df$start <- ifelse(lookup_df$start == "NULL", NA,lookup_df$start)
        lookup_df$end <- ifelse(lookup_df$end == "NULL", NA,lookup_df$end)

        # if any NULLs are specified, which have to be character to make the c() work and be balanced,
        # then suggest easier to replace those with the min or max of that bird's data
        # but then you won't kniow that be cycling through [[i]] of first_data if animal not there

        for(j in 1:nrow(lookup_df)){
          if(!any(is.na(lookup_df[j,]))){
            if(any(datetime(lookup_df[j,]$end) < datetime(lookup_df[j,]$start))){
              stop("End datetimes for one or more TagIDs are less than start")
            }
          }
        }

        tid = unique(first_data[[i]]$individual.local.identifier)

        # best to cycle through the lookup_df NOT the birds in the first_data[[i]]??
        # get which TagIDs match the lookup first
        # subset the lookup for TIDs from the repo in which found
        lookup_df_ <- lookup_df[lookup_df$TagID %in% tid,]

        j <- 1
        sublist <- list()
        for(j in 1:nrow(lookup_df_)){

          sub1 <- first_data[[i]][first_data[[i]]$individual.local.identifier == lookup_df_[j,]$TagID,]

          if(!is.na(lookup_df_[j,]$start)){
            start1 <- datetime(lookup_df_[j,]$start)
          } else start1 <- NA
          if(!is.na(lookup_df_[j,]$end)){
            end1 <- datetime(lookup_df_[j,]$end)
          } else end1 <- NA

          if(is.na(start1) & !is.na(end1)){
            sub1 <- sub1[sub1$timestamp <= end1,]
          }
          if(!is.na(start1) & is.na(end1)){
            sub1 <- sub1[sub1$timestamp >= start1,]
          }
          if(!is.na(start1) & !is.na(end1)){
            sub1 <- sub1[sub1$timestamp >= start1 & sub1$timestamp <= end1,]
          }
          sublist[[j]] <- sub1
        }
        second_data[[i]] <- sublist
        #first_data[[i]] <- sub1 # if overwriting the original data, then not possible to have >1 time section per animal
      }

      ### CASE 3b start only END = NULL
      if(!is.null(start) & length(start) > 1 & is.null(end)){
        if(verbose == TRUE){
          if(i == 1){message("Using tag-specific start times (end = NULL)")}
        }

        # obviously have balanced inputs: build a lookup data.frame
        lookup_df <- data.frame(TagID,start) # note this could allow multiple TagID and date times??
        lookup_df$start <- ifelse(lookup_df$start == "NULL", NA,lookup_df$start)

        tid = unique(first_data[[i]]$individual.local.identifier)

        lookup_df_ <- lookup_df[lookup_df$TagID %in% tid,]

        j <- 1
        sublist <- list()
        for(j in 1:nrow(lookup_df_)){

          sub1 <- first_data[[i]][first_data[[i]]$individual.local.identifier == lookup_df_[j,]$TagID,]

          if(!is.na(lookup_df_[j,]$start)){
            start1 <- datetime(lookup_df_[j,]$start)
          } else start1 <- NA # in which case no subsetting done

          if(!is.na(start1)){
            sub1 <- sub1[sub1$timestamp >= start1,]
          }
          sublist[[j]] <- sub1
        }
        second_data[[i]] <- sublist
      }

      ### CASE 3c end only START = NULL
      if(!is.null(end) & length(end) > 1 & is.null(start)){
        if(verbose == TRUE){
          if(i == 1){message("Using tag-specific end times (start = NULL)")}
        }

        # obviously have balanced inputs: build a lookup data.frame
        lookup_df <- data.frame(TagID,end) # note this could allow multiple TagID and date times??
        lookup_df$start <- ifelse(lookup_df$end == "NULL", NA,lookup_df$end)

        tid = unique(first_data[[i]]$individual.local.identifier)
        lookup_df_ <- lookup_df[lookup_df$TagID %in% tid,]

        j <- 1
        sublist <- list()
        for(j in 1:nrow(lookup_df_)){

          sub1 <- first_data[[i]][first_data[[i]]$individual.local.identifier == lookup_df_[j,]$TagID,]

          if(!is.na(lookup_df_[j,]$end)){
            end1 <- datetime(lookup_df_[j,]$end)
          } else end1 <- NA # in which case no subsetting done

          if(!is.na(end1)){
            sub1 <- sub1[sub1$timestamp <= end1,]
          }
          sublist[[j]] <- sub1
        }
        second_data[[i]] <- sublist
      }

  }


    # otherwise OPTION 1, you have NULLs for starts and ends, so second_data will still be a list
    if(is.null(start) & is.null(end)){
      second_data <- first_data
    }
    # NOTE in the above "NULL" is needed as character OR just NA

  }



  #length(second_data)
  # then at this point you have the second_data list, again 1 and 2 from the different repos
  # BUT then each TagID is subsetted for datetimes as a further list level AND...in the
  # second level you can have MULTIPLE tagIDs for periods selected

  # then the next job is to apply the finalise tracks .finalise_tracks above.

  # then all else below can be dropped!!
  p4sin <- p4s

  #data <- third_data[[1]][[1]]

  # internal function to finalise the tracks and get in standardised format for analysis
  #data = third_data[[1]][[1]]

  ################### ADDITIONS TO MAKE THE FUNCTION WORK WITH BTOTT in mind for aligning with UvA OR just MoveBank raw raead in
  #option = "BTOTT" # the format required for aligning to UvA data as normally used
  #option = "BTOTT_MB" # retains ALL MB variables but inserts additional copies needed for Track objects in BTOTT to be used

  # BUG FIX: 07/12/2021 AND 28/02/2023 (latter issue of flt_switch column still being retained if no flt.switch coloumn found! Causes downstream issues with clean_GPS())
  #.finalise_tracks(third_data[[1]][[1]])
  #data =third_data[[1]][[15]]

  .finalise_tracks <- function(data, option){

    #print(unique(data$individual.local.identifier))
    # proceed with normal processing
    # this is always needed

    if(exists("timestamp",data)){
      data$DateTime <- as.POSIXct(data$timestamp,format = "%Y-%m-%d %H:%M:%S",tz="UTC")
    } else{
      stop("MB data should have 'timestamp' if you want to read MB data into BTOTT a 'Track' format")
    }
    if(exists("location.long",data)){
        data$longitude <- data$location.long
    } else{
      stop("MB data should have 'location.long' if you want to read MB data into BTOTT a 'Track' format")
    }
    if(exists("location.lat",data)){
      data$latitude <- data$location.lat
    } else{
      stop("MB data should have 'location.lat' if you want to read MB data into BTOTT a 'Track' format")
    }
    if(exists("individual.local.identifier",data)){
      data$TagID <- data$individual.local.identifier
    } else{
      stop("MB data should have 'individual.local.identifier' if you want to read MB data into BTOTT a 'Track' format")
    }


    #### continue further processing for BTOTT
    if(option == "BTOTT"){

      # this is not ideal if both are present!! In which case would break out of BTOTT formatting....
      if(exists("height.above.ellipsoid",data)){
        names(data)[which(names(data) == "height.above.ellipsoid")] <- "altitude"
      }
      if(exists("height.above.msl",data)){
        names(data)[which(names(data) == "height.above.msl")] <- "altitude"
      }
      if(exists("ground.speed",data)){
        names(data)[which(names(data) == "ground.speed")] <- "ground.speed.MT"
      }
      if(exists("gps.satellite.count",data)){
        names(data)[which(names(data) == "gps.satellite.count")] <- "satellites_used"
      }
      if(exists("event.id",data)){
        names(data)[which(names(data) == "event.id")] <- "location"
      }
      if(exists("ground.speed",data)){
        names(data)[which(names(data) == "ground.speed")] <- "speed_2d"
      }
      if(exists("location.error.numerical",data)){
        names(data)[which(names(data) == "location.error.numerical")] <- "h_accuracy"
      }
      if(exists("vertical.error.numerical",data)){
        names(data)[which(names(data) == "vertical.error.numerical")] <- "v_accuracy"
      }
      if(exists("heading",data)){
        names(data)[which(names(data) == "heading")] <- "direction"
      }
      if(exists("external.temperature",data)){
        names(data)[which(names(data) == "external.temperature")] <- "temperature"
      }
      if(exists("barometric.pressure",data)){
        names(data)[which(names(data) == "barometric.pressure")] <- "pressure"
      } else{
        data$pressure <- NA # don't think there are any pressure measurements in the MT data yet, although is in example of Daniel's for Tysties
      }

      data$Type <- "MoveBank"
      data$gps.pdop <- NA   # in UvA but not Movetech
      data$altitude_agl <- NA # in UvA but not Movetech
      data$speed_3d <- NA
      data$gps_fixtime <- NA
      data$x_speed <- NA
      data$y_speed <- NA
      data$z_speed <- NA
      data$speed_accuracy <- NA

      # further in MB not in UVA (unless merged)
      if(exists("tag.voltage",data)){
        names(data)[which(names(data) == "tag.voltage")] <- "tag_voltage"
      } else{
        data$tag_voltage <- NA
      }
      if(exists("battery.charging.current",data)){
        names(data)[which(names(data) == "battery.charging.current")] <- "battery_charging_current"
      } else{
        data$battery_charging_current <- NA
      }

    }

    #dataVo <- read_voltage_UvA(TagID, start=start, end=end)

    # common to all options
    data <- data[!is.na(data$latitude),]
    data <- data[!is.na(data$longitude),]

    if(nrow(data) > 0){
      # these options can be done for both though
      # project_points

      # OLD sp method
      #bb <- data.frame(Latitude = data$latitude, Longitude = data$longitude)
      #sp::coordinates(bb) <- c("Longitude", "Latitude")
      #sp::proj4string(bb) <- sp::CRS("+proj=longlat +ellps=WGS84")
      #bb <- sp::spTransform(bb, p4sin)
      #bb2 <- as.data.frame(bb)
      #data$X <- bb2$Longitude
      #data$Y <- bb2$Latitude

      # new sf updated
      #data <- MoveRakeR::LBBGWalB201416[[1]][[1]]
      #p4sin <- 27700
      #sf::st_crs()
      data_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
      data_sf <- sf::st_transform(data_sf, p4sin)
      coords = sf::st_coordinates(data_sf)
      data$X <- coords[,1]
      data$Y <- coords[,2]
      data <- data[!duplicated(data$DateTime),]

      # TRAJ SPEED AND dist/dt
      dataXa <- adehabitatLT::as.ltraj(subset(data,select=c(X,Y)),data$DateTime,id=1)
      dataXa  <- dataXa[[1]]
      data$dist <- dataXa$dist
      data$dt <- dataXa$dt
      data$traj.speed <- data$dist / data$dt # in m/s

      # drop data with less or equal to than X satellites
      if(dropsat == TRUE){

        if(option == "BTOTT"){
          if(exists("satellites_used",data)){
            data <- data[data$satellites_used > dropsats,]
          } else{
            warning("No satellites_used column found, no further dropping of no. satellites < dropsat made")
          }

        }

        if(option == "BTOTT_MB"){

          if(exists("gps.satellite.count",data)){
            data <- data[data$gps.satellite.count > dropsats,]
          } else{
            warning("No gps.satellite.count column found, no further dropping of no. satellites < dropsat made")
          }
        }

      }

      flt = FALSE
      if(flt_switch){

        if(exists("flt.switch",data)){

          # note this assumes flt.switch column -99 will always be the formatting used in MB
          flt = TRUE # set to TRUE TO NOT FILL WITH ZEROS BELOW
          ##### switch != 0 removed - I think this was covered anyway but making more explicit here
          data$flt.switch <- ifelse(is.na(data$flt.switch),-99,data$flt.switch)
          data$flt.switch <- ifelse(data$flt.switch != 0,-99,data$flt.switch)
          #data <- data[data$flt.switch != -99,]

          if(option == "BTOTT"){ # silly underscore renaming!
            names(data)[which(names(data) == "flt.switch")] <- "flt_switch"
          }

        } else{
          warning("No flt.switch column found, no assessment of flt.switch values made")
        }

      }

      if(dim(data)[1] > 0){ # by removing duff data from MB via flt_switch can reduce some tag data to none

        data <- data[!is.na(data$latitude),]
        data <- data[!duplicated(data$DateTime),]

        if(option == "BTOTT"){
          # reduce dataframe to a more manageable number of columns
          # BUT also need to check we have the columns from the renaming above. And return a warning
          # maybe .... although that would be a repeated warning across animals...

          BTOTT_nm_check <- c("Type","TagID","DateTime","speed_2d","speed_3d","traj.speed","latitude","longitude","X","Y","dist","dt","satellites_used","gps_fixtime","gps.pdop","gps.hdop","altitude","altitude_agl","h_accuracy","v_accuracy","pressure","temperature","x_speed","y_speed","z_speed","speed_accuracy","direction","location","flt_switch","tag_voltage","battery_charging_current")

          miss <- which(!BTOTT_nm_check %in% names(data))
          if(length(miss) > 0){
            nm_miss <- BTOTT_nm_check[miss]
            # add as NA to the data

            addin <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(nm_miss)))
            names(addin) <- nm_miss
            data <- cbind(data,addin)

          }

          if(reduced){
            data <- subset(data,select = c(Type,TagID,DateTime,speed_2d,speed_3d,traj.speed,latitude,longitude,X,Y,dist,dt,satellites_used,gps_fixtime,gps.pdop,gps.hdop,altitude,altitude_agl,h_accuracy,v_accuracy,pressure,temperature,x_speed,y_speed,z_speed,speed_accuracy,direction,location,flt_switch,tag_voltage,battery_charging_current))
          }

          # flt_switch column is ALWAYS returned even if user doesn't want it OR if there is no flt.switch column in MB dataset!
          # so if that's the case, force acceptance of all row values under BTOTT formatting
          if(!flt){data$flt_switch <- 0}

        }


      } else{
        warning(paste0("Too little or no data for ", unique(data$TagID), " so excluded"))
        data <- NULL
        return(data)
      }


      if(!is.null(data)){
        if(dim(data)[1] < mindata){
          warning(paste0("Too little or no data for ", TagID, " so excluded"))
          data <- NULL
          return(data)
        } # if less than X data points set data to NULL and skip it
        return(Track(data)) # both BTOTT and BTOTT_MB options give this
      }
    } else{
      return(NULL)
    }



  }

  # this needs to be cycled OVER the second_data[[1]][[1]] structure of lists
  # if you ONLY ran one start or end across all birds OR did no star/end corrections
  # then you have a TrackStack style list of MULTIPLE birds
  # break that out into a TMS

  # Structure from specific tagId subsetting Barrow 2016
  #
  #List of 1
  #$ :List of 9
  #..$ :'data.frame':	471 obs. of  33 variables:

  #### IF NO DATA AT ALL ACROSS REPOS, stop at this point
  # any repos with n data for dates for birds?

  # if second data is all NULL:
  if(all(do.call('rbind',lapply(second_data,is.null))[,1])){
    stop("No data available for TagIDs, DateTimes and chosen repos")
  }

  # if second data is a nested list:
  if(inherits(second_data[[1]], "list")){
    ls_check <- list()
    for(i in length(second_data)){
      ls_check[[i]] <- all(do.call('rbind',lapply(second_data[[i]],function(x){nrow(x) == 0}))[,1])
    }
    ls_check <- do.call('rbind',ls_check)[,1]
    if(any(ls_check== TRUE)){
      stop("No data available for TagIDs, DateTimes and chosen repos")
    }
  }
  # if the data so far are just one list:
  if(!inherits(second_data[[1]], "list")){
    if(all(do.call('rbind',lapply(second_data,function(x){nrow(x) == 0}))[,1])){
      stop("No data available for TagIDs, DateTimes and chosen repos")
    }
  }


  ### BUG CURRENTLY FIXED 03/12/2021 keeping these tests here
  ## not worked
  #TagID = c(202,278,253)
  #start = c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23")
  #end = c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45")
  #repo = "BTO - North West England 2016 - Lesser Black-backed Gull"
  #
  #dataMB <- read_track_MB(TagID,repo=repo,start=start,end=end)
  #
  ## worked
  #TagID=NULL
  #repo="BTO - Copeland 2021 - Black Guillemot"
  #start=NULL
  #end=NULL
  #
  #dataMB_Cop <- read_track_MB(TagID=NULL,repo="BTO - Copeland 2021 - Black Guillemot")
  #
  #str(second_data) # NW england three tags, st/ends
  #List of 1
  #$ :List of 3
  #..$ :'data.frame':	1495 obs. of  33 variables:
  #  .. ..$ event.id                       : num [1:1495] 1.59e+09 1.59e+09 1.59e+09 1.59e+09 1.59e+09 ...
  #.. ..$ visible                        : chr [1:1495] "true" "true" "true" "true" ...
  #.. ..$ timestamp                      : POSIXct[1:1495], format: "2016-06-01 14:23:57" ...
  #.. ..$ location.long                  : num [1:1495] -
  #
  # Daniel's copeland read in of all birds
  #List of 1
  #$ :'data.frame':	94652 obs. of  30 variables:
  #  ..$ event.id                        : num [1:94652] 1.99e+10 1.99e+10 1.99e+10 1.99e+10 1.99e+10 ...
  #..$ visible                         : chr [1:94652] "true" "true" "true" "true" ...
  #..$ timestamp                       : POSIXct[1:94652], format: "2021-06-20 10:00:42" "2021-06-20 10:05:40" ...
  #
  #dan <- second_data
  #chris <- second_data


  # SINGLE LIST STRUCTURE
  if(inherits(second_data[[1]], "data.frame")){

    # any nulls for repos?
    if(any(do.call('rbind',lapply(second_data,is.null))[,1])){
      second_data <- second_data[-which(sapply(second_data, is.null))]
    }

    # any repos with n data for dates for birds?
    if(any(do.call('rbind',lapply(second_data,function(x){nrow(x) == 0}))[,1])){
      second_data <- second_data[-which(sapply(second_data, function(x){nrow(x) == 0}))]
    }

    third_data <- list()
    sublist <- list()
    for(i in 1:length(second_data)){
      # e.g. may only be length one for only one repo
      tid <- unique(second_data[[i]]$individual.local.identifier)
      for(j in 1:length(tid)){
        sublist[[j]] <- second_data[[i]][second_data[[i]]$individual.local.identifier == tid[j],]
      }
      third_data[[i]] <- sublist
      names(third_data[[i]]) <- tid
    }
  }


  # NESTED NESTED LIST STRUCTURE
  if(inherits(second_data[[1]][[1]], "data.frame")){
    # then we have a list in a list, TMS type

    # any nulls for repos? NOTE SAME AS ABOVE BUT THIS MAY NEED FURTHER THOUGHT........

    ls_check <- list()
    for(i in length(second_data)){
      ls_check[[i]] <- all(do.call('rbind',lapply(second_data[[i]],function(x){nrow(x) == 0}))[,1])
    }
    ls_check <- do.call('rbind',ls_check)[,1]
    if(any(ls_check== TRUE)){
      stop("No data available for TagIDs, DateTimes and chosen repos")
    }

    third_data <- second_data

    # but...naming, if you have multiple extractions of a bird for DIFFERENT dates, then
    # this needs fixing
    for(i in 1:length(third_data)){
      nms <- as.vector(do.call('rbind',lapply(third_data[[i]], function(x){unique(x$individual.local.identifier)}))[,1])
      # rename with a _1, _2, _3 etc for each duplicate TagID
      nms <- make.unique(as.character(nms), sep = '_')
      names(third_data[[i]]) <- nms
    }

  }

  #f = function(x){
  #  max(x$timestamp)
  #}
  #lapply(third_data[[3]],f)

  #.finalise_tracks(third_data[[1]][[1]])

  #str(third_data[[1]][[1]])

  ####
  if(verbose == TRUE){message("Building final Track object...")}

  stdata = list()
  i <- 1
  for(i in 1:length(third_data)){

    stdata[[i]] <- lapply(third_data[[i]],.finalise_tracks, option = option)

    t1 <- lapply(stdata[[i]], function(x) if(!is.null(nrow(x))){x} else NULL)
    stdata[[i]] <- t1[!sapply(t1,is.null)]

    if(length(stdata[[i]]) == 0){
      if(verbose == TRUE){message(paste0("Function dropped out all data for stack ", i))}
      stdata[[i]] <- NULL
    }

    if(length(stdata) > 0){
      if(length(stdata[[i]]) > 0){
        stdata[[i]] <- TrackStack(stdata[[i]])
      }
    }

  }
  stdata <- stdata[!sapply(stdata,is.null)]


  # it is possible that all of a TrackStack could get dropped (Bowland 2016 34 bird)
  # so you can then skip the TMS bit below
  # Also if NO DATA AT ALL IS RETURNED, then needs to also be accounted for
  stdata2 <- NULL
  if(length(stdata) > 0){
    # gclewley, best return as a Track object in keeping with read_track_UvA, now updated
    # there was also a bug in this in that sometimes it returns an NA for a TagID...

    # data are returned as a TrackStack list - need to turn this into a Track
    # if only one animal, then will already be a Track
    for(i in 1:length(stdata)){
      if(is_TrackStack(stdata[[i]])){
        stdata2[[i]] <- TrackStack2Track(stdata[[i]])
      } else{
        stdata2[[i]] <- Track(stdata[[i]])
      }
    }

    # if more than one Stack (from different repos) then rbind all together as one Track
    # otherwise if only one, then unlist the single element
    # or if no data returned, then NULL it
    if(length(stdata2) > 1){
      outdata <- do.call('rbind', stdata2)
    } else{
      outdata <- stdata2[[1]]
    }
  } else{
    outdata <- NULL
  }

    #outdata <- TrackMultiStack(stdata)
    #outdata <- TrackMultiStack2TrackStack(outdata, rename = FALSE)
  #} else if(length(stdata) == 0){
  #  outdata <- NULL
  #}
    #else{
    #outdata <- stdata[[1]]
  #}

  if(verbose == TRUE){message("...done")}

  return(outdata)

}

#' Reading in animal tracking data from the University of Amsterdam Bird Tracking (UvA-BiTS) database.
#'
#' Connect and download tracking data directly from the UvA-BiTS phpPGAdmin SQL database.
#'
#' At the core of this function is the desire to have wrapped flexibility
#' to extract sections of data across multiple individuals from specific
#' repositories. \code{read_track_UvA} along with its sister function \code{read_track_MB}
#' is therefore designed to do just that, so that a "control file"
#' i.e. with static start and end date-times for TagIDs can be used as a lookup table,
#' to extract the necessary data with ease. WARNING: \code{read_track_UvA}
#' requires access to the UvA-BiTS database in the first place, and further, it is
#' possible within these read in functions to extract large amounts of data. It is therefore advisable
#' to make as few direct calls to the databases and extract the data needed to be stored
#' and then read in again for subsequent analytical steps.
#'
#' \code{read_track_UvA} uses an ODBC connection to establish a link with the UvA-BiTS
#' database. \strong{WARNING: THIS MUST BE SET UP FIRST BEFORE USING THIS FUNCTION}
#' and of course you must have prior access to the UvA-BiTS system with a username and password. Steps to do
#' this are provided here: \url{https://gitlab.com/uva_ibed_ame/uva-bits/-/wikis/How-to-access-the-e-Ecology-database}.
#' Extraction of GPS positional data from the database is made using R library \code{RODBC}
#' and the user must also first establish the database connection in their global environment
#' named as object \strong{'db'}, specifying whet the connection was named as in the help file steps above:
#' in this case called "GPS", using \code{RODBC::odbcConnect("GPS")}. See examples.
#'
#' \code{read_track_UvA} then uses \code{RODBC} to construct SQL statements. Along with it's sister function \code{\link{read_track_MB}},
#' \code{read_track_UvA} is designed to extract GPS positional data on the fly in an efficient manner and to directly coerce
#' the resultant \code{base::data.frames} objects to \code{Track}, \code{TrackStack} and \code{TrackMultiStack} classes.
#' The user can also specify \emph{start} and and \emph{end} DateTimes (see below)
#' to specify subsections from the entire dataset available in repositories for individual
#' animals. R library \code{adehabitatLT:as.:traj} is also called to add a trajectory speed to the dataset
#' between fixes, along with a "dt" time difference (secs) and distance (m) between
#' fixes.
#'
#' There are other arguments included in this function for cleaning, but these have
#' been transferred to the function \code{\link{clean_GPS}} function although they
#' are retained here are set to FALSE (see Arguments).
#'
#' @param TagID The individual tag identifier from the UvA-BiTS database.
#' @param start The start time to delineate subsets of the total data for the individual in UTC.
#' @param end The end time to delineate subsets of the total data for the individual in UTC.
#' The start and end times must be give in the format "2014-06-25 15:20:23".
#' Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
#' of the animal and must be given in' \emph{character()} format, which are then converted to
#' \code{base::POSIXct} format within the \code{read_track_MB} function. Functionality is also
#' available for situations where only one or other of start and end need to be specified,
#' with the the other defaulting to NULL. NOTE: THE FUNCTION WILL RETURN AN ERROR IF NO DATA ARE FOUND BETWEEN START AND END TIMES SPECIFIED.
#' @param dropsat Logical defaults to FALSE. This indicates whether to drop GPS fixes with satellites below
#' the \emph{dropsats} threshold. It is suggested the user do not change these
#' from FALSE but instead use the function \code{\link{clean_GPS}}.
#' @param dropsats Numeric value indicating a threshold for satellite retention.
#' @param flt_switch. logical argument, set to FALSE whether to retain
#' Movebank data erroneous flt_switch values, instead being dealt with in the
#' \code{\link{clean_GPS}} function.
#' @param mindata This specifies a minimum number of rows of data for the individual
#' to be retained and is a form of cleaning, but here is retained to exclude reading
#' in data from animals with too few data deemed unusable for analysis.
#' Defaults to mindata = 5. It can of course be set to zero if a completely raw dataset.
#' @param pressure logical defaulting to FALSE, but if set to TRUE, the function will look to
#' see if there are any pressure data for the tag and date-time range, and merge it in. Note the data
#' can also be extracted using function \code{\link{read_pressure_UvA}} independently. If TRUE,
#' the "pressure" and "temperature" columns are overwritten by the data in the UvA pressure database view,
#' taking the form of averages of no. data points collected by the pressure sensor. Pressure units are in MB
#' and temperature in degC. A warning is returned if no pressure data are found if TRUE is specified.
#' @param p4s A coordinate reference system argument using \code{sf::st_crs()}, thus the user just needs to enter in an epsg value, default = 3035.
#' The lat-longs extracted from the Movebank database will be projected via this CRS and stored in two new columns
#' \emph{X} and \emph{Y} (note capitilisation). Note however, that further functions
#' give options to change this if you wish in due course, and the user of course do all point-level manipulation
#' outside the functions if they so wish anyway. Argument Defaults to BNG using epsg string \code{CRS("+init=epsg:27700")}.
#'
#' @return The final data outputted for individual animals is a \code{Track and data.frame} object of 29 variables for each
#' TagID; Type, TagID, DateTime, speed_2d, speed_3d, traj_speed, latitude, longitude,
#' X, Y, dist, dt, satellites_used, gps_fixtime, gps.pdop, gps.hdop, altitude,
#' altitude_agl, h_accuracy, v_accuracy, pressure, temperature, x_speed, y_speed,
#' z_speed, speed_accuracy, direction, location and flt_switch.
#' It is assumed here the user will be familiar with most of these.
#' You will notice that for Movebank and data downloaded from the University of Amsterdam
#' i.e. from different sources, will have columns of all NAs for some variables
#' that were in one database but not another. This alignment has meant the
#' inclusion of missing variables in those cases. For example UvA has a gps.pdop
#' value but Movetech has gps.hdop. Note also variables may differ to their
#' original truly "raw" naming convention in stored databases. The output here is
#' to align and simplify for further analysis. All time units are in seconds
#' and distances in metres, speeds in m/s. Importantly "location" refers to the
#' individual row.name identifier in respective databases, so that no matter what
#' data are extracted in this function, the saved data can be matched back and if
#' necessary recreated through the location variable if need be. This is important
#' for data that is continually being updated remotely and particularly if you
#' are working with the full data downloaded from animals up to very present.
#' Data at the individual level are always provided as a \code{Track} class, and for multiple
#' TagIDs, a \code{TrackStack} object is returned. Note, read in functions do not currently support
#' \code{TrackMultiStack} objects (i.e. the higher level listed class of \code{TrackStack} objects).
#' For \code{TrackMultiStack} objects, \code{TrackStack} object should be concatenated and coerced using \code{\link{track_multi_stack}}.
#' Names of each animal are provided for \code{Track} lists.
#'
#' @examples
#'
#' db_file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db_file) # currently required "db" to be named globally
#'
#' ## Example using direct calls to UvA-BiTS database using SQL coding for date-time start and ends per animal
#' # Note, you can extract multiple birds with the same start-ends this way too
#' TagID <- "('1','2','3')" # Tag (bird) ID's replace with your actual IDs
#' sttime <- "'2020-05-30 17:00:00'"
#' entime <- "'2020-06-02 17:00:00'"
#' data <- RODBC::sqlQuery(db, query =
#'                    paste(
#'                      "select", "device_info_serial, date_time, latitude, longitude, altitude, pressure, temperature, satellites_used, gps_fixtime, positiondop, h_accuracy, v_accuracy, x_speed, y_speed, z_speed, speed_accuracy, location, userflag, speed_3d, speed_2d, direction, altitude_agl",
#'                     "from", "gps.ee_tracking_speed_limited", "where",
#'                     "device_info_serial", "in", TagID,
#'                     "and",
#'                     "date_time >=", sttime,
#'                     "and",
#'                     "date_time <=", entime)
#' )
#'
#' ## Using read_track_UvA - read in three birds with different start/end times in 2016
#' TagID <- "('1','2','3')"
#' start = c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23") # e.g. using specific starts/ends per bird
#' end = c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45")
#' dataUvA <- read_track_UvA(TagID,start=start,end=end)
#'
#' ### testing pressure as well - if available this will be extracted and added to the GPS dataset
#' TagID <- "('1','2','3')"
#' start = c("2020-06-01 13:53:50", "2020-06-01 12:22:13", "2020-06-05 08:07:23")
#' end = c("2020-07-15 09:17:14", "2020-07-20 01:08:58", "2020-07-18 14:22:45")
#' data_pr <- read_track_UvA(TagID=TagID,start=start,end=end,pressure = TRUE)
#'
#' # comparing numbers of records and extraction through read_pressure_UvA (one bird)
#' dataPr <- read_track_UvA(TagID='1',start="2020-06-01 13:53:50",end="2020-07-15 09:17:14",pressure=TRUE)
#' dataPr2 <- read_pressure_UvA(TagID='1',start="2020-06-01 13:53:50",end="2020-07-15 09:17:14")
#'
#' @seealso [MoveRakeR::read_track_MB], [MoveRakeR::read_pressure_UvA], [MoveRakeR::read_accn_UvA]
#'
#' @export
read_track_UvA <- function(TagID,start=NULL,end=NULL, dropsat = FALSE, dropsats=3, flt_switch=FALSE, mindata = 5, pressure = FALSE,
                           p4s = 3035,
                           verbose = TRUE){

  print(paste0("Processing UvA tags ", paste(TagID, collapse = " ")))
  p4sin = p4s # rename CRS oject so no recursion

  # error capture
  if(!is.null(start) & length(TagID) > 1 & length(start) > 1 & length(TagID) != length(start)){
    stop("Multiple animals detected with multiple starts, but length(start) != length(TagID)")
  }
  if(!is.null(end) & length(TagID) > 1 & length(end) > 1 & length(TagID) != length(end)){
    stop("Multiple animals detected with multiple ends, but length(end) != length(TagID)")
  }
  if(length(end) > 1 & length(start) > 1 & length(end) != length(start)){
    stop("Multiple animals but start and end lengths do not match") # probably can't get to this error from above but just in case
  }

  if(verbose == TRUE){message("Linking to UvA database....")}

  ####### internal function to apply across input values
  .track_prep <- function(TagID, start, end, p4s=p4s){
    # .track_prep will expect a character now of NULLs to convert to R NULLs for mapply to work! Not that elegant

    if(any(start == "NULL")){start = NULL}
    if(any(end == "NULL")){end = NULL}

    # convert start and end times to posixCT format
    if(!is.null(start)){start <- as.POSIXct(start,format="%Y-%m-%d %H:%M:%S",tz="UTC")}
    if(!is.null(end)){end <- as.POSIXct(end,format="%Y-%m-%d %H:%M:%S",tz="UTC")}

    blist = paste0("('",TagID,"')")

    # if start and end are supplied, then use them, if not, get ALL the data for the bird!

    if(!is.null(start)){sttime = paste0("'",start,"'")}
    if(!is.null(end)){entime = paste0("'",end,"'")}

    ############## GPS TRACKING DATA
    if(is.null(start) & is.null(end)){ # no start and end given
      Query = paste(
        "select", "device_info_serial, date_time, latitude, longitude, altitude, pressure, temperature, satellites_used, gps_fixtime, positiondop, h_accuracy, v_accuracy, x_speed, y_speed, z_speed, speed_accuracy, location, userflag, speed_3d, speed_2d, direction, altitude_agl",
        "from", "gps.ee_tracking_speed_limited", "where",
        "device_info_serial", "in", blist)
    }
    if(is.null(start) & !is.null(end)){ # end time selected but no start
      Query = paste(
        "select", "device_info_serial, date_time, latitude, longitude, altitude, pressure, temperature, satellites_used, gps_fixtime, positiondop, h_accuracy, v_accuracy, x_speed, y_speed, z_speed, speed_accuracy, location, userflag, speed_3d, speed_2d, direction, altitude_agl",
        "from", "gps.ee_tracking_speed_limited", "where",
        "device_info_serial", "in", blist,
        "and","date_time <=", entime)
    }
    if(!is.null(start) & is.null(end)){ # start time given but no end
      Query = paste(
        "select", "device_info_serial, date_time, latitude, longitude, altitude, pressure, temperature, satellites_used, gps_fixtime, positiondop, h_accuracy, v_accuracy, x_speed, y_speed, z_speed, speed_accuracy, location, userflag, speed_3d, speed_2d, direction, altitude_agl",
        "from", "gps.ee_tracking_speed_limited", "where",
        "device_info_serial", "in", blist,
        "and", "date_time >=", sttime)
    }
    if(!is.null(start) & !is.null(end)){
      Query = paste(
        "select", "device_info_serial, date_time, latitude, longitude, altitude, pressure, temperature, satellites_used, gps_fixtime, positiondop, h_accuracy, v_accuracy, x_speed, y_speed, z_speed, speed_accuracy, location, userflag, speed_3d, speed_2d, direction, altitude_agl",
        "from", "gps.ee_tracking_speed_limited", "where",
        "device_info_serial", "in", blist,
        "and",
        "date_time >=", sttime,
        "and",
        "date_time <=", entime)
    }

    # could do bird by bird for specific dates then.....with multiple sql calls?
    data <- RODBC::sqlQuery(db, query = Query
                            ,as.is=c(FALSE, TRUE, rep(FALSE,20)) # constrain to readin datetime as character WITHOUT forcing to a TZ!
    )

    if(dim(data)[1] == 0){
      message(paste0("No data found for TagID ",TagID))
      data = NULL
    } else{

      # then have to align Movetech and UvA in the same way as previous function
      data$DateTime <- as.POSIXct(data$date_time,format = "%Y-%m-%d %H:%M:%S",tz="UTC") # convert date-time to correct format - here using "UTC" e.g. if through the year analysis
      names(data)[which(names(data) == "device_info_serial")] <- "TagID"
      names(data)[which(names(data) == "positiondop")] <- "gps.pdop"
      data$gps.hdop <- NA   # in Movetech but not UvA
      data <- data[!is.na(data$latitude),]
      data <- data[!is.na(data$longitude),]

      ############# PRESSURE DATA (IF AVAILABLE)
      if(pressure == TRUE){
        # check and merge into pressure column

        if(is.null(start) & is.null(end)){ # no start and end given
          Query = paste(
            "select", "device_info_serial, date_time, id, pressure_avg, temp_avg",
            "from", "gps.ee_pressure_burst_limited", "where",
            "device_info_serial", "in", blist)
        }
        if(is.null(start) & !is.null(end)){ # end time selected but no start
          Query = paste(
            "select", "device_info_serial, date_time, id, pressure_avg, temp_avg",
            "from", "gps.ee_pressure_burst_limited", "where",
            "device_info_serial", "in", blist,
            "and","date_time <=", entime)
        }
        if(!is.null(start) & is.null(end)){ # start time given but no end
          Query = paste(
            "select", "device_info_serial, date_time, id, pressure_avg, temp_avg",
            "from", "gps.ee_pressure_burst_limited", "where",
            "device_info_serial", "in", blist,
            "and", "date_time >=", sttime)
        }
        if(!is.null(start) & !is.null(end)){
          Query = paste(
            "select", "device_info_serial, date_time, id, pressure_avg, temp_avg",
            "from", "gps.ee_pressure_burst_limited", "where",
            "device_info_serial", "in", blist,
            "and",
            "date_time >=", sttime,
            "and",
            "date_time <=", entime)
        }

        # could do bird by bird for specific dates then.....with multiple sql calls?
        data_pr <- RODBC::sqlQuery(db, query = Query
                                  ,as.is=c(FALSE, TRUE, FALSE, FALSE, FALSE) # constrain to readin datetime as character WITHOUT forcing to a TZ!
        )

        if(dim(data_pr)[1] == 0){warning(paste0("No pressure data found for Tag ",unique(data$TagID), " so skipping pressure merge"))
        } else{
          names(data_pr)[which(names(data_pr) == "date_time")] <- "DateTime"
          data_pr$DateTime <- as.POSIXct(data_pr$DateTime,format = "%Y-%m-%d %H:%M:%S",tz="UTC") # convert date-time to correct format - here using "UTC" e.g. if through the year analysis
          data_pr <- data_pr[order(data_pr$DateTime),]
          data_pr <- subset(data_pr,select=c(DateTime,pressure_avg,temp_avg))

          # merge in the pressure data to the GPS - ASSUMES THAT THE TWO ARE SYNCHED
          data. <- merge(data,data_pr,by="DateTime",all=TRUE)

          data.$pressure <- data.$pressure_avg
          data.$temperature <- data.$temp_avg
          data. <- subset(data.,select=c(-pressure_avg,-temp_avg))
          data <- data.
        }

      }

      # project.points (old sp method)
      #bb <- data.frame(Latitude = data$latitude, Longitude = data$longitude)
      #sp::coordinates(bb) <- c("Longitude", "Latitude")
      #sp::proj4string(bb) <- sp::CRS("+proj=longlat +ellps=WGS84")
      #bb <- sp::spTransform(bb, p4sin)
      #bb2 <- as.data.frame(bb)
      #data$X <- bb2$Longitude
      #data$Y <- bb2$Latitude

      # new sf updated
      data_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
      data_sf <- sf::st_transform(data_sf, p4sin)
      coords = sf::st_coordinates(data_sf)
      data$X <- coords[,1]
      data$Y <- coords[,2]
      data <- data[!duplicated(data$DateTime),]

      # TRAJ SPEED AND dist/dt
      dataXa <- adehabitatLT::as.ltraj(subset(data,select=c(X,Y)),data$DateTime,id=1)
      dataXa  <- dataXa[[1]]
      data$dist <- dataXa$dist
      data$dt <- dataXa$dt
      data$ground.speed.MT <- NA
      data$traj.speed <- data$dist / data$dt # in m/s

      if(dropsat == TRUE){
        data <- data[data$satellites_used > dropsats,]
      }

      data$Type <- "UvA"
      data$flt_switch <- NA

      # additional columns added - need to perhaps have a merge of these if the use wishes
      # from read_voltage_UvA but the UvA voltages are collected at different rate not
      # synched with the GPS line by line as is for the MB data presentation
      # so adding these NA columns for now
      data$tag_voltage <- NA
      data$battery_charging_current <- NA

      # reduce dataframe to a more manageable number of columns
      data <- subset(data,select = c(Type,TagID,DateTime,speed_2d,speed_3d,traj.speed,latitude,longitude,X,Y,dist,dt,satellites_used,gps_fixtime,gps.pdop,gps.hdop,altitude,altitude_agl,h_accuracy,v_accuracy,pressure,temperature,x_speed,y_speed,z_speed,speed_accuracy,direction,location,flt_switch,tag_voltage,battery_charging_current))
      data <- data[!is.na(data$latitude),]
      data <- data[!is.na(data$DateTime),]

      if(!is.null(data)){
        if(dim(data)[1] < mindata){
          data <- NULL
          warning(paste0("To little or no data for ", TagID, " so excluded"))
        } # if less than X data points set data to NULL and skip it
      }


    }

    #data <- data.table::as.data.table(data)
    return(data)

  }


  # if you have NULL start or ends, then coerce the .track_prep function to accept through mapply
  if(is.null(start)){start <- rep("NULL",length(TagID))}
  if(is.null(end)){end <- rep("NULL",length(TagID))}


  # stacked is a great format!! You could potentialyl even retain this in future versions
  stacked <- mapply(.track_prep, TagID=TagID, start=start, end=end)

  # there is a bug I think in this as sometimes the mapply
  # returns list format, other time not....
  # quickly fixed here but needs more testing! Not sure why
  # two years later this now has a bug!

  if(is.null(ncol(stacked))){

    if(inherits(stacked, "list")){
      stacked[sapply(stacked, is.null)] <- NULL # removes any null data
      stacked <- simplify2array(stacked, higher = (TRUE == "array")) # puts in matrix array class format
    } else if(is.null(stacked)){stop("No data extracted")}
  }

  if(!is.null(ncol(stacked))){
    if(ncol(stacked) > 1){
      stacked[sapply(stacked, is.null)] <- NULL # removes any null data
      stacked <- simplify2array(stacked, higher = (TRUE == "array")) # puts in matrix array class format
    }
  } else if(is.null(stacked)){stop("No data extracted")}


  # ORIGINAL CODE:
  #if(ncol(stacked) > 1){
  #  # if bird's return NULL data, then need to comvert to matrix array style of the lists as from mapply ordinarily
  #  stacked[sapply(stacked, is.null)] <- NULL # removes any null data
  #  stacked <- simplify2array(stacked, higher = (TRUE == "array")) # puts in matrix array class format
  #} else if(is.null(stacked)){stop("No data extracted")}

  # need to also drop out the offending Tag with no data if there is one,
  # from the original TagID fed from e.g. a control file
  # and this retains the correct order as was in "stacked"
  TagID = unique(unlist(stacked[2,]))

  if(verbose == TRUE){message("Building final TrackStack...")}

  # if just one animal, assign Track class and be done with read in
  if(ncol(stacked) == 1){
    dat = Track(as.data.frame(stacked[,1]))
  }

  # multiple animals list together and make TrackStack class as well as Track for individual data.frames
  if(ncol(stacked) > 1){

    # stacking the data read in
    dat <- list()
    for(b in 1:ncol(stacked)){
      dat[[b]] <- Track(as.data.frame(stacked[,b])) # assign Track class to each dataframe
    }
    names(dat) <- TagID # name data frames by TagID
    dat = TrackStack(dat)

  }
  if(verbose == TRUE){message("...OK")}

  if(pressure == TRUE){message("Note that pressure data are assumed to have been synched to the GPS (closest DateTime matches not yet coded)")}


  return(dat)

} # close function




