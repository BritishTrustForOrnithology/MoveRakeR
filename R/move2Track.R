#' Convert a downloaded Movebank .csv to a Track format
#'
#' @description
#' A function to convert a downloaded .csv file from Movebank to a \code{Track-family} object.
#'
#' @details
#' This function replicates code used within the \code{read_track_MB} and allows conversion of \code{move}
#' objects from Movebank accessed through \code{move}, through to a Track class within \code{MoveRakeR}.
#'
#' @param data The input .csv downloaded from Movebank in standard format.
#' @param TagID The individual tag identifier from the Movebank .csv, defaults to NULL
#' thus retaining all data from the .csv.
#' @param start The start time to delineate subsets of the total data for the individual
#' in UTC.
#' @param end The end time to delineate subsets of the total data for the individual in UTC.
#' The start and end times are ideally given in the format "2014-06-25 15:20:23" but the
#' internal MoveRakeR function \code{datetime} is used to accept "/" and dd/mm/YY two, or
#' four length years (YY, YYYY) as well.
#' #' Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
#' of the animal and must be given in' \emph{character()} format, which are then converted to
#' \code{base::POSIXct} format within the \code{read_track_MB} function. Functionality is also
#' available for situations where only one or other of start and end need to be specified,
#' with the the other defaulting to NULL. Multiple periods per TagID are also permitted.
#' @param dropsat Logical defaults to FALSE. This indicates whether to drop GPS fixes with satellites below
#' the \emph{dropsats} threshold. It is suggested the user do not change these
#' from FALSE but instead use the function \code{\link{clean_GPS}}.
#' @param dropsats Numeric value indicating a threshold for satellite retention.
#' @param flt_switch logical argument, set to FALSE whether to retain
#' Movebank data erroneous flt_switch values, instead being dealt with in the
#' \code{\link{clean_GPS}} function.
#' @param mindata This specifies a minimum number of rows of data for the individual
#' to be retained and is a form of cleaning, but here is retained to exclude reading
#' in data from animals with too few data deemed unusable for analysis.
#' Defaults to mindata = 5. It can of course be set to zero if a completely raw dataset.
#' @param p4s The coordinate reference system number defaulting to epsg:3035.
#' The lat-longs extracted from the Movebank database will be projected via this CRS and stored in two new columns
#' \emph{X} and \emph{Y} (note capitilisation). Note however, that further functions
#' give options to change this if you wish in due course, and the user of course do all point-level manipulation
#' outside the functions if they so wish anyway.
#' @param verbose logical (default TRUE) for whether detailed messaging is required.
#'
#' @return As with \code{read_Track_MB}, the final data outputted for individual animals is a \code{Track and data.frame} object of 29 variables for each
#' TagID; Type, TagID, DateTime, speed_2d, speed_3d, traj.speed, latitude, longitude,
#' X, Y, dist, dt, satellites_used, gps_fixtime, gps.pdop, gps.hdop, altitude,
#' altitude_agl, h_accuracy, v_accuracy, pressure, temperature, x_speed, y_speed,
#' z_speed, speed_accuracy, direction, location and flt_switch. See \code{read_Track_MB} for more
#' details.
#'
#' @examples
#'
#' ### NOT RUN
#'
#' # Data downloaded from Movebank
#' in_dir <- "dir.path/"
#' data <- read.csv(paste0(in_dir,"data.csv"))
#'
#' # Coerce one animal with two separate time sections to Track format
#' Trackdat <- Movebank2Track(data=data,
#'   start = c("2019-06-01 15:00:00","2020-02-01 01:00:00"),
#'   end = c("2019-09-30 23:59:59","2020-09-30 23:59:59"),
#'   TagID = c('1','2'))
#'
#' # Or just one bird from the file
#' Trackdat <- Movebank2Track(data=data, TagID = c('1')) # replace with real TagIDs
#'
#' # Or lots
#' Trackdat <- Movebank2Track(data=data,
#'   start = c("2019-05-20 15:30:20","2019-05-20 17:55:10","2020-05-07 16:03:45"),
#'   end = c("2019-09-30 23:59:59","2019-09-30 23:59:59","2020-05-11 09:51:10"),
#'   TagID = c('1','2','3'))
#'
#' @export
move2Track <- function(data, TagID=NULL,start=NULL,end=NULL,TagID_drop=TRUE,
                       dropsat = FALSE, dropsats=3, flt_switch=TRUE, mindata = 5,
                       p4s = 3035,
                       verbose = TRUE){
  
  
  # identify animals
  anims <- unique(data$individual.local.identifier)
  
  ###########################
  # also checking for extra column of algorithm.marked.outlier that occurs in some
  first_data <- list()
  i <- 25
  for(i in 1:length(anims)){
    
    first_data[[i]] <- data[data$individual.local.identifier == anims[[i]],]
    if(exists("algorithm.marked.outlier",first_data[[i]])){
      #first_data[[i]] <- first_data[[i]][first_data[[i]]$algorithm.marked.outlier == "",]
      first_data[[i]] <- subset(first_data[[i]],select = -algorithm.marked.outlier)
    }
  }
  
  ###########################
  # tid naming
  TagID2 <- unique(TagID)
  
  if(!is.null(TagID)){
    #### ADJUST THE TAG LOCAL ID TO THAT FED INTO THE FUNCTION (TagID column is made in .finalise_tracks)
    i <- 5
    for(i in 1:length(first_data)){
      tid = unique(first_data[[i]]$individual.local.identifier)
      
      #j = 1
      aa <- list()
      for(j in 1:length(tid)){
        aa[[j]] <- first_data[[i]][first_data[[i]]$individual.local.identifier == tid[j],]
        
        # lookup the TagID supplied and replace local identifier in MB file
        mm <- list()
        for(k in 1:length(TagID2)){
          mm[[k]] <- grep(TagID2[k],tid[j], value = FALSE)
          if(length(mm[[k]]) == 1)  mm[[k]] <- TRUE else  mm[[k]] <- FALSE
        }
        if(mm[[k]] != FALSE){
          aa[[j]]$individual.local.identifier <- TagID2[unlist(mm)]
        }
        
        
        
        
      }
      
      first_data[[i]] <- do.call('rbind', aa)
      
    }
  } else{
    tid = anims
  }
  
  
  if(!is.null(TagID)){
    if(TagID_drop == TRUE){
      # drop out anything NOT named in TagID
      first_data <- lapply(first_data, function(x){x[x$individual.local.identifier %in% TagID2,]})
      t1 <- lapply(first_data, function(x){nrow(x) > 0})
      
      first_data <- first_data[which(t1==TRUE)]
      
    }
    
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
            
            #### cycling through first_data[[i]] i.e. stemming from potential multiple repos above
            
            # make a new list in case multiple tagIDs are requested st/ends
            second_data <- list() #for(u in 1:length(first_data)){second_data[[u]] <- NA}
            i <- 1
            for(i in 1:length(first_data)){
              
              ### CASE 2 starts and ENDS == 1, meaning same across birds (using datetime function allowing different formats of datetimes in simple fashion)
              if(length(start) == 1 & length(end) == 1){
                if(length(TagID) > 1){
                  if(verbose == TRUE){message(paste0("Taking a single start and end time for all TagIDs"))}
                } else{if(verbose == TRUE){message("Using tag-specific start and end times")}}
                second_data[[i]] <- first_data[[i]][first_data[[i]]$timestamp >= datetime(start, auto=TRUE),]
                second_data[[i]] <- second_data[[i]][second_data[[i]]$timestamp <= datetime(end, auto=TRUE),]
                
              }
              
              # note all boundary conditions treated as inclusive
              # also the user may have incorrectly specified tagid numbers (dealt with above)
              # but needs the routine to select the right start and end from lookup
              
              
              ### CASE 3 (note from above error catch it's not possible at this point for starts and ends not to be balanced AND match the TagID length)
              lookup_df <- NULL
              
              if(!is.null(start) & length(start) > 1 & is.null(end)){
                if(length(start) != length(TagID)){
                  stop("start length not equal to TagID")
                }
                
                # then have start vector length of TagID
                lookup_df <- data.frame(TagID,start, end = NA)
                
                
              }
              if(!is.null(end) & length(end) > 1 & is.null(start)){
                if(length(end) != length(TagID)){
                  stop("end length not equal to TagID")
                }
                
                # then have start vector length of TagID
                lookup_df <- data.frame(TagID,start=NA,end)
                
              }
              if(!is.null(end) & !is.null(start) & length(end) > 1 & length(start) > 1){
                if(length(end) != length(TagID)){
                  stop("end length not equal to TagID")
                }
                if(length(start) != length(TagID)){
                  stop("start length not equal to TagID")
                }
                # then have start vector length of TagID
                lookup_df <- data.frame(TagID,start,end)
                
              }
              
              # if any NULLs are specified, which have to be character to make the c() work and be balanced,
              # then suggest easier to replace those with the min or max of that bird's data
              # but then you won't kniow that be cycling through [[i]] of first_data if animal not there
              
              
              ### START, END = NULL
              if(!is.null(lookup_df)){
                
                for(j in 1:nrow(lookup_df)){
                  if(!any(is.na(lookup_df[j,]))){
                    if(any(datetime(lookup_df[j,]$end) < datetime(lookup_df[j,]$start))){
                      stop("End datetimes for one or more TagIDs are less than start")
                    }
                  }
                }
                
                
                if(verbose == TRUE){
                  if(i == 1){message("Using tag-specific start and/or end times")}
                }
                
                tid = unique(first_data[[i]]$individual.local.identifier)
                
                # best to cycle through the lookup_df NOT the birds in the first_data[[i]]??
                # get which TagIDs match the lookup first
                # subset the lookup for TIDs from the repo in which found
                lookup_df_ <- lookup_df[lookup_df$TagID %in% tid,]
                
                j <- 1
                sublist <- list()
                for(j in 1:nrow(lookup_df_)){
                  
                  #print(j)
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
              
              
              # otherwise OPTION 1, you have NULLs for starts and ends, so second_data will still be a list
              if(is.null(start) & is.null(end)){
                second_data <- first_data
              }
              # NOTE in the above "NULL" is needed as character OR just NA
              
            }
            
          }
  # then at this point you have the second_data list, again 1 and 2 from the different repos
  # BUT then each TagID is subsetted for datetimes as a further list level AND...in the
  # second level you can have MULTIPLE tagIDs for periods selected
  
  # then the next job is to apply the finalise tracks .finalise_tracks above.
  
  # then all else below can be dropped!!
  p4sin <- p4s
  
  #data <- third_data[[1]][[1]]
  #data <- third_data[[1]][[2]]
  
  # internal function to finalise the tracks and get in standardised format for analysis
  data = third_data[[i]][[1]]
  
  .finalise_tracks <- function(data){
    
    # proceed with normal processing
    data$DateTime <- as.POSIXct(data$timestamp,format = "%Y-%m-%d %H:%M:%S",tz="UTC") # convert date-time to correct format - here using "UTC" e.g. if through the year analysis
    
    #### continue further processing
    names(data)[which(names(data) == "individual.local.identifier")] <- "TagID"
    names(data)[which(names(data) == "location.long")] <- "longitude"
    names(data)[which(names(data) == "location.lat")] <- "latitude"
    names(data)[which(names(data) == "height.above.ellipsoid")] <- "altitude"
    names(data)[which(names(data) == "ground.speed")] <- "ground.speed.MT"
    names(data)[which(names(data) == "gps.satellite.count")] <- "satellites_used"
    names(data)[which(names(data) == "event.id")] <- "location"
    
    
    data$gps.pdop <- NA   # in UvA but not Movetech
    data$altitude_agl <- NA
    
    data <- data[!is.na(data$latitude),]
    data <- data[!is.na(data$longitude),]
    
    # project_points
    bb <- data.frame(Latitude = data$latitude, Longitude = data$longitude)
    
    ## old sp:
    #sp::coordinates(bb) <- c("Longitude", "Latitude")
    #sp::proj4string(bb) <- sp::CRS("+proj=longlat +ellps=WGS84")
    #bb <- sp::spTransform(bb, p4sin)
    #bb2 <- as.data.frame(bb)
    #data$X <- bb2$Longitude
    #data$Y <- bb2$Latitude
    
    ## new sf:
    bb <- sf::st_as_sf(bb, coords = c("Longitude", "Latitude"), crs = 4326)
    bb <- sf::st_transform(bb, p4sin)
    bb_df <- as.data.frame(sf::st_coordinates(bb))
    data$X <- bb_df$X
    data$Y <- bb_df$Y
    
    data <- data[!duplicated(data$DateTime),]
    
    # TRAJ SPEED AND dist/dt
    dataXa <- adehabitatLT::as.ltraj(subset(data,select=c(X,Y)),data$DateTime,id=1)
    dataXa  <- dataXa[[1]]
    data$dist <- dataXa$dist
    data$dt <- dataXa$dt
    data$traj.speed <- data$dist / data$dt # in m/s
    data$speed_2d <- data$ground.speed
    data$speed_3d <- NA
    data$h_accuracy <- data$location.error.numerical # assuming this is the same as h_accuracy reported by UvA
    data$v_accuracy <- data$vertical.error.numerical # likewise as above for v_accuracy
    data$gps_fixtime <- NA
    
    if(dropsat == TRUE){
      # drop data with less or equal to than X satellites
      data <- data[data$satellites_used > dropsats,]
    }
    
    if(flt_switch == TRUE){
      
      if(exists("flt.switch", data)){
        ##### switch != 0 removed - I think this was covered anyway but making more explicit here
        data$flt_switch <- ifelse(is.na(data$flt.switch),-99,data$flt.switch)
        data$flt_switch <- ifelse(data$flt_switch != 0,-99,data$flt_switch)
        data <- data[data$flt_switch != -99,]
      } else{
        warning("No column called flt.switch found")
      }
    
    }
    
    if(dim(data)[1] > 0){ # by removing duff data from MB via flt_switch can reduce some tag data to none
      data$Type <- "MoveBank"
      
      ### additionals for a fuller file with other gubbins - the UvA one was stripped back too much - e.g. need direction column
      data$temperature <- data$external.temperature
      data$pressure <- NA # don't think there are any pressure measurements in the MY data yet
      data$x_speed <- NA
      data$y_speed <- NA
      data$z_speed <- NA
      data$speed_accuracy <- NA
      data$direction <- data$heading
      
      # reduce dataframe to a more manageable number of columns
      #data <- subset(data,select = c(Type,TagID,DateTime,speed_2d,speed_3d,traj.speed,latitude,longitude,X,Y,dist,dt,satellites_used,gps_fixtime,gps.pdop,gps.hdop,altitude,altitude_agl,h_accuracy,v_accuracy,pressure,temperature,x_speed,y_speed,z_speed,speed_accuracy,direction,location,flt_switch))
      
      data <- data[!is.na(data$latitude),]
      data <- data[!duplicated(data$DateTime),]
      
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
      return(Track(data))
    }
    
  }
  
  # this needs to be cycled OVER the second_data[[1]][[1]] structure of lists
  # if you ONLY ran one start or end across all birds OR did no star/end corrections
  # then you have a TrackStack style list of MULTIPLE birds
  # break that out into a TMS
  
  if(!is.list(second_data[[1]][[1]])){
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
  
  
  if(is.list(second_data[[1]][[1]])){
    # then we have a list in a list, TMS type
    third_data <- second_data
    
    # but...naming, if you have multiple extractions of a bird for DIFFERENT dates, then
    # this needs fixing
    for(i in 1:length(third_data)){
      nms <- as.vector(do.call('rbind',lapply(third_data[[i]], function(x){unique(x$individual.local.identifier)}))[,1])
      # rename with a _1, _2, _3 etc for each duplicate TagID
      nms <- make.unique(as.character(nms), sep = '_')
      names(third_data[[i]]) <- nms
      
      #### need a way of making the name_1 transfer through to the data.frame
      for(k in 1:length(third_data[[i]])){
        third_data[[i]][[k]]$individual.local.identifier <- nms[k]
      }
      
    }
    
  }
  
  
  
  ####
  if(verbose){message("Building final TrackStack")}
  stdata = list()
  i <- 1
  for(i in 1:length(third_data)){
    
    stdata[[i]] <- lapply(third_data[[i]],.finalise_tracks)
    
    t1 <- lapply(stdata[[i]], function(x) if(!is.null(nrow(x))){x} else NULL)
    stdata[[i]] <- t1[!sapply(t1,is.null)]
    
    if(length(stdata[[i]]) == 0){
      if(verbose){message(paste0("Function dropped out all data for stack ", i))}
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
  
  if(length(stdata) > 1){
    outdata <- TrackMultiStack(stdata)
    outdata <- TrackMultiStack2TrackStack(outdata, rename = FALSE)
  } else{
    outdata <- stdata[[1]]
  }
  
  return(outdata)
  
} # close function




