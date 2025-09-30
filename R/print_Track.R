#' Print S3 method for \code{Track} family objects
#'
#' @description
#' A generic printing method for objects of class \code{Track}, \code{TrackStack} and \code{TrackMultiStack}.
#'
#' @details
#' Prints a simplified view of the data and a quick summary of the data structures in the R console terminal window,
#' following a combination of \code{data.table} and \code{tidyverse} implementations.
#' \code{print.Track} looks for columns "TagID","DateTime", "longitude", "latitude", for core printing
#' and then will display other columns of satellites_used and altitude, for now, although this
#' may change and expect flexibility to be increased. For \code{TrackStack} and \code{TrackMultiStack} objects,
#' the displayed data is given (top and tail) for the first Track object in the dataset.
#'
#' Print options are also available under "options" as specified by functions: \code{\link{reset_options_mR}},
#' \code{\link{get_options_mR}} and \code{\link{set_options_mR}} to control the view of the printed output.
#' See help files for those functions.
#'
#' Dev note: not yet coded flexibility should the first dataset not actually have enough data to print. Plus,
#' not yet coded warnings for differences in TS or TMS objects if there are differing numbers of columns
#' in list sub-datasets.
#'
#' @param x An object of class \code{Track}, \code{TrackStack}, or \code{TrackMultiStack}.
#' @param len The desired length of the top and tail of the data to show, defaults to len=6 (integer value)
#' rows of \code{utils::head()} and \code{utils::tail()} respectively.
#'
#' @return
#' No data are converted, only an on-screen "pretty" view is given.
#'
#' @seealso [MoveRakeR::get_options_mR], [MoveRakeR::set_options_mR], [MoveRakeR::reset_options_mR]
#'
#' @examples
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' print(data)
#'
#' @export
#' @method print Track
print.Track <- function(x, len = 6, ...){

    colfind <- c("TagID", "DateTime", "longitude", "latitude")

    # get user-defined printing options
    pm <- attr(get_options_mR(),"print_method")
    pt <- attr(get_options_mR(),"print_type")

    # if any of those columns are NOT found in first data.frame of Track object (TS, TMS), or in
    # data.frame of Track, then switch to data.frame printing, rather than just fall over
    if(is_Track(x)){
      # data.frame
      if(any(!colfind %in% names(x))){
        # then set to a data.frame print method, rather than just fall over
        set_options_mR(Print_method = "data.frame", Print_type = "all")
      }
    }

    if(is_TrackStack(x)){
      # list
      if(any(!colfind %in% names(x[[1]]))){
        set_options_mR(Print_method = "data.frame", Print_type = "all")
      }
    }

    if(is_TrackMultiStack(x)){
      # list in a list
      if(any(!colfind %in% names(x[[1]][[1]]))){
        set_options_mR(Print_method = "data.frame", Print_type = "all")
      }
    }

    #if(is(x, "Track")){
    #  set_options_mR(Print_method = pm, Print_type = pt)
    #}
    #get_options_mR()


    warn = FALSE # initialise warning logical for unequal column check

    .risub <- function(x,n){substring(x,nchar(x)-n+1)}

    # assess global printing options
    print_style <- get_options$print_method
    print_type <- get_options$print_type

    #######################
    # for summary of TS and TMS objects, get the first data set of an animal
    # and use that to show example data format, but need to report if any
    # no. columns do not match across TS and TMS objects

    if(print_style == "data.frame"){

      if(is_Track(x)){
        if(print_type == "subset"){
          print(head(data.frame(x), n = len))
        } else if(print_type == "all"){print(data.frame(x))}
      }

      ################### ran out of time - find out why the above is not printing.....

      if(is_TrackStack(x)){
        xx <- list()
        if(print_type == "subset"){
          for(i in 1:length(x)){
            xx[[i]] <- head(data.frame(x[i]), n = len)
          }
        }
        else if(print_type == "all"){
          for(i in 1:length(x)){
            xx[[i]] <- print(data.frame(x[i]))
          }
        }
        print(xx)
      }

      if(is_TrackMultiStack(x)){
        xx <- list()
        if(print_type == "subset"){
          for(i in 1:length(x)){
            xx[[i]] <- list()
            for(j in 1:length(x[[i]])){
            xx[[i]][[j]] <- head(data.frame(x[[i]][j]), n = len)
            }
          }
        }
        else if(print_type == "all"){
          for(i in 1:length(x)){
            xx[[i]] <- list()
            for(j in 1:length(x[[i]])){
              xx[[i]][[j]] <- print(data.frame(x[[i]][j]))
            }
          }
        }
        print(xx)
      }

    } else if(print_style == "Track"){


    if(is_TrackStack(x)){
      take_this <- 1
      data <- x[[1]]
      Class <- "TrackStack"
      gtid <- get_TagIDs(x, verbose = FALSE)
      N <- length(gtid)

      # assess if col lengths in each stack are the same, and store a wornign if not
      #diff_col <- FALSE # initialise this logical, will get made TRUE if column lengths are different amont the Track elements
      coll <- unlist(lapply(x, ncol))
      ucoll <- unique(coll)

      if(length(ucoll) != 1){
        warn <- TRUE

        dd <- data.frame(table(coll))
        dd <- dd[order(dd$Freq, decreasing = TRUE),]
        i <- 1
        ll <- list()
        for(i in 1:length(dd)){
          if(dd[i,]$Freq > 1){s <- "s"} else{s = NULL}
          ll[[i]] <- paste0(dd[i,]$Freq, " Track",s, ": ", dd[i,]$coll, " columns")
        }
        message <- paste0("*Unbalanced object (", paste(unlist(ll), collapse = ", "), ")")
      }

      # if the first data has too few data to show on screen, take another bird as example
      if(nrow(data) < 2*len){
        LEN <- 0

        while(LEN == 0){
           cc <- unlist(lapply(x,nrow))
           take_this <- as.vector(which(cc == max(cc)))
           data <- x[[take_this]]
           if(nrow(data) > len*2){
              data <- x[[take_this]]
              LEN = 1
              break
            }
        }

      }

    } else
    if(is_TrackMultiStack(x)){

        take_this <- 1; ST <- 1
        data <- x[[1]][[1]]
        Class <- "TrackMultiStack"
        N_Stacks <- length(x)
        N <- length(get_TagIDs(x, verbose = FALSE))

        # assess if col lengths in each stack are the same, and store a wornign if not
        #diff_col <- FALSE # initialise this logical, will get made TRUE if column lengths are different amont the Track elements
        coll <- NULL
        for(nn in 1:N_Stacks){
          coll <- c(coll,unlist(lapply(x[[nn]], ncol)))
        }
        ucoll <- unique(coll)

        if(length(ucoll) != 1){
          warn <- TRUE

          dd <- data.frame(table(coll))
          dd <- dd[order(dd$Freq, decreasing = TRUE),]
          i <- 1
          ll <- list()
          for(i in 1:length(dd)){
            if(dd[i,]$Freq > 1){s <- "s"} else{s = NULL}
            ll[[i]] <- paste0(dd[i,]$Freq, " Track",s, ": ", dd[i,]$coll, " columns")
          }
          message <- paste0("*Unbalanced object (", paste(unlist(ll), collapse = ", "), ")")
        }

        # if the first data has too few data to show on screen, take another bird as example
        if(nrow(data) < 2*len){
          LEN <- 0

          while(LEN == 0){

            nn <- 1
            for(nn in 1:N_Stacks){

              print(nn)
              cc <- unlist(lapply(x[[nn]],nrow))
              take_this <- as.vector(which(cc == max(cc)))
              data <- x[[nn]][[take_this]]
              if(nrow(data) > len*2){
                data <- x[[nn]][[take_this]]
                ST <- nn
                LEN = 1
                break
              }
            }

          }

        }

      # animals per stack
      gtid <- list()
      for(i in 1:N_Stacks){
        gtid[[i]] <- get_TagIDs(x[[i]], verbose = FALSE)
      }
    } else
    if(is_Track(x)){
        data <- x
        Class <- "Track"
        gtid <- get_TagIDs(x, verbose = FALSE)
        N <- length(gtid)
        X <- nrow(data)
        Y <- ncol(data)
      }

    #######################
    # EXAMPLE DATA PRINT
    # look for columns

    w <- getOption("width")

    if(Class == "Track"){lcol <- 36}
    if(Class == "TrackStack"){lcol <- 33}
    if(Class == "TrackMultiStack"){lcol <- 31}

    cat(paste0("\033[1;", lcol, "m",paste(rep("—",w),collapse=""), "\033[0m","\n"))


    ################ Track, data[[1]] and data[[1]][[1]] examples

    if(nrow(data) >= 2*len){
      toprint = data.frame(data.table::rbindlist(list(head(data,len), tail(data,len)), use.names = FALSE))
      rn = c(seq_len(len), seq.int(to = nrow(data), length.out = len))
      rownames(toprint) = paste0(format(rn, right = TRUE, scientific = FALSE), ":")
      dots = TRUE
    } else{
      toprint = data
      rn <- 1:nrow(toprint)
      rownames(toprint) = paste0(format(rn, right = TRUE, scientific = FALSE), ":")
      dots = FALSE
    }

    ############### If the option "subset" is chosen, then subset view wanted:
    ## i.e. subset referring to the column restricted view, "all" is for top-tailed Track-style view but with ALL columns

    if(print_type == "subset"){
      ##### subset original toprint down after top and tailing
      sub1 <- subset(toprint,select = c("TagID", "DateTime"))
      sub1$latlong <- paste0("(", format(round(toprint$latitude,3), nsmall = 3), ", ",  format(round(toprint$longitude,3), nsmall = 3), ")")

      # check for other columns:
      colnm <- tolower(names(toprint))

      s1 <- which(grepl("sat", colnm))
      sub1 <- cbind(sub1, toprint[s1])

      a1 <- which(grepl("alt", colnm))
      sub1 <- cbind(sub1, toprint[a1])

      # remember these columns for printing a list of those not displayed:
      cols_not_printed <- colnames(toprint)[!colnames(toprint) %in% colnames(sub1)]

      if("latlong" %in% colnames(sub1)){
        cols_not_printed <- cols_not_printed[!cols_not_printed %in% c("longitude","latitude")]
      }
    } else if(print_type == "all"){
      sub1 <- toprint
    }



    ################ get the type of data

    i <- 1
    typ <- list()
    for(i in 1:ncol(sub1)){
      typ[[i]] <- typeof(sub1[,i])

      if(inherits(sub1[,i],"POSIXct")){
        typ[[i]] <- "<dttm>"
      }
      if(typ[[i]] == "character"){typ[[i]] <- "<chr>"}
      if(typ[[i]] == "integer"){typ[[i]] <- "<int>"}
      if(typ[[i]] == "double"){typ[[i]] <- "<dbl>"}
      if(typ[[i]] == "factor"){typ[[i]] <- "<fct>"}
      else if(typ[[i]] != "<chr>" &
              typ[[i]] != "<int>" &
              typ[[i]] != "<dbl>" &
              typ[[i]] != "<fct>" &
              typ[[i]] != "<dttm>"
      ){
        typ[[i]] <- paste0("<",substr(typ[[i]],1,3),">")
      }

    }

    types <- do.call('rbind', typ)[,1]
    #typescol <- paste0("\033[0;", 32, "m", types, "\033[0m","\n")


    ##### Initial summary of TMS objects

    if(Class == "TrackMultiStack"){
      fbit <- paste(c("A TrackMultiStack <list><list> of ", N_Stacks, " TrackStack objects:"), collapse = "")

      get_stack_detail <- function(s){

        ll <- paste0("<list> [[",s,"]]:")
        sbit <- paste0(ll," ", length(get_TagIDs(x[[s]], verbose = FALSE)), " Track objects ")
        sbit <- paste0("\033[1;", 32, "m", sbit, "\033[0m")
        rws <- unlist(lapply(x[[s]],nrow))

        # reassess if ncols unequal! This bit could be improved
        coll <- unlist(lapply(x[[s]], ncol))
        ucoll <- unique(coll)

        if(length(ucoll) > 1) {
          tbit <- paste0("(",min(rws), " |" , median(rws),  "| " , max(rws), " rows [min |med| max], ",  paste(ucoll,collapse = "/"), " cols*")
        } else{
          tbit <- paste0("(",min(rws), " |" , median(rws),  "| " , max(rws), " rows [min |med| max], ", ncol(data), " cols)")
        }
        tbit <- paste0("\033[0;", 32, "m", tbit, "\033[0m", "\n")
        sbit2 <- paste0(sbit, tbit)
        #sbit2 <- paste0(substr(sbit,1,(nchar(sbit) - (nchar(sbit)-w)) - 8)," ...")

        return(sbit2)
      }

      get_tid_detail <- function(s){

        indent <- 12 # matching nchars paste0("<list> [[",s,"]]:") withiout the "s"
        #indent <- 5 # for including "TagIDs:" looks pants

        ggtid <- (paste(gtid[[s]], collapse = ", "))
        gap <- paste0(paste(rep(" ",indent+nchar(s)-1),collapse = ""))
        sbit <- paste0(gap,"  TagIDs: ", ggtid)

        if(nchar(sbit) >= w-8){
          sbit2 <- paste0(substr(sbit,1,(nchar(sbit) - (nchar(sbit)-w)) - 8)," ...")
        } else{
          sbit2 <- sbit
        }
        sbit2 <-  paste0("\033[0;", 31, "m", sbit2, "\033[0m", "\n")

      }


      header_text1 <- paste0("\033[1;", lcol, "m", fbit, "\033[0m", "\n")
      cat(header_text1)

      for(ss in 1:N_Stacks){
        header_text2 <- get_stack_detail(ss)
        header_text3 <- get_tid_detail(ss)

        cat(header_text2)
        cat(header_text3)

      }

      cat(paste0("\033[1;", lcol, "m",paste(rep("—",w),collapse=""), "\033[0m","\n"))
      #cat(paste0("\033[1;", lcol, "m",paste(" "), "\033[0m","\n"))

      if(take_this == 1 & ST == 1){
        cat(paste0("\033[0;", lcol, "m",paste("~~~~~ View of first Track object data[[1]][[1]] ~~~~~"), "\033[0m","\n"))
      } else{
        cat(paste0("\033[0;", lcol, "m",paste0("~~~~~ View of Track object no. ", take_this ," from data[[",ST,"]] ~~~~~"), "\033[0m","\n"))
      }


    }

    if(Class == "TrackStack"){

      fbit <- paste(c("A TrackStack <list> of ", N, " Track objects"),collapse = "")
      ggtid <- (paste(gtid, collapse = ", "))
      sbit <- paste0("TagIDs: ",  typ[[1]], " ", ggtid)

      if(nchar(sbit) >= w-8){
        sbit2 <- paste0(substr(sbit,1,(nchar(sbit) - (nchar(sbit)-w)) - 8)," ...")
      } else{
        sbit2 <- sbit
      }
      rws <- unlist(lapply(x,nrow))
      tbit <- paste0("Dims: ", min(rws), " |" , median(rws),  "| " , max(rws), " rows [min |med| max], ", ncol(data), " cols")

      if(warn == TRUE){
        tbit <- paste0("Dims: ", min(rws), " |" , median(rws),  "| " , max(rws), " rows [min |med| max], ", paste(ucoll,collapse = "/"), " cols*")
      }

      header_text1 <- paste0("\033[1;", lcol, "m", fbit, "\033[0m", "\n")
      header_text2 <- paste0("\033[0;", 32, "m", sbit2, "\033[0m", "\n")
      header_text3 <- paste0("\033[0;", 34, "m", tbit, "\033[0m", "\n")

      cat(header_text1)
      cat(header_text2)
      cat(header_text3)

      cat(paste0("\033[1;", lcol, "m",paste(rep("—",w),collapse=""), "\033[0m","\n"))
      #cat(paste0("\033[1;", lcol, "m",paste(" "), "\033[0m","\n"))


      if(take_this == 1){
        cat(paste0("\033[0;", lcol, "m",paste("~~~~~ View of first Track object data[[1]] ~~~~~"), "\033[0m","\n"))
      } else{
        cat(paste0("\033[0;", lcol, "m",paste0("~~~~~ View of Track object data[[",take_this,"]] ~~~~~"), "\033[0m","\n"))
      }


    }

    if(Class == "Track"){
      fbit = paste0("A Track object (", X, " rows, ", Y, " columns) containing ", N, " TagIDs:")
      ggtid <- (paste(gtid, collapse = ", "))
      sbit <- paste0("TagIDs: ",  typ[[1]], " ", ggtid)
      sbit2 <- paste0(substr(sbit,1,(nchar(sbit) - (nchar(sbit)-w)) - 8)," ...")

      header_text1 <- paste0("\033[1;", lcol, "m", fbit, "\033[0m", "\n")
      header_text2 <- paste0("\033[0;", 32, "m", sbit2, "\033[0m", "\n")

      cat(header_text1)
      cat(header_text2)

      cat(paste0("\033[1;", lcol, "m",paste(rep("—",w),collapse=""), "\033[0m","\n"))

    }


    sub1$TagID <- as.character(sub1$TagID)

    ############### adjust length of data columns being printed
    ##### ONLY TO BE DONE IF print_type == "subset"

    if(print_type == "subset"){
      # built in _stack_ from TMS to TS or to T:
      if(any(grepl("_stack_",sub1$TagID))){
        tid <- do.call('rbind',strsplit(sub1$TagID, "_"))[,1]
        if(any(nchar(tid) > 6)){
          tid <- paste0(substr(tid,1,6), "~")
        }
        stack <- do.call('rbind',strsplit(sub1$TagID, "_"))[,3]
        sub1$TagID <- paste0(tid, "_s",stack)
      }
      # otherwise check length of TagID and shorten ~. if > 6 chars
      if(!any(grepl("_stack_",sub1$TagID)) & any(nchar(as.character(sub1$TagID)) > 6)){
        sub1$TagID <- paste0(substr(sub1$TagID,1,6), "~.")
      }

      ############# shortern length of names of the df
      onm <- names(sub1)[-1:-3]

      if(any(nchar(onm) > 7)){
        #ww <- which(nchar(onm)> 7)
        #i <- 1

        onm2 <- list()
        for(i in 1:length(onm)){
          if(nchar(onm[i]) > 7){
            onm2[[i]] <- paste0(substr(onm[i],1,3),"~",.risub(onm[i],2))
          } else if(nchar(onm[i]) > 8){
            onm2[[i]] <- paste0(substr(onm[i],1,3),"~",.risub(onm[i],4))
          } else{
            onm2[[i]] <- onm[[i]]
          }

        }

        onm2a <- do.call('rbind',onm2)[,1]
        names(sub1)[which(names(sub1) %in% onm)] <- onm2a

      }
    }

    ############## rename back to before
    toprint <- sub1

    ############## <dbl> make 3dp printed
    dbs <- which(types == "<dbl>")

    # can't round a character variable....below issue.....take out TagID from dbs
    if(any(names(toprint)[dbs] == "TagID")){
      dbs_rm <- dbs[which(names(toprint)[dbs] == "TagID")]
      dbs <- dbs[!dbs %in% dbs_rm]
    }

    if(length(dbs) > 0){
      if(length(dbs) > 1){
        toprint[,dbs] <- apply(toprint[,dbs], MARGIN = c(1,2), function(x){round(x,3)})
      }
      else if(length(dbs) == 1){
        dbs_df = data.frame(x = round(toprint[,dbs],3))
        names(dbs_df) <-  names(toprint)[dbs]
        toprint[,dbs] <- dbs_df
      }
    }

    ############## reformat datetime to character (otherwise won't rbind)
    toprint$DateTime <- as.character(toprint$DateTime)
    #dashes = paste(rep("-",max(nchar(rownames(toprint)))),collapse="")

    if(dots){
      toprint = rbind(head(toprint, len), "- - -"="",tail(toprint, len))
    }

    name.width <- max(sapply(names(toprint), nchar))

    ### only format the layout if "subset" print type chosen
    if(print_type == "subset"){
      names(toprint) <- format(names(toprint), width = name.width, justify = "centre")
      toprint <- format(toprint, width = name.width, justify = "centre")

    }

    ############## add types
    ff <- data.frame(t(types))
    colnames(ff) <- colnames(toprint)
    rownames(ff) <- paste(rep(" ",max(nchar(rownames(toprint)))),collapse="")
    toprint <- rbind(toprint,ff)

    ### only format the layout if "subset" print type chosen (as above)
    if(print_type == "subset"){
      toprint <- format(toprint, width = name.width, justify = "centre")
    }


    ##### FOR ALL PRINT_TYPES print() toprint at this point
    print(toprint)


    ### Finally, only return non-printed columns if "subset" print type chosen
    if(print_type == "subset"){
      writeLines(paste(rep("-",w/8),collapse=""))

      Cols_not_printed <- paste(cols_not_printed, collapse = ", ")
      Cols_not_printed <- paste0("\033[0;", 37, "m", Cols_not_printed, "\033[0m")

      footer_text <- paste0("A further ", length(cols_not_printed), " columns are not displayed: ")
      footer_text <- paste0("\033[0;", 35, "m", footer_text, "\033[0m")
      cat(footer_text)
      cat(Cols_not_printed,"\n")

    }

    # print final line

    cat(paste0("\033[0;", lcol, "m",paste(rep("_",w),collapse=""), "\033[0m","\n"))

    # Final warning if object is unbalanced
    if(warn == TRUE){warning(message)}

    }

    # return global settings of the user
    set_options_mR(pm,pt)
}










