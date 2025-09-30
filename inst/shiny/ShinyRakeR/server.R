
server <- function(input, output, session) {

  #### dev
  #df <- userData

  # Retrieve user-supplied data or use default
  #data <- session$options$userData
  #Providers <- session$options$Providers

  data <- shiny::getShinyOption("userData", default = fake_data)
  Providers <- shiny::getShinyOption("Providers", default = c("OpenStreetMap", "GoogleEarth"))

  #browser()
  if(is.null(data)){data <- fake_data} # fallback example dataset

  tids<- unique(data$TagID)
  if(is.null(Providers)){
    # fallback in case not passed
    Providers <- c("OpenStreetMap", "GoogleEarth", "Esri.OceanBasemap", "Esri.WorldImagery")
  }

  # Define a palette for TagID colors
  tag_palette <- reactive({
    df <- filtered_data_tag()
    unique_tags <- sort(unique(df$TagID))
    colorFactor(
      palette = viridisLite::viridis(length(unique_tags), option = "D"),
      domain = unique_tags
    )
  })

  # --- Validate columns ---
  req_cols <- c("TagID", "DateTime", "latitude", "longitude")
  missing_cols <- setdiff(req_cols, names(data))
  if(length(missing_cols) > 0){
    stop(paste("Data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Ensure DateTime is POSIXct
  if(!inherits(data$DateTime, "POSIXct")){
    data$DateTime <- as.POSIXct(data$DateTime)
  }


  time_range <- reactiveVal(NULL)

  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################

  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  # Tab 1 Annotation
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################

  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################

  # using a base data that picks a tagID from new input$tid2
  # then annotated data is a reactive that uses the base_data selection of tagIDs
  # to perform some annoations

  # starting point
  # Base dataset (all animals, raw)

  # --- Reactive: filter by TagID ---
  output$tid2 <- renderUI({
    pickerInput(
      inputId = "tid2",
      label = "TagID:",
      choices = unique(data$TagID),
      selected = unique(data$TagID)[1],
      multiple = TRUE
    )
  })

  #output$tid_dropdown_ui2 <- renderUI({
  #  shinyWidgets::dropdown(
  #    inputId = "tid_dropdown",   # different from `tid2`
  #    label = "Select animals",
  #    pickerInput(
  #      inputId = "tid2",
  #      label = NULL,
  #      choices = unique(data$TagID),
  #      selected = unique(data$TagID)[1],
  #      multiple = TRUE
  #    ),
  #    style = "unite",
  #    icon = icon("earlybirds"),
  #    width = "200px"
  #  )
  #})

  ######################################################################################################

  # Synchronize slider -> numeric
  observeEvent(input$turn_thresh_slider, {
    updateNumericInput(session, "turn_thresh_numeric", value = input$turn_thresh_slider)
  })

  # Synchronize numeric -> slider
  observeEvent(input$turn_thresh_numeric, {
    updateSliderInput(session, "turn_thresh_slider", value = input$turn_thresh_numeric)
  })

  ################
  observeEvent(input$nsat_thresh_slider, {
    updateNumericInput(session, "nsat_thresh_numeric", value = input$nsat_thresh_slider)
  })

  observeEvent(input$nsat_thresh_numeric, {
    updateSliderInput(session, "nsat_thresh_slider", value = input$nsat_thresh_numeric)
  })

  ################
  observeEvent(input$pdop_thresh, {
    updateNumericInput(session, "pdop_thresh_numeric", value = input$pdop_thresh)
  })

  observeEvent(input$pdop_thresh_numeric, {
    updateSliderInput(session, "pdop_thresh", value = input$pdop_thresh_numeric)
  })

  ################
  observeEvent(input$hdop_thresh, {
    updateNumericInput(session, "hdop_thresh_numeric", value = input$hdop_thresh)
  })

  observeEvent(input$hdop_thresh_numeric, {
    updateSliderInput(session, "hdop_thresh", value = input$hdop_thresh_numeric)
  })

  ################
  observeEvent(input$speed_thresh, {
    updateNumericInput(session, "speed_thresh_numeric", value = input$speed_thresh)
  })

  observeEvent(input$speed_thresh_numeric, {
    updateSliderInput(session, "speed_thresh", value = input$speed_thresh_numeric)
  })

  ################
  observeEvent(input$custom_speed_thresh, {
    updateNumericInput(session, "custom_speed_thresh_numeric", value = input$custom_speed_thresh)
  })

  observeEvent(input$custom_speed_thresh_numeric, {
    updateSliderInput(session, "custom_speed_thresh", value = input$custom_speed_thresh_numeric)
  })

  ################
  observeEvent(input$fix_thresh_slider, {
    updateNumericInput(session, "fix_thresh_numeric", value = input$fix_thresh_slider)
  })

  observeEvent(input$fix_thresh_numeric, {
    updateSliderInput(session, "fix_thresh_slider", value = input$fix_thresh_numeric)
  })



  # -------------------------------------------------------------- #
  # Reactive data start
  # -------------------------------------------------------------- #

  # some processes don't depend on gaps, such as fix-specific pdop, hdop, NA lat longs, dups, turn filter etc
  # but speed filer DOES - so needs to recognise the correct reactive
  # Base data (optionally gap-sectioned)
  data_start <- reactive({ data })

  # ---------------------------------------- #
  # Gap assignment
  # ---------------------------------------- #

  # reactiveVals
  gapped_data <- reactiveVal({ data }) # I think this needs assigning in case the user DOES NOT run gapsections first
  gap_done <- reactiveVal(FALSE)

  # if the user presses gapsection run button, then yes do gapsections
  observeEvent(input$run_gap_section, {
    #browser()
    gap_done(FALSE)

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Gap section definitions ...")

    #df <- data_start()
    df <- get_annotation_base()
    #if(input$do_gap_section) {
      df <- gap_section(
        df,
        GAP = input$gap_thresh,
        tol = input$gap_tol,
        drop_single_gap = FALSE,
        verbose = TRUE,
        attr = FALSE
      )
    #}

    df$singlegap_rm <- 0

    shinybusy::remove_modal_spinner()
    gap_done(TRUE)
    gapped_data(df)
    annotated_data(df)

  })

  # UI element for showing the tick
  output$gap_done_ui <- renderUI({
    if(gap_done()){
      tags$span(style="color:green; font-weight:bold;", "\u2714 Gap section applied!")  # ✓ symbol
    } else {
      NULL
    }
  })

  # ---------------------------------------- #
  # TagID filter happens after gaps assigned
  # ---------------------------------------- #
  # best to NOT do this as it complicates things; filter TagIDs only after full dataset has been handled for simplicity
  #data_tagged <- reactive({
  #  df <- gapped_data()
  #  req(df)
  #  if(!is.null(input$tid2)) {
  #    df <- df %>% dplyr::filter(TagID %in% input$tid2)
  #  }
  #  df
  #})

  # ---------------------------------------- #
  # Annotated data
  # ---------------------------------------- #

  # Central store for annotated data
  annotated_data <- reactiveVal(NULL)

  # Helper to get current base for annotation
  #
  #
  #
  #
  # / # / # / # / # at the moment this seems incorrect as gapped data is ONLY run if annotated_data is null
  # i.e. if it is not null, and the user want to re-do gaps, then bespoke gaps will not come through
  # and the check in speed filt below will either find the prvious gap column or not at all so defaults will be taken
  #
  #
  #
  # / # / # / # / #
  #get_annotation_base <- function() {
  #  if(is.null(annotated_data())) {
  #
  #    gapped_data() # <-------------------- ...... only when NULL?
  #    #data_tagged() # skipping this part of the reactive chain
  #  } else {
  #
  #    annotated_data()
  #  }
  #}

  get_annotation_base <- function(){
   base <- gapped_data()
   req(base)

   if(!is.null(annotated_data())){
     return(annotated_data())
   } else{
     return(base)
   }

  }



  ########################################################################################################
  # Actions for annotating
  ########################################################################################################

  # possible column names:
  # - NA_latlong_rm (lat/long point NA)
  # - all_NA_rm (NA rows)
  # - sat_NA (NA of rows without satellites)
  # - sat_rm (number of satellites)
  # - flt_rm (flt_switch)
  # - gps_pdop_rm (pdop)
  # - gps_hdop_rm (hdop)
  # - singlegap_rm ('orphaned' gapsections)
  # - dup_rm (duplicate DateTimes)
  # - speed_rm (speed filter)
  # - angle_rm (angle filter)
  # - combined_rm, A combined remove column using conditions across all annotation rows
  #choices = c("Latlong NA", "All NA", "Satellites NA", "Satellites removed", "Flt_switch",
  #            "PDOP", "HDOP", "Single gaps", "DateTime duplicates",
  #            "Traj speed", "Turn angle"),

  #########################################################################################################

  # -------------------------------------------------------------- #
  # Birds with too few fixes
  # -------------------------------------------------------------- #

  observeEvent(input$run_fixflag, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    # basically, if any birds have too few fixes, flag it.
    # basic dplyr
    df <- get_annotation_base()

    df0 = df %>% group_by(TagID) %>%
      summarise(n = n()) %>%
      mutate(flag = ifelse(n < input$fix_thresh_numeric, 1L, 0L))

    # which animals have less than the min number
    tid_rm <- df0[df0$flag == 1L,]$TagID
    df$tid_rm <- ifelse(df$TagID %in% tid_rm, 1L, 0L)

    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })



  # -------------------------------------------------------------- #
  # NA Lat Longs
  # -------------------------------------------------------------- #

  na_ll_warn <- reactiveVal(2)

  # UI element for warning if pdop column not existing
  output$na_warning <- renderUI({
    #browser()
    if(na_ll_warn() == 0){
      tags$span(
        style = "color:blue; font-weight:bold;",
        "\u274C No missing lat long data found"
      )
    } else {
      NULL
    }
  })

  observeEvent(input$close_warn_1,{
    output$na_warning <- NULL
  })

  observeEvent(input$run_na_ll, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    #browser()
    df <- get_annotation_base()
    req(df)

    na_ll_warn(0)

    # annotate the rows
    df$NA_latlong_rm <- 0

    if(any(is.na(df$latitude) | is.na(df$longitude))){
      df$NA_latlong_rm <- ifelse( is.na(df$latitude) | is.na(df$longitude),1, df$NA_latlong_rm)

      na_ll_warn(1)
    }

    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })


  # thought: clear button to remove the flag from the data and thus also the map?



  # -------------------------------------------------------------- #
  # Duplicate data times
  # -------------------------------------------------------------- #

  observeEvent(input$run_dup, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    df <- get_annotation_base()
    req(df)

    df <- df %>%
      group_by(TagID) %>%
      mutate(dup_rm = if_else(duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE), 1L, 0L)) %>%
      ungroup()

    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })



  # -------------------------------------------------------------- #
  # Number of satellites
  # -------------------------------------------------------------- #

  # satellite column
  output$sel_sat_col <- renderUI({

    df = data_start()

    selectInput(
      inputId = "sat_col",
      label =NULL,
      choices = names(df),   # or dynamically via renderUI
      selected = NULL
    )
  })


  #observeEvent(input$run_nsats, {
  #
  #  shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")
  #
  #  #browser()
  #  df <- get_annotation_base()
  #  req(df)
  #
  #  # annotate the rows
  #  df$sat_rm <- ifelse(df$nsats >= input$nsat_thresh_numeric, 0, 1)
  #
  #  shinybusy::remove_modal_spinner()
  #  annotated_data(df)
  #
  #})

  observeEvent(input$run_nsats, {
    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running nsats ...")

    tryCatch({
      req(get_annotation_base(), input$sat_col, input$nsat_thresh_numeric)

      df <- get_annotation_base()
      colname <- input$sat_col

      # Defensive checks
      if (!colname %in% names(df)) {
        warning("Selected satellite column not found in data")
        return()
      }
      if (!is.numeric(df[[colname]])) {
        warning("Selected satellite column is not numeric")
        return()
      }
      if (all(is.na(df[[colname]]))) {
        warning("Selected satellite column contains only NA values")
        return()
      }
      if (max(df[[colname]], na.rm = TRUE) > 31 || min(df[[colname]], na.rm = TRUE) < 0) {
        warning("Incompatible data stream selected")
        return()
      }

      # If we got here, safe to compute flag
      df$sat_rm <- ifelse(df[[colname]] >= input$nsat_thresh_numeric, 0L, 1L)
      annotated_data(df)

    }, finally = {
      shinybusy::remove_modal_spinner()
    })
  })

  # -------------------------------------------------------------- #
  # FLT_switch
  # -------------------------------------------------------------- #

  # may skip this here as niche

  # -------------------------------------------------------------- #
  # PDOP filter button
  # -------------------------------------------------------------- #
  pdop_warn <- reactiveVal(0)

  # UI element for warning if pdop column not existing
  output$pdop_warning <- renderUI({
    #browser()
    if(pdop_warn() == 1){
      tags$span(
        style = "color:red; font-weight:bold;",
        "\u274C Warning: no column for pdop found!"
      )
    } else {
     NULL
    }
  })


  observeEvent(input$run_pdop, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    # user selection to keep in all data within the bar selected i.e. above max value are dropped out
    df <- get_annotation_base()

    pdop_warn(0)

    #browser()
    if(!exists("pdop", df)){
      if(!exists("gps.pdop", df)){
        if(!exists("PDOP", df)){
          if(!exists("gps_pdop", df)){
            warning("No column found matching pdop, gps.pdop, gps_pdop or PDOP, name to one of those.")
            pdop_warn(1)
          } else{
            df <- df %>% mutate(gps_pdop_rm = case_when(is.na(gps_pdop) ~ NA_integer_, gps_pdop >= input$pdop_thresh ~ 1L, gps_pdop < input$pdop_thresh ~ 0L))
          }
        } else{
          df <- df %>% mutate(gps_pdop_rm = case_when(is.na(PDOP) ~ NA_integer_, PDOP >= input$pdop_thresh ~ 1L, PDOP < input$pdop_thresh ~ 0L))
        }
      } else{
        df <- df %>% mutate(gps_pdop_rm = case_when(is.na(gps.pdop) ~ NA_integer_, gps.pdop >= input$pdop_thresh ~ 1L, gps.pdop < input$pdop_thresh ~ 0L))
      }
    } else{
      df <- df %>% mutate(gps_pdop_rm = case_when(is.na(pdop) ~ NA_integer_, pdop >= input$pdop_thresh ~ 1L, pdop < input$pdop_thresh ~ 0L))
    }

    #df <- get_annotation_base()
    #req(df)
    #df <- pdop_filt(df, pdop_thresh = input$pdop_thresh)
    #annotated_data(df)
    shinybusy::remove_modal_spinner()
    annotated_data(df)
  })


  # -------------------------------------------------------------- #
  # HDOP filter button
  # -------------------------------------------------------------- #
  hdop_warn <- reactiveVal(0)

  # UI element for warning if pdop column not existing
  output$hdop_warning <- renderUI({
    #browser()
    if(hdop_warn() == 1){
      tags$span(
        style = "color:red; font-weight:bold;",
        "\u274C Warning: no column for hdop found!"
      )
    } else {
      NULL
    }
  })

  observeEvent(input$run_hdop, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    # user selection to keep in all data within the bar selected i.e. above max value are dropped out
    df <- get_annotation_base()

    hdop_warn(0)

    #browser()
    if(!exists("hdop", df)){
      if(!exists("gps.hdop", df)){
        if(!exists("HDOP", df)){
          if(!exists("gps_hdop", df)){
            warning("No column found matching hdop, gps.hdop, gps_hdop or HDOP, name to one of those.")
            hdop_warn(1)
          } else{
            df <- df %>% mutate(gps_hdop_rm = case_when(is.na(gps_hdop) ~ NA_integer_, gps_hdop >= input$hdop_thresh ~ 1L, gps_hdop < input$hdop_thresh ~ 0L))
          }
        } else{
          df <- df %>% mutate(gps_hdop_rm = case_when(is.na(HDOP) ~ NA_integer_, HDOP >= input$hdop_thresh ~ 1L, HDOP < input$hdop_thresh ~ 0L))
        }
      } else{
        df <- df %>% mutate(gps_hdop_rm = case_when(is.na(gps.hdop) ~ NA_integer_, gps.hdop >= input$hdop_thresh ~ 1L, gps.hdop < input$hdop_thresh ~ 0L))
      }
    } else{
      df <- df %>% mutate(gps_hdop_rm = case_when(is.na(hdop) ~ NA_integer_, hdop >= input$hdop_thresh ~ 1L, hdop < input$hdop_thresh ~ 0L))
    }

    #df <- get_annotation_base()
    #req(df)
    #df <- pdop_filt(df, pdop_thresh = input$pdop_thresh)
    #annotated_data(df)
    shinybusy::remove_modal_spinner()
    annotated_data(df)
  })

  # -------------------------------------------------------------- #
  # Singlegap_rm ('orphaned' gapsections)
  # -------------------------------------------------------------- #

  single_gap_warn <- reactiveVal(2)

  # UI element for warning if pdop column not existing
  output$singlegap_warning <- renderUI({
    #browser()
    if(single_gap_warn() == 1){
      tags$span(
        style = "color:red; font-weight:bold;",
        "\u274C Warning: no column for gapsec found (run step 1.)"
      )
    } else {
      NULL
    }
  })


  observeEvent(input$close_warn_2,{
    output$singlegap_warning <- NULL

    single_gap_warn(2)
  })

  # if the user presses gapsection run button, then yes do gapsections
  observeEvent(input$run_orph, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running pdop simple filter ...")

    #browser()
    df <- get_annotation_base()

    if(exists("gapsec", df)){

      single_gap_warn(0)

      # assess the structure of gapsections
      # this needs checking with real data ...

      df <- df %>%
        group_by(TagID, gapsec) %>%
        mutate(singlegap_rm = if_else(n() == 1, 1L, 0L)) %>%  # 1 if only one row in group
        ungroup()

      shinybusy::remove_modal_spinner()
      annotated_data(df)
    } else{

      single_gap_warn(1)
      df$singlegap_rm <- 0

      shinybusy::remove_modal_spinner()
      annotated_data(df)

      #single_gap_warn(2)
    }

  })


  # -------------------------------------------------------------- #
  # MoveRakeR speed filter
  # -------------------------------------------------------------- #
  observeEvent(input$run_speed, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running MoveRakeR::speed_filt() ...")

    df <- get_annotation_base()
    req(df)
    has_gaps <- "gapsec" %in% names(df)
    df <- speed_filt(df, sp_thres = input$speed_thresh, annotate = TRUE,
                     verbose = TRUE, detailed_verbose = FALSE, hasgaps = has_gaps)

    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })

  # -------------------------------------------------------------- #
  # Simple speed filter
  # -------------------------------------------------------------- #

  # ok here - my process works best for really unusual points and the filter can remove points that
  # point-by-point are ok but once one is removed may still be in violation

  # so we may want to have FURTHER flexibility to just look at a flat filter or use a t+1 t-1 approach
  # or bring distance in as well!!!!! That goes somewhat against the MR process!

  observeEvent(input$run_custom_speed, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running custom speed filt ...")

    df <- get_annotation_base()
    req(df)
    has_gaps <- "gapsec" %in% names(df)

    # for each gapsection per bird - just do a simple speed filter from x to x+1
    #browser()

    if(has_gaps){
      df0 = df %>% group_by(TagID, gapsec)
    } else{
      df0 = df %>% group_by(TagID)
      warning("---- no gapsecs yet defined, running at the bird level ----")
    }

    df0 <- df0 %>%
      mutate(
        dist = if (n() > 1) {
          c(geosphere::distHaversine(cbind(longitude, latitude)), NA)
        } else {
          NA_real_
        },
        dt = as.numeric(difftime(lead(DateTime), DateTime, units = "secs"))
      )

    df0 = df0 %>% mutate(traj_speed = dist / dt,
                          traj_speed = lag(traj_speed, default= NA_real_)) # pad with 'NA' at the start, removing last row

    # assign values
    df0 <- df0 %>% mutate(cust_speed_rm = case_when(is.na(traj_speed) ~ NA_integer_, traj_speed >= input$custom_speed_thresh_numeric ~ 1L, traj_speed < input$custom_speed_thresh_numeric ~ 0L))

    df0 = df0 %>% ungroup() %>% group_by(TagID)

    shinybusy::remove_modal_spinner()
    annotated_data(df0)

  })


  # -------------------------------------------------------------- #
  # Turning angle filter button
  # -------------------------------------------------------------- #
  observeEvent(input$run_turn, {

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running MoveRakeR::turn_filt() ...")

    df <- get_annotation_base()
    req(df)
    has_gaps <- "gapsec" %in% names(df)

    #browser()
    df <- turn_filt(data = df, turnFilt = input$turn_thresh_numeric, turnFilt_dir = input$turn_dir,
                         annotate = TRUE, verbose = TRUE, hasgaps = has_gaps) # using pre-identified gaps from step #6

    df <- df %>% select(-any_of(c("init_row", "cc", "row_id")))


    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })

  # -------------------------------------------------------------- #
  # Extreme lat long selector
  # -------------------------------------------------------------- #

  # this uses the map clicks when rectangle mode is ON

  # need an oberver to automatically detect population of the coordinates
  # and remove the option again if it is NULL

  observeEvent(input$run_ll,{

    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Running extreme lat longs ...")

    df <- get_annotation_base()
    req(df)

    # if rectangle mode is on, compute:
    if(rect_active()){

      #browser()
      if(!is.na(input$lng1) & !is.na(input$lng2) & !is.na(input$lat1) & !is.na(input$lat2)){
        if(!is.null(input$lng1) & !is.null(input$lng2) & !is.null(input$lat1) & !is.null(input$lat2)){
          df <- df %>% mutate(ll_rm =
                                ifelse(longitude > input$lng1 & longitude < input$lng2 &
                                         latitude > input$lat2 & latitude < input$lat1, 0L, 1L)
          )
        }
      }

    } else{

       df <- df %>% select(-any_of("ll_rm"))

    }

    shinybusy::remove_modal_spinner()
    annotated_data(df)

  })


  # -------------------------------------------------------------------- #
  # timeline plot

  timeline_data <- reactive({
    df <- get_annotation_base()
    req(df)

    slider <- input$time_slider

    if(!is.null(slider) && length(slider) == 2){
      df <- df %>% filter(DateTime >= slider[1],
                          DateTime <= slider[2])
    }

    df
  })


  output$timeline_plot <- renderPlot({
    df <- timeline_data()
    req(nrow(df) > 0)
    req(input$date_break)

    plotter <- tag_timeline(df,
                            plot = FALSE,
                            return_plot = TRUE,
                            overlaps = TRUE,
                            messages = FALSE)


    suppressMessages({
      attr(plotter, "plot") +
        scale_x_date(
          date_breaks = input$date_break, #"1 year"
          date_labels =  input$date_labels #"%Y"
        ) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            vjust = 1
          )
        )

      })

  })


  #######################################################################################################


  # -------------------------------------------------------------- #
  # Custom flags
  # -------------------------------------------------------------- #
  # user may already have columns flagged and want to include / visualise
  # e.g. different versions of speed filtering from different R packages



  # 1. Let user pick extra flags
  # UI for user to select existing flag-like columns (0/1/NA)
  output$custom_flag_ui <- renderUI({
    df <- data_start()
    req(df)

    # find candidate columns that are only 0, 1, NA (ignoring missing)
    is_flag_col <- vapply(df, function(col) {
      if (!is.numeric(col) && !is.integer(col)) return(FALSE)
      vals <- unique(na.omit(col))
      all(vals %in% c(0, 1))
    }, logical(1))

    flag_candidates <- names(df)[is_flag_col]

    #exclude potential gap and gapsec
    excl <- c("gap", "gapsec")

    if(any(flag_candidates %in% excl)){
      flag_candidates <- flag_candidates[!flag_candidates %in% excl]
    }

    if(length(flag_candidates) == 0) {
      return(NULL)  # don’t show anything if no eligible columns
    }

    selectInput(
      inputId = "custom_flags",
      label = "Add existing flag columns:",
      choices = flag_candidates,
      multiple = TRUE,
      selected = NULL
    )
  })


  #
  #
  #

  # Combine annotation columns automatically
  # mapping between human labels and real column names
  flag_labels <- c(
    "Combined"     = "combined_rm",
    "Animal nfix"  = "tid_rm",
    "NA latlong"   = "NA_latlong_rm",
    "No. sats"     = "sat_rm",
    "PDOP"         = "gps_pdop_rm",
    "HDOP"         = "gps_hdop_rm",
    "Traj. speed" = "speed_rm",
    "Traj. speed (custom)" = "cust_speed_rm",
    "Turn angle"   = "angle_rm",
    "DateTime dups" = "dup_rm",
    "Single gap"   = "singlegap_rm",
    "Extreme latlongs" = "ll_rm"
  )


#  # 2. Main flag source selector (built-in + user-provided)
#  output$flag_source_ui <- renderUI({
#    df <- annotated_data()
#    req(df)
#
#    # Add custom flags if present
#    custom <- input$custom_flags
#    choices <- c(flag_labels, setNames(custom, custom))
#
#    selectInput(
#      inputId = "flag_source",
#      label   = "Which flag to use for map labels?",
#      choices = choices,
#      selected = "combined_rm"
#    )
#  })

  # Dynamic UI for choosing flag source
  output$flag_source_ui <- renderUI({
    df <- annotated_data()
    req(df)

    #browser()
    # Keep only those that are actually present in df
    builtin_map <- flag_labels[flag_labels %in% names(df)]

    # ---- User-provided flags (0/1/NA) ----
    user_flag_cols <- names(df)[sapply(df, function(col) {
      is.numeric(col) && all(na.omit(col) %in% c(0, 1))
    })]

    # Remove app-generated columns so we only keep "external" flags
    user_flag_cols <- setdiff(user_flag_cols, builtin_map)

    # But only use if the user has selected it
    extra_flags <- input$custom_flags

    if(!is.null(extra_flags)){
      user_flag_cols <- user_flag_cols[extra_flags %in% user_flag_cols]

      # Map them with their raw names as display labels
      user_map <- setNames(user_flag_cols, user_flag_cols)

      # ---- Combine all choices ----
      all_choices <- c(builtin_map, user_map)

    } else{
      all_choices <- builtin_map
    }

    if(length(all_choices) == 0) {
      return(tags$em("No annotation columns available yet"))
    }


    #exclude potential gap and gapsec
    excl <- c("gap", "gapsec")

    if(any(all_choices %in% excl)){
      all_choices <- all_choices[!all_choices %in% excl]
    }

    selectInput(
      inputId = "flag_source",
      label   = "Which column to use for map labels?",
      choices = all_choices,
      selected = if("combined_rm" %in% all_choices) {
        "combined_rm"
      } else {
        all_choices[1]
      }
    )
  })


  # Combine annotation columns automatically (built-in + user-provided)
  observe({
    df <- annotated_data()
    req(df)

    #browser()
    # Built-in flags you always consider
    base_flags <- c(
      "tid_rm","NA_latlong_rm", "all_NA_rm", "sat_rm", "flt_rm",
      "gps_pdop_rm", "gps_hdop_rm", "singlegap_rm",
      "dup_rm", "speed_rm", "cust_speed_rm","angle_rm", "ll_rm"
    )

    # Add user-selected flags
    extra_flags <- input$custom_flags
    all_flags <- unique(c(base_flags, extra_flags))
    all_flags <- intersect(all_flags, names(df))  # keep only those that exist

    if(length(all_flags) == 0L) {
      df$combined_rm <- NA_integer_
    } else {
      mat <- as.matrix(df[, all_flags, drop = FALSE])
      df$combined_rm <- ifelse(rowSums(mat == 1, na.rm = TRUE) > 0, 1L,
                               ifelse(rowSums(mat == 0, na.rm = TRUE) > 0, 0L, NA_integer_))
    }

    annotated_data(df)
  })

  #observe({
  #  df <- annotated_data()
  #  req(df)
  #
  #  base_flags <- c("NA_latlong_rm", "all_NA_rm", "sat_rm", "flt_rm",
  #           "gps_pdop_rm", "gps_hdop_rm", "singlegap_rm",
  #           "dup_rm", "speed_rm", "angle_rm")
  #  # Add custom flag if selected
  #  #if(!is.null(input$custom_flag_col) && input$custom_flag_col != "None"){
  #  #  nms <- c(nms, input$custom_flag_col)
  #  #}
  #  # Add custom flags if selected
  #  if (!is.null(input$custom_flag_cols) && length(input$custom_flag_cols) > 0) {
  #    base_flags <- c(base_flags, input$custom_flag_cols)
  #  }
  #  cols <- intersect(base_flags, names(df))
  #  if(length(cols) == 0L) {
  #    df$combined_rm <- NA_integer_
  #  } else {
  #    mat <- as.matrix(df[, cols, drop = FALSE])
  #    df$combined_rm <- ifelse(rowSums(mat == 1, na.rm = TRUE) > 0, 1L,
  #                             ifelse(rowSums(mat == 0, na.rm = TRUE) > 0, 0L, NA_integer_))
  #  }
  #  annotated_data(df)
  #})

  # -------------------------------------------------------------- #
  # Filter subset based on flag toggle OR TagID
  # -------------------------------------------------------------- #
  filtered_annotated <- reactive({
    df <- annotated_data()
    req(df)

    # Determine which column to use for plotting/filtering
    flag_col <- input$flag_source

    # If NULL or not found in df, default to combined_rm
    if (is.null(flag_col) || !(flag_col %in% names(df))) {
      flag_col <- "combined_rm"
    }

    # Create a generic plotting column
    df$plot_col <- df[[flag_col]]

    # Apply combined filter dropdown and subset from the plot_col
    if(!is.null(input$flag_filter)) {
      if(!("All" %in% input$flag_filter)){
        keep <- rep(FALSE, nrow(df))
        if("Flagged" %in% input$flag_filter) keep <- keep | df$plot_col == 1
        if("OK" %in% input$flag_filter)      keep <- keep | df$plot_col == 0
        if("NA" %in% input$flag_filter)      keep <- keep | is.na(df$plot_col)
        df <- df[keep, ]
      }
    }


    # Apply TagID filter
    if(!is.null(input$tid2) && length(input$tid2) > 0){
      df <- df[df$TagID %in% input$tid2, ]
    } else{
      unique(df$TagID)[1]
      df <- df[df$TagID %in% unique(df$TagID)[1], ]
    }

    return(df)
  })

  ##########################
  filtered_data_threshold <- reactive({
    df <- filtered_annotated()
    req(input$time_slider_threshold)
    df %>% filter(DateTime >= input$time_slider_threshold[1],
                  DateTime <= input$time_slider_threshold[2])
  }) %>% debounce(200)

  ###############################################################################################

  # -------------------------------------------------------------- #
  # MAPPING
  # -------------------------------------------------------------- #

  # Leaflet map for annotated data
  output$mymap2 <- leaflet::renderLeaflet({
    map_reactive()
  })

  # GPS lines
  observe({

    #df <- filtered_annotated()
    df <- filtered_data_threshold()
    req(df)

    proxy <- leafletProxy("mymap2")

    # Remove existing polylines
    proxy %>% clearGroup("trajectories")

    if(isTRUE(input$show_lines)) {
      #line_list <- lapply(split(filtered_annotated(), filtered_annotated()$TagID), function(tag_df) {
      line_list <- lapply(split(filtered_data_threshold(), filtered_data_threshold()$TagID), function(tag_df) {
        if(nrow(tag_df) > 1) sfheaders::sf_linestring(tag_df, x="longitude", y="latitude") else NULL
      })
      line_list <- do.call(rbind, line_list)

      if(!is.null(line_list) && nrow(line_list) > 0){
        proxy %>% addPolylines(data=line_list, color="grey", weight= input$line_size, opacity=input$line_opacity, group="trajectories")
      }
    }
  })


  # GPS fixes
  observe({
    #df <- filtered_annotated()
    df <- filtered_data_threshold()
    req(df)

    df$flag_col <- ifelse(is.na(df$plot_col), "grey",
                          ifelse(df$plot_col == 1, "red", "blue"))

    ### flag reasoning....
    base_flags <- c(
      "tid_rm","NA_latlong_rm", "all_NA_rm", "sat_rm", "flt_rm",
      "gps_pdop_rm", "gps_hdop_rm", "singlegap_rm",
      "dup_rm", "speed_rm", "cust_speed_rm","angle_rm", "ll_rm"
    )


    # Add user-selected flags
    extra_flags <- input$custom_flags
    all_flags <- unique(c(base_flags, extra_flags))
    all_flags <- intersect(all_flags, names(df))  # keep only those that exist

    # Create a flag reason column
    #df$flag_reason <- apply(df[, all_flags, drop = FALSE], 1, function(row_vals) {
    #  flagged <- names(row_vals)[!is.na(row_vals) & row_vals == 1]   # ignore NAs
    #  if(length(flagged) == 0) return("OK")
    #  paste("Flagged by:", paste(flagged, collapse = ", "))
    #})

    # Build popup text
    #df$popup_text <- paste0(df$TagID, ": ", df$DateTime, "<br>", df$flag_reason)

    #df$popup_text <- paste0(df$TagID, ": ", df$DateTime,
    #                        ifelse(df$plot_col == 1, " Flagged",
    #                               ifelse(is.na(df$plot_col), " NA", " OK")))

    # Example mapping: flag column -> original data column
    flag_value_map <- list(
      tid_rm       = NA,           # purely binary flag
      NA_latlong_rm= NA,           # binary
      sat_rm       = "satellites_used",     ##### in the test case....this could vary by what the user had named!
      gps_pdop_rm  = "gps.pdop",   ##### in the test case....this could vary by what the user had named!
      gps_hdop_rm  = "gps.hdop",   ##### in the test case....this could vary by what the user had named!
      speed_rm     =  NA, ##### NOT CURRENTLY CARRIED THROUGH in  way that matches how traj_speed() works
      cust_speed_rm = "traj_speed", # the current row is a lead so this is the speed from current row to t-1
      angle_rm     = "turn_angle", ##### NOT CURRENTLY CARRIED THROUGH
      dup_rm       = NA,
      singlegap_rm = NA,
      ll_rm        = NA
    )

    # Add any user-selected flags as NA by default
    extra_flags <- input$custom_flags
    if(!is.null(extra_flags)){
      for(f in extra_flags){
        if(!(f %in% names(flag_value_map))) flag_value_map[[f]] <- NA
      }
    }
    #browser()
    #i = 39

    # Now generate popup text with underlying values when available
    df$flag_reason <- sapply(seq_len(nrow(df)), function(i){
      row_vals <- df[i, all_flags, drop = FALSE]
      flagged <- names(row_vals)[!is.na(row_vals) & row_vals == 1]
      nas <- names(row_vals)[is.na(row_vals)]

      if(length(flagged) == 0 && length(nas) == 0) return("OK")

      flagged_text <- c()

      if(length(flagged) > 0){
        flagged_text <- sapply(flagged, function(f){
          value_col <- flag_value_map[[f]]
          if(!is.na(value_col) && value_col %in% names(df)){
            val <- df[i, value_col]  # <- take value from **current row only**
            paste0(f, " (", round(val, 2), ")")
          } else {
            paste0(f, " (1)")
          }
        })
      }

      if(length(nas) > 0){
        flagged_text <- c(flagged_text, paste0(nas, " (NA)"))
      }

      paste0("Flagged by: ", paste(flagged_text, collapse = ", "))
    })

    # Integrate into popup text
    df$popup_text <- paste0(
      df$TagID, ": ", df$DateTime, "\n", df$flag_reason
    )

    # update map
    if(isTRUE(input$show_points)) {
      leafletProxy("mymap2") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data=df, ~longitude, ~latitude,
          popup=~popup_text,
          radius = input$point_size,
          color=~flag_col,
          fillOpacity=input$point_opacity
        )
    } else{
      leafletProxy("mymap2") %>%
        clearMarkers()
    }

  })

  # Legend
  observeEvent(input$show_legend,{
    #df <- filtered_annotated()
    df <- filtered_data_threshold()
    req(df)
    if(isTRUE(input$show_legend)) {
     leafletProxy("mymap2") %>%
        addLegend("bottomright",
                  colors=c("blue","red","grey"),
                  labels=c("OK","Flagged","NA"),
                  title="Annotation")

    } else{
      leafletProxy("mymap2") %>% clearControls()
    }


  }, ignoreInit = FALSE)



  # ---------------------------------------------------------------------------------------- #
  # The rake elements
  # ---------------------------------------------------------------------------------------- #
  # using percentage of flagged points per annotation step
  # this uses annotated_data() i.e. nothing filtered by TagID, or time

  output$rake_circles <- renderUI({
    df <- annotated_data()
    req(df)

    steps <- c(
      "Combined"      = "combined_rm",
      "Animal nfix"   = "tid_rm",
      "NA latlong"    = "NA_latlong_rm",
      "No. sats"      = "sat_rm",
      "PDOP"          = "gps_pdop_rm",
      "HDOP"          = "gps_hdop_rm",
      "Traj. speed"   = "speed_rm",
      "Traj. speed (custom)" = "cust_speed_rm",
      "Turn angle"    = "angle_rm",
      "DateTime dups" = "dup_rm",
      "Single gap"    = "singlegap_rm",
      "Extreme latlongs" = "ll_rm"
    )

    extra_flags <- input$custom_flags
    if(!is.null(extra_flags)){
      extra_map <- setNames(extra_flags, extra_flags)
      steps <- c(steps, extra_map)
    }

    # Inline display of gauges with labels
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 20px;",
      lapply(names(steps), function(step_name) {
        col <- steps[[step_name]]
        if(!col %in% names(df)) return(NULL)

        gauge_id <- paste0("gauge_", col)

        tags$div(
          style = "flex: 0 0 auto; text-align: center;",
          tags$div(tags$b(step_name), style = "margin-bottom: 4px; font-size: 1em;"),
          flexdashboard::gaugeOutput(gauge_id, width = "160px", height = "140px")
        )
      })
    )
  })

  observeEvent(annotated_data(),{
      df <- annotated_data()
      req(df)


      steps <- c(
        "Combined"      = "combined_rm",
        "Animal nfix"   = "tid_rm",
        "NA latlong"    = "NA_latlong_rm",
        "No. sats"      = "sat_rm",
        "PDOP"          = "gps_pdop_rm",
        "HDOP"          = "gps_hdop_rm",
        "Traj. speed"   = "speed_rm",
        "Traj. speed (custom)" = "cust_speed_rm",
        "Turn angle"    = "angle_rm",
        "DateTime dups" = "dup_rm",
        "Single gap"    = "singlegap_rm",
        "Extreme latlongs" = "ll_rm"
      )

      extra_flags <- input$custom_flags
      if(!is.null(extra_flags)){
        extra_map <- setNames(extra_flags, extra_flags)
        steps <- c(steps, extra_map)
      }

      for(step_name in names(steps)) {
        col <- steps[[step_name]]
        if(!col %in% names(df)) next

        gauge_id <- paste0("gauge_", col)

        local({
          col_local <- col
          gauge_id_local <- gauge_id

          output[[gauge_id_local]] <- flexdashboard::renderGauge({
            n_total <- nrow(df)
            n_flagged <- sum(df[[col_local]] == 1, na.rm = TRUE)
            pct <- round(100 * n_flagged / n_total, 1)

            flexdashboard::gauge(
              value = pct,
              min = 0, max = 100,
              sectors = gaugeSectors(
                success = c(70, 100),
                warning = c(40, 69),
                danger  = c(0, 39)
              ),
              label = paste0(n_flagged, "/", n_total, " flagged")
            )
          })
        })
      }
    })

  ####################

  output$rake_summary_table <- DT::renderDataTable({
    #browser()
    df <- annotated_data()
    req(df)

    #browser()

    steps <- c(
      "Combined"      = "combined_rm",
      "Animal nfix"   = "tid_rm",
      "NA latlong"    = "NA_latlong_rm",
      "No. sats"      = "sat_rm",
      "PDOP"          = "gps_pdop_rm",
      "HDOP"          = "gps_hdop_rm",
      "Traj. speed"   = "speed_rm",
      "Traj. speed (custom)" = "cust_speed_rm",
      "Turn angle"    = "angle_rm",
      "DateTime dups" = "dup_rm",
      "Single gap"    = "singlegap_rm",
      "Extreme latlongs" = "ll_rm"
    )


    extra_flags <- input$custom_flags
    if(!is.null(extra_flags)){
      extra_map <- setNames(extra_flags, extra_flags)
      steps <- c(steps, extra_map)
    }

    n_birds <- length(unique(df$TagID))
    n_fixes <- nrow(df)

    step_name = "speed_basic"
    summary_table <- lapply(names(steps), function(step_name) {
      #print(step_name)
      col <- steps[[step_name]]
      if(!col %in% names(df)) return(NULL)

      param_val <- switch(step_name, # this is a neat trick
                          "Combined" = "n/a",
                          "Animal nfix"   = "n/a",
                          "NA latlong"    = "n/a",
                          "No. sats" = input$nsat_thresh_numeric,
                          "PDOP" = input$pdop_thresh,
                          "HDOP" = input$hdop_thresh,
                          "Traj. speed" = input$speed_thresh_numeric,
                          "Traj. speed (custom)" = input$custom_speed_thresh_numeric,
                          "Turn angle" = input$turn_thresh_numeric,
                          "DateTime dups" = "n/a",
                          "Single gap"    = "n/a",
                          "Extreme latlongs" = "n/a"
                          )
      if(is.null(param_val)){param_val <- "n/a"}

      # this needs to be at the bird level
      df0 = df %>% select("TagID", col) %>% group_by(TagID) %>%
        summarise(n_flagged = sum(!!sym(col), na.rm=TRUE),
                  n_NA = length(which(is.na(!!sym(col)))),
                  total_fix = length(TagID),
                  pct = ifelse( ((n_flagged / total_fix )*100) < 0.01 & ((n_flagged / total_fix )*100) > 0, "<0.01",
                                as.character(round((n_flagged / total_fix )*100, 2)))
        )
      df0$Parameter = param_val
      df0$Annotation = step_name
      return(df0)
    })

    summary_table <- do.call(rbind, summary_table) # and combining only what data the user last selected, or has available
    DT::datatable(summary_table, rownames = FALSE, filter = "top",
                    options = list(
                      dom = 't',          # no extra controls
                      scrollY = "300px",  # vertical scroll height
                      scrollX = TRUE,     # horizontal scroll if needed
                      paging = FALSE      # show all rows, no pagination

                    )
                  )
  })



  ##################################
  # DATA EXPORT TO R SESSION

  # observe change to botton for export
  observeEvent(input$export_threshold, {
    req(input$export_name)
    df_export <- filtered_data_threshold()

    if(nrow(df_export) == 0){
      showNotification("No data to export!", type = "warning")
      return()
    }

    # Make a safe variable name
    safe_name <- make.names(input$export_name)

    # Assign to global R session
    assign(safe_name, df_export, envir = .GlobalEnv)

    showNotification(paste0("Thresholded data exported as: '", safe_name, "' in R session"), type = "message")
  })

  ###############################################################################################
  ###############################################################################################
  # map click options for generating the rectangle in turn to highlight erroneous fixes
  # by an a priori lat long extent
  ###############################################################################################
  ###############################################################################################

  # Initialize reactive values
  points <- reactiveVal(list())
  rect_state <- reactiveVal(0)  # 0 = waiting top-left, 1 = waiting bottom-right, 2 = drawn rectangle


  rect_active <- reactiveVal(FALSE)

  # Toggle rectangle mode
  observeEvent(input$activate_rect, {
    new_state <- !rect_active()   # flip state
    rect_active(new_state)

    if(!new_state){  # if toggled OFF
      leafletProxy("mymap2") %>% removeShape("selection_rect")
      rect_state(0)   # reset process to start
      # note: do NOT clear points() or text inputs
    }
  })

  # change button label dynamically
  observe({
    label <- if(rect_active()) "Rect Mode ON" else "Rect Mode OFF"
    updateActionButton(session, "activate_rect", label = label)
  })

  observeEvent(input$mymap2_click, {
    req(rect_active())  # only proceed if rectangle mode is on

    click <- input$mymap2_click
    clicks <- points()
    state <- rect_state()

    if(state == 0){   # first click -> top-left
      points(list(c(click$lat, click$lng)))
      rect_state(1)
    } else if(state == 1){  # second click -> bottom-right + draw
      clicks <- append(clicks, list(c(click$lat, click$lng)))
      points(clicks)

      # Compute rectangle bounds
      lat_min <- min(clicks[[1]][1], clicks[[2]][1])
      lat_max <- max(clicks[[1]][1], clicks[[2]][1])
      lng_min <- min(clicks[[1]][2], clicks[[2]][2])
      lng_max <- max(clicks[[1]][2], clicks[[2]][2])

      # Draw rectangle
      leafletProxy("mymap2") %>%
        removeShape("selection_rect") %>%   # clear previous
        addRectangles(
          lng1 = lng_min, lat1 = lat_max,   # top-left
          lng2 = lng_max, lat2 = lat_min,   # bottom-right
          fill = FALSE, color = "blue",
          layerId = "selection_rect"
        )

      # Update UI
      updateTextInput(session, "lat1", value = lat_max)
      updateTextInput(session, "lng1", value = lng_min)
      updateTextInput(session, "lat2", value = lat_min)
      updateTextInput(session, "lng2", value = lng_max)

      rect_state(2)   # rectangle drawn, next click = clear
    } else if(state == 2){  # third click -> just clear rectangle
      points(list())
      leafletProxy("mymap2") %>% removeShape("selection_rect")
      rect_state(0)  # reset to top-left
      #rect_active(FALSE)  # auto-disable mode

    }
  })

  # Reset button
  observeEvent(input$reset_rect, {
    # Clear clicks and rectangle
    points(list())
    rect_state(0)             # reset cycle to stage 0
    leafletProxy("mymap2") %>% removeShape("selection_rect")

    # Optionally clear the text inputs
    updateTextInput(session, "lat1", value = "")
    updateTextInput(session, "lng1", value = "")
    updateTextInput(session, "lat2", value = "")
    updateTextInput(session, "lng2", value = "")

    # Also remove ll_rm from the data if present
    df <- annotated_data()
    if (!is.null(df)) {
      df <- df %>% dplyr::select(-any_of("ll_rm"))
      annotated_data(df)
    }
  })


  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################

  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  # Tab 2 Temporal investigation
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################
  #############################################################################################

  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################
  ###########################################################################################################################

  # filtered_data_tag() filters by all the TagIDs the user selected
  # Then filtered_data() takes that and applies the time slider filter
  # The Leaflet observer reacts only to filtered_data()

  # --- Reactive: filter by TagID ---
  output$tid <- renderUI({
    pickerInput(
      inputId = "tid",
      label = "TagID:",
      choices = unique(data$TagID),
      selected = unique(data$TagID)[1],
      multiple = TRUE
    )
  })

  filtered_data_tag <- reactive({
    req(input$tid)
    if(is.null(tids) || length(tids) == 0){
      # show all data by default
      first_tid <- unique(data$TagID)[1]
      data %>% filter(TagID == first_tid)
    } else {
      data %>% filter(TagID %in% input$tid)
    }

  }) %>% debounce(200)

  ########################################################################################################
  # --- Time slider ---
  slider_vals <- reactiveVal(NULL)

  observeEvent(input$time_slider, {
    slider_vals(input$time_slider)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)  # <- prevents unnecessary updates


  # --- Render the slider once with defaults ---
  output$time_slider_ui <- renderUI({
    df <- filtered_data_tag()
    req(nrow(df) > 0)

    min_time <- min(df$DateTime)
    max_time <- max(df$DateTime)

    sliderInput(
      "time_slider", "Select time range:",
      min = min_time, max = max_time,
      value = c(min_time, max_time),
      timeFormat = "%Y-%m-%d %H:%M:%S",
      width = "100%"
    )
  })

  ##########
  # render a sister timeslider for the threshold explorer
  output$time_slider_threshold_ui <- renderUI({

    df <- filtered_annotated()

    if(nrow(df) == 0){
      df <- annotated_data()

      # get current tid selection
      if(is.null(input$tid2)){
        tid = unique(df$TagID)[1]
      } else{
        tid = input$tid2
      }
      df <- df[df$TagID %in% input$tid2,]

      if(nrow(df) == 0){
        df <- annotated_data()
      }
    }

    min_time <- min(df$DateTime)
    max_time <- max(df$DateTime)

    #browser()

    if(is.null(min_time)){browser()}
    if(is.null(max_time)){browser()}

    if(is.na(min_time)){
      df <- annotated_data()
      min_time <- min(df$DateTime)
      #browser()
    }
    if(is.na(max_time)){
      #browser()
      df <- annotated_data()
      max_time <- max(df$DateTime)
    }


    #browser()
    sliderInput(
      "time_slider_threshold", "Select time range:",
      min = min_time,
      max = max_time,
      value = c(min_time, max_time),
      timeFormat = "%Y-%m-%d %H:%M:%S",
      width = "100%"
    )
  })

  #############
  #observeEvent(input$time_slider, {
  #  req(input$time_slider)
  #  updateSliderInput(session, "time_slider_threshold",
  #                    value = input$time_slider)
  #}, ignoreInit = TRUE, ignoreNULL = TRUE)
  #
  #observeEvent(input$time_slider_threshold, {
  #  req(input$time_slider_threshold)
  #  updateSliderInput(session, "time_slider",
  #                    value = input$time_slider_threshold)
  #}, ignoreInit = TRUE, ignoreNULL = TRUE)

  #sync_time_flag <- reactiveVal(FALSE)

  observeEvent(input$time_slider, {
    if(isTRUE(input$link_maps)){
      updateSliderInput(session, "time_slider_threshold", value = input$time_slider)
    }

  }, ignoreInit = TRUE)

  observeEvent(input$time_slider_threshold, {

    if(isTRUE(input$link_maps)){
      updateSliderInput(session, "time_slider", value = input$time_slider_threshold)
    }

  }, ignoreInit = TRUE)


  ############

  # --- Update slider bounds whenever TagID changes ---
  observeEvent(filtered_data_tag(), {
    #browser()
    df <- filtered_data_tag()
    min_time <- min(df$DateTime)
    max_time <- max(df$DateTime)

    # keep user selection if possible
    current <- slider_vals()
    if (is.null(current)) {
      new_val <- c(min_time, max_time)
    } else {
      new_val <- c(
        max(min_time, current[1]),
        min(max_time, current[2])
      )
    }

    updateSliderInput(
      session,
      "time_slider",
      min = min_time,
      max = max_time,
      value = new_val
    )
  })

  # --- Store user changes safely ---
  observeEvent(input$time_slider, {
    slider_vals(input$time_slider)
  }, ignoreInit = TRUE)

  # --- Filtered data ---
  filtered_data <- reactive({
    req(input$time_slider)
    df <- filtered_data_tag()
    df %>% filter(DateTime >= input$time_slider[1],
                  DateTime <= input$time_slider[2])
  }) %>% debounce(200)



  ########################################################################################################
  # reactiveValues guard
  sync_flag <- reactiveVal(FALSE)

  observeEvent(input$tid, {
    req(input$tid)
    if(!sync_flag()) {
      sync_flag(TRUE)

      # Keep only IDs that actually exist in tid2 choices
      #valid_choices <- input$tid2_choices  # store the current choices for tid2
      #new_selection <- intersect(input$tid, valid_choices)
      #updatePickerInput(session, "tid2", selected = new_selection)

      updatePickerInput(session, "tid2", selected = input$tid)
      sync_flag(FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$tid2, {
    req(input$tid2)
    if(!sync_flag()) {
      sync_flag(TRUE)

      # Keep only IDs that exist in tid choices
      #valid_choices <- input$tid_choices  # store current choices for tid
      #new_selection <- intersect(input$tid2, valid_choices)
      #updatePickerInput(session, "tid", selected = new_selection)

      updatePickerInput(session, "tid", selected = input$tid2)
      sync_flag(FALSE)
    }
  }, ignoreInit = TRUE)
  ########################################################################################################

  # reactiveValues to track variables to plot
  rv <- reactiveValues(
    current_xy_vars = character(0)  # start empty
  )

  # --------------- Covariate selection UI ---------------- #
  output$covariates_ui <- renderUI({
    df <- filtered_data_tag()
    req(nrow(df) > 0)

    # available numeric variables
    num_vars <- names(df)[sapply(df, is.numeric) & !(names(df) %in% c("latitude", "longitude"))]

    pickerInput(
      "xy_vars", "Select numeric covariates:",
      choices = num_vars,
      selected = rv$current_xy_vars,  # show currently tracked variables
      multiple = TRUE
    )
  })



  # Update tracked variables when user changes selection
  #observeEvent(input$xy_vars, {
  #  if (is.null(rv$current_xy_vars)) {
  #    rv$current_xy_vars <- input$xy_vars
  #  } else {
  #    # Merge new selections with existing
  #    rv$current_xy_vars <- unique(c(rv$current_xy_vars, input$xy_vars))
  #  }
  #}, ignoreNULL = FALSE)



  observeEvent(input$xy_vars, {
    req(input$xy_vars)

    # update reactiveValues with **exact selection**, not merging with old
    rv$current_xy_vars <- input$xy_vars
  }, ignoreNULL = FALSE)


  output$xy_plots_ui <- renderUI({
    #req(input$xy_vars)
    #req(rv$current_xy_vars)
    req(debounced_xy_vars())

    #plot_list <- lapply(input$xy_vars, function(var) {
    plot_list <- lapply(rv$current_xy_vars, function(var) {
      tagList(
        conditionalPanel(
          condition = "input.plot_type == 'ggplot'",
          plotOutput(outputId = paste0("xy_", var, "_gg"), height = paste0(input$plot_height, "px"))
        ),
        conditionalPanel(
          condition = "input.plot_type == 'plotly'",
          plotlyOutput(outputId = paste0("xy_", var, "_plotly"), height = paste0(input$plot_height, "px"))
        )
      )
    })
    do.call(tagList, plot_list)
  })


  #xy_plot_data <- reactive({
  #  df <- filtered_data() #%>% debounce(400)  # Debounce to avoid multiple triggers
  #  vars <- rv$current_xy_vars
  #  list(df = df, vars = vars)
  #})

  debounced_xy_vars <- reactive({
    rv$current_xy_vars
  }) %>% debounce(300)   # 300 ms delay to avoid rapid triggers

  xy_plot_data <- reactive({
    df <- filtered_data()  # already filtered / subset by TagID / time
    vars <- debounced_xy_vars()  # use debounced variables
    list(df = df, vars = vars)
  })

  # --- Render each plot ---
  observeEvent(xy_plot_data(),{
    #req(input$xy_vars)

    #req(rv$current_xy_vars)

    data_list <- xy_plot_data()
    df <- data_list$df
    vars <- data_list$vars
    pal <- tag_palette()


    #for(var in input$xy_vars){
    #for(var in rv$current_xy_vars){
    for(var in vars){
      local({ #local() keeps each variable’s render isolated
        v <- var

        #if(input$plot_type == "plotly"){

          output[[paste0("xy_", v, "_plotly")]] <- renderPlotly({
            #df <- filtered_data()
            #df_local <- isolate(df)  # Use isolate() to avoid re-triggering when filtered_data() changes; note debounced already above though in xy_plot_data()
            #df_local <- data_list$df

            req(nrow(df) > 0)

            # build ggplot
            #suppressWarnings(
              p <- ggplot(df, aes(
                x = DateTime,
                y = .data[[v]],
                color = TagID,
                text = paste0(
                  "TagID: ", TagID,
                  "<br>DateTime: ", DateTime,
                  "<br>", v, ": ", round(.data[[v]], 2)
                )
              )) +
                geom_line(na.rm = TRUE) +
                geom_point(size = input$plot_point_size, na.rm = TRUE) +  # ensures hoverable points
                scale_color_manual(values = pal(sort(unique(df$TagID)))) +
                labs(y = v, x = "", col = "TagID") +
                theme_minimal() +
                #theme(legend. position = “none”)  +
                guides(color = "none")
            #)
            # convert ggplot -> plotly
            ggplotly(p, tooltip = "text") %>%
              layout(legend = list(orientation = "h", x = 0, y = -0.2))
          })
        #}

        #if(input$plot_type == "ggplot"){
          output[[paste0("xy_", v, "_gg")]] <- renderPlot({
            #df <- filtered_data()
            #df_local <- isolate(df)

            #df_local <- data_list$df
            req(nrow(df) > 0)

            #suppressWarnings(
              ggplot(df, aes(
                x = DateTime,
                y = .data[[v]],
                color = TagID
              )) +
                geom_line(na.rm = TRUE) +
                geom_point(size = input$plot_point_size, na.rm = TRUE) +
                scale_color_manual(values = pal(sort(unique(df$TagID)))) +
                labs(y = v, x = "", col = "TagID") +
                theme_minimal() +
                guides(color = "none")
             #)
          })

       # }
      })
    }
  })




  # ---------------------------------------------------------------------- #
  # leaflet map reactive
  map_reactive <- reactive({

    if("GoogleEarth" %in% Providers){
      Providers_GE <- "GoogleEarth"
      Providers <- Providers[Providers != "GoogleEarth"]
    }

    m = leaflet::leaflet() %>% leaflet::leaflet(
      options = leaflet::leafletOptions(
        preferCanvas = TRUE, zoomControl = FALSE,  attributionControl=FALSE
      )
    )

    # ADDITIONAL
    # moving the +- button: https://stackoverflow.com/questions/71013017/move-zoom-controls-in-leaflet-for-r-shiny
    # this is to allow the trips selector menu to sit more neatly at the top left of the map
    #if(input$zoom_control){
    m <- m %>%
      htmlwidgets::onRender( # java script code goes in the JsCode arg
        "function(el, x) {
                              L.control.zoom({position:'topright'}).addTo(this);
                          }")

    #}

    # --------------------------- #
    # Layer tile options
    # --------------------------- #

    # add Google Earth Layer if exists, and make sure layer names reflect this addition
    if(exists("Providers_GE")){
      m <- m %>% leaflet::addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
                                   attribution = 'Google',
                                   options = leaflet::providerTileOptions(updateWhenIdle = TRUE, updateWhenZooming = FALSE))  %>%
        leafem::addMouseCoordinates()
    }
    for(i in 1:length(Providers)){
      m <- m %>% leaflet::addProviderTiles(Providers[i], group = Providers[i],
                                           options = leaflet::providerTileOptions(
                                             updateWhenIdle = FALSE,
                                             updateWhenZooming = FALSE,
                                             providerTileOptions(zIndex=-10), # because TIA raster overlays do not work otherwise: https://stackoverflow.com/questions/34159578/raster-image-goes-below-base-layer-while-markers-stay-above-xindex-is-ignored
                                             updateWhenZooming = FALSE)
      )
    }
    if(exists("Providers_GE")){
      Providers <- c(Providers, "GoogleEarth")
    }
    m <- m %>% leaflet::addLayersControl(
      baseGroups = Providers,
      #overlayGroups = "overlay",
      options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% # autoZIndex because TIA raster overlays do not work otherwise
      #leaflet::setView(lat = 55, lng = -4, zoom = 9) %>%
      leaflet::mapOptions(zoomToLimits = "never") %>%
      leaflet::addScaleBar(position = "bottomleft")

    ########## Look into this
    #?leaflet.extras2::antpathOptions

    if(!is.null(data)){

      m <- m %>% leaflet::setView(lat = mean(data$latitude,na.rm=TRUE), lng = mean(data$longitude,na.rm=TRUE), zoom = 9)
    } else{
      m <- m %>% leaflet::setView(lat = 56.1, lng = -3.2, zoom = 7)
    }


    # add right mouse click
    # https://stackoverflow.com/questions/60750953/r-shiny-leaflet-right-click-context-menu
    #m <- m  %>%
    #  htmlwidgets::onRender("
    #function(el,x) {
    #    mymap = this;
    #    mymap.on('contextmenu', function(e) {
    #    var coords = {lng: e.latlng.lng, lat: e.latlng.lat}
    #    Shiny.setInputValue('mymap_right_click', coords);
    #});
    #}
    #")


    m <- m %>%
          htmlwidgets::onRender("
      function(el, x) {
        var map = this;

        // Left click
        map.on('click', function(e) {
          Shiny.setInputValue('mymap_click', {lat: e.latlng.lat, lng: e.latlng.lng}, {priority: 'event'});
        });

        // Right click (context menu)
        map.on('contextmenu', function(e) {
          Shiny.setInputValue('mymap_right_click', {lat: e.latlng.lat, lng: e.latlng.lng}, {priority: 'event'});
        });
      }
      ")

    m$dependencies <- c(m$dependencies, leaflet:::leafletAwesomeMarkersDependencies())

    #m <- m %>%   addEasyprint(options = easyprintOptions( exportOnly = TRUE ))

    m

  })

  output$mymap <- leaflet::renderLeaflet({
    map_reactive()
  })


  # Reactive: create multilinestring per TagID from filtered data
  sf_lines_reactive <- reactive({
    #df <- filtered_data() %>% arrange(TagID, DateTime)

    df <- filtered_data() %>% arrange(TagID, DateTime) %>% dplyr::ungroup()

    req(nrow(df) > 1)  # need at least two points to make a line

    # Create multilinestring (one line per TagID)
    lines_sf <- sfheaders::sf_multilinestring(
      obj = df,
      x = "longitude",
      y = "latitude",
      multilinestring_id = "TagID"
    )

    sf::st_agr(lines_sf) <- "constant"
    lines_sf

    # leafgl test
    #sf::st_agr(lines_sf) <- "constant"
    #lines_sf <- sf::st_cast(lines_sf, "LINESTRING") # for leafgl
    #lines_sf


  })

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  # GPS lines

  observe({

    df <- filtered_data()
    req(df)
    req(nrow(df) > 0)
    pal <- tag_palette()

    # get list of TagIDs in the reactive
    tags <- sort(unique(df$TagID))

    proxy <- leafletProxy("mymap")

    # Remove existing polylines
    proxy %>% clearGroup("trajectories_ti")

    if(isTRUE(input$show_lines_ti)) {

      lines_sf <- sf_lines_reactive()

      proxy %>%  addPolylines(
        data = lines_sf,
        color = ~pal(TagID),
        weight = input$line_size_ti,
        opacity = input$line_opacity_ti,
        group = "trajectories_ti"

      )

    } else{
      proxy %>% leafgl::removeGlPolylines(layerId = "trajectories_ti")
    }
  })

  # leafgl test
#  observe({
#
#    df <- filtered_data()
#    req(df)
#    req(nrow(df) > 0)
#    pal <- tag_palette()
#
#    # get list of TagIDs in the reactive
#    tags <- sort(unique(df$TagID))
#
#    proxy <- leafletProxy("mymap")
#    #proxy <- leafgl::leafglProxy("mymap")
#
#    # Remove existing polylines
#    #proxy %>% clearGroup("trajectories_ti")
#    proxy <- proxy %>% leafgl::removeGlPolylines(layerId = "trajectories_ti")
#
#    if(isTRUE(input$show_lines_ti)) {
#
#      lines_sf <- sf_lines_reactive()
#
#      #proxy %>%  addPolylines(
#      proxy %>%  leafgl::addGlPolylines(
#        data = lines_sf,
#        color = ~pal(TagID),
#        weight = input$line_size_ti,
#        opacity = input$line_opacity_ti,
#        #group = "trajectories_ti"
#        layerId = "trajectories_ti"
#      )
#
#    } else{
#      proxy %>% leafgl::removeGlPolylines(layerId = "trajectories_ti")
#    }
#  })

  # - o - o - o - o - o - o - o - o - o - o - o - o - o - o #
  # GPS fixes
  df_debounced <- debounce(filtered_data, 500)  # 500 ms delay


  observe({
    df <- as.data.frame(df_debounced()) # for leafgl

    req(df)
    #req(nrow(df) > 0)
    req(!is.null(df), nrow(df) > 0)
    pal <- tag_palette()

    # get list of TagIDs in the reactive
    tags <- sort(unique(df$TagID))

    proxy <- leafletProxy("mymap")

    # update map
    if(isTRUE(input$show_points_ti)) {
      proxy %>%
        clearMarkers() %>%
        addCircleMarkers(
          data=df, ~longitude, ~latitude,
          popup = ~paste0(TagID, ": ", DateTime),
          radius = input$point_size_ti,
          color=pal(df$TagID),
          fillOpacity=input$point_opacity_ti
        )
    } else{
      proxy %>%
        clearMarkers()
    }

  })

  # leafgl experiment, no joy
#  observe({
#    df <- as.data.frame(df_debounced())
#    req(nrow(df) > 0)
#
#    pal <- tag_palette()
#    proxy <- leafletProxy("mymap")
#
#    if(isTRUE(input$show_points_ti)) {
#
#      # Optional: limit points for performance
#      max_points <- 5000
#      if(nrow(df) > max_points) df <- df[sample(nrow(df), max_points), ]
#
#      points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
#
#      proxy %>%
#       leafgl::removeGlPoints(layerId = "points") %>%
#        leafgl::addGlPoints(
#          data = points_sf,             # data.frame
#          #x = df$longitude,      # numeric vector of longitudes
#          #y = df$latitude,       # numeric vector of latitudes
#          popup = paste0(points_sf$TagID, ": ", points_sf$DateTime),
#          radius = input$point_size_ti,
#          color = pal(points_sf$TagID),
#          fillOpacity = input$point_opacity_ti,
#          layerId = "points"
#        )
#
#    } else {
#      proxy %>% leafgl::removeGlPoints(layerId = "points")
#    }
#  })

  ############## LEGEND ###################
  observeEvent(filtered_data(),{
    # Only proceed if legend toggle is TRUE
    req(input$show_legend_ti)

    df <- filtered_data()
    req(nrow(df) > 0)

    pal <- tag_palette()
    tags <- sort(unique(df$TagID))

    proxy <- leafletProxy("mymap")

    # Always clear existing legend first
    proxy %>% clearControls()

    # Add updated legend
    proxy %>% addLegend(
      pal = pal,
      values = tags,
      opacity = 1,
      title = "Animals"
    )
  })

  ########################################################################################################
  # collapsible histogram box

  # Dynamic selectInput based on available numeric variables
  # Track if user has overridden the histogram variable
  rv_hist <- reactiveValues(selected = NULL, user_override = FALSE)

  # Update when user explicitly changes the histogram selector
  observeEvent(input$hist_var, {
    rv_hist$selected <- input$hist_var
    rv_hist$user_override <- TRUE
  })

  # Update when xy_vars change (but only if no override)
  observeEvent(xy_plot_data()$vars, {
    vars <- xy_plot_data()$vars
    if(length(vars) > 0 && !rv_hist$user_override) {
      rv_hist$selected <- vars[1]
    }
  }, ignoreNULL = TRUE)

  # Histogram plot
  output$hist_plot <- renderPlot({
    data_list <- xy_plot_data()
    df <- data_list$df
    req(nrow(df) > 0)
    req(rv_hist$selected)
    req(input$hist_bins)

    var <- rv_hist$selected
    df <- df %>% filter(!is.na(.data[[var]]))

    # log transform if chosen
    if(input$hist_scale == "log10") {
      df <- df %>% filter(.data[[var]] > 0) %>%
        mutate(value = log10(.data[[var]]))
        xlab <- paste0("log10(", var, ")")
    } else if(input$hist_scale == "log") {
      df <- df %>% filter(.data[[var]] > 0) %>%
        mutate(value = log(.data[[var]]))
        xlab <- paste0("log(", var, ")")
    } else if(input$hist_scale == "sqrt") {
      df <- df %>% filter(.data[[var]] > 0) %>%
        mutate(value = sqrt(.data[[var]]))
      xlab <- paste0("sqrt(", var, ")")
    } else{
      df <- df %>% mutate(value = .data[[var]])
      xlab <- var
    }


    if(!is.na(input$trunc_hist_upper)){
      if(!is.null(input$trunc_hist_upper)){
        df <- df %>% filter(value <= input$trunc_hist_upper)
      }
    }
    if(!is.na(input$trunc_hist_lower)){
      if(!is.null(input$trunc_hist_lower)){
        df <- df %>% filter(value >= input$trunc_hist_lower)
      }
    }

    pal <- tag_palette()

    ggplot(df, aes(x = value, fill = TagID)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = input$hist_bins) +
      scale_fill_manual(values = pal(unique(df$TagID))) +
      facet_wrap(~TagID, ncol = 3, scales = "free_y") +
      theme_minimal(base_size = 13) +
      labs(x = xlab, y = "Count", title = paste("Histogram of", var)) +
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold")
      )

    #  ggplot(df, aes(x = value, fill = TagID)) +
    #  geom_histogram(position = "dodge", bins = 20, color = "black", alpha = 0.7) +
    #  scale_fill_manual(values = pal(unique(df$TagID))) +
    #  theme_minimal(base_size = 13) +
    #  labs(x = xlab, y = "Count", title = paste("Histogram of", var)) +
    #  theme(legend.position = "right")



  })

  observe({
    df <- filtered_data_tag()
    req(nrow(df) > 0)

    num_vars <- names(df)[sapply(df, is.numeric) & !(names(df) %in% c("latitude", "longitude"))]

    updateSelectInput(session, "hist_var", choices = num_vars)

    data_list <- xy_plot_data()
    if(!is.null(data_list$vars)){
      updateSelectInput(session, "hist_var", selected = data_list$vars[1])
    }
  })



}
