
ui <- dashboardPage(

  dashboardHeader(title = "ShinyRakeR"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Rake Overview", tabName = "rake_overview", icon = icon("leaf")),
      menuItem("Threshold Explorer", tabName = "threshold_explorer", icon = icon("sliders-h")),
      menuItem("Temporal Inspector", tabName = "temporal_inspect", icon = icon("clock")),

      checkboxInput("link_maps", "Link maps to each other", value = FALSE)

    )
  ),
  dashboardBody(
    shinybusy::add_busy_spinner(spin = "fading-circle", timeout = 200),

    tabItems(
      # ---- Tab 1: General Rake ----
      #tabItem(tabName = "rake_overview",
      #        fluidRow(
      #          box(title = "Data Summary (rake)", status = "primary", width = 12,
      #              plotOutput("rake_plot", height = 400)
      #          )
      #        )
      #),

      # ---- Tab 1: Threshold Explorer ----
      tabItem(tabName = "threshold_explorer",
              fluidRow(

                column(
                  width = 4,
                  #div(
                    #style = "overflow-y: auto; max-height: 90vh; padding-right: 10px;",

                      box(title = "Threshold and gap controls", status = "warning", width = 12,
                          height = "100%",  # full column height
                          # Step 1: Optional Gap Assignment
                          wellPanel(
                            tags$h4("Step 1: Identify Gaps (Optional)", style = "color: steelblue;"),
                            #checkboxInput("do_gap_section", "Run gap_section() first?", value = FALSE),
                            numericInput("gap_thresh", "Gap threshold (seconds):", value = 3600, min = 1, step = 60),
                            numericInput("gap_tol", "Tolerance (seconds):", value = 0.2, min = 0, step = 0.01),
                            actionButton("run_gap_section", "Run gap_section()"),
                            uiOutput("gap_done_ui"),
                            tags$p("Set gap parameters before applying annotation filters.", style = "font-size: 12px; color: gray;")
                          ),

                          # Annotation / Threshold Sliders
                          wellPanel(
                            tags$h4("Step 2: Annotate data", style = "color: darkgreen;"),
                            uiOutput("custom_flag_ui"),

                            # 0. Too few data per animal
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 3;",
                                  sliderInput("fix_thresh_slider", "Number of fixes/animal:",
                                              min = 0, max = 100, value = 5) # that upper value may need to be flexible
                              ),
                              div(style = "flex: 1;",
                                  numericInput(
                                    "fix_thresh_numeric",
                                    "",
                                    value = 5, min = 0, max = 100, step = 1
                                  )
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("run_fixflag", "Run fix/animal", style = "width: 120px;")
                              )
                            ),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                            # 1. NA Lat long
                            div(
                              style = "display: flex; align-items: center;",
                              div(style = "flex: 1;",
                                  htmltools::h5(tags$b("Missing latitute/longitude:"))
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("close_warn_1", "", style = "width: 30px;", icon = icon("xmark"))
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("run_na_ll", "Flag NAs", style = "width: 120px;")
                              )
                              #,
                              #div(style = "flex: 0 0 auto;",
                              #    actionButton("clear_ll", "clear", style = "width: 60px;") #, icon = icon("ban"))
                              #)
                            ),
                            uiOutput("na_warning"),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),

                            # 2. dup data
                            div(
                              style = "display: flex; align-items: center;",
                              div(style = "flex: 1;",
                                  htmltools::h5(tags$b("Duplicate DateTime stamps:"))
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("run_dup", "Duplicates", style = "width: 120px;")
                              )
                            ),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),

                            # 3. Nsats
                            div(
                              style = "margin-bottom: 12px;",  # space below this block

                              # --- First row: label + dropdown ---
                              div(
                                style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                                div(style = "flex: 1;",
                                    htmltools::h5(tags$b("Number of satellites:"))
                                ),
                                div(style = "flex: 2;",
                                    uiOutput("sel_sat_col")
                                )
                              ),

                              # --- Second row: slider + numeric + button ---
                              div(
                                style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin-top: 6px;",
                                div(style = "flex: 3;",
                                    sliderInput("nsat_thresh_slider", NULL,
                                                min = 3, max = 6, value = 4)
                                ),
                                div(style = "flex: 1;",
                                    numericInput("nsat_thresh_numeric", NULL,
                                                 value = 4, min = 0, max = 8, step = 1)
                                ),
                                div(style = "flex: 0 0 auto;",
                                    actionButton("run_nsats", "Run nsats", style = "width: 120px;")
                                )
                              )
                            ),


                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),


                            # 4. PDOP filter + button
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 3;",
                                  sliderInput("pdop_thresh", "PDOP filter:",
                                              min = 0, max = 10, value = 5)
                              ),
                              div(style = "flex: 1;",
                                  numericInput(
                                    "pdop_thresh_numeric",
                                    "",
                                    value = 5, min = 0, max = 10, step = 0.01
                                  )
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("run_pdop", "Run pdop", style = "width: 120px;")
                              )
                            ),
                            uiOutput("pdop_warning"),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),

                            # 5. HDOP filter + button
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 3;",
                                  sliderInput("hdop_thresh", "HDOP filter:",
                                              min = 0, max = 10, value = 5)
                              ),
                              div(style = "flex: 1;",
                                  numericInput(
                                    "hdop_thresh_numeric",
                                    "",
                                    value = 5, min = 0, max = 10, step = 0.01
                                  )
                              ),
                              div(style = "flex: 0 0 auto;",
                                  actionButton("run_hdop", "Run hdop", style = "width: 120px;")
                              )
                            ),
                            uiOutput("hdop_warning"),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),

                            # 6. Single gapsections
                            #fluidRow(
                            #  style = "align-items: centre;",
                            #  column(8,
                            #         htmltools::h5(tags$b("Single gapsections:"))
                            #  ),
                            #  column(4,
                            #         div(style = "text-align: right;",
                            #             actionButton("run_orph", "Single gapsecs", style = "width: 120px;")
                            #             ))
                            #),
                            div(style = "display: flex; align-items: centre;",
                                div(style = "flex: 1;",
                                    htmltools::h5(tags$b("Single gapsections:"))
                                ),
                                div(style = "flex: 0 0 auto;",
                                    actionButton("close_warn_2", "", style = "width: 30px;", icon = icon("xmark"))
                                ),
                                div(style = "flex: 0 0 auto;",
                                    actionButton("run_orph", "Single gapsecs", style = "width: 120px;")
                                )
                            ),
                            uiOutput("singlegap_warning"),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                            # 7. Iterative trajectory speed
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 3;",  # slider takes remaining width
                                  sliderInput("speed_thresh", "Trajectory speed (m/s):",
                                              min = 0, max = 500, value = 50)
                              ),
                              div(style = "flex: 1;",
                                  numericInput(
                                    "speed_thresh_numeric",
                                    "",
                                    value = 50, min = 0, max = 500, step = 0.01
                                  )
                              ),
                              div(style = "flex: 0 0 auto;",  # button keeps its natural width
                                  actionButton("run_speed", "Run speed_filt()", style = "width: 120px;")
                              )
                            ),

                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                            # 8. Simple trajectory speed
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 3;",  # slider takes remaining width
                                  sliderInput("custom_speed_thresh", "Trajectory speed (m/s):",
                                              min = 0, max = 500, value = 50)
                              ),
                              div(style = "flex: 1;",
                                  numericInput(
                                    "custom_speed_thresh_numeric",
                                    "",
                                    value = 50, min = 0, max = 500, step = 0.01
                                  )
                              ),
                              div(style = "flex: 0 0 auto;",  # button keeps its natural width
                                  actionButton("run_custom_speed", "Custom speed", style = "width: 120px;")
                              )
                            ),
                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                            # 9. Turning angle speed
                            div(
                              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                              div(style = "flex: 2;",
                                  sliderInput(
                                    "turn_thresh_slider",
                                    "Turning angle (rads):",
                                    min = 0, max = round(pi, 3), value = pi/2, step = 0.01)
                                ),
                                div(style = "flex: 1;",
                                      numericInput(
                                        "turn_thresh_numeric",
                                        "",
                                        value = pi/2, min = 0, max = round(pi, 3), step = 0.01
                                      )
                                ),
                                div(style = "flex: 1;",
                                  radioButtons(
                                    inputId = "turn_dir",
                                    label = "Select level",
                                    choices = c("less_than", "greater_than"),
                                    selected = "less_than",
                                    inline = FALSE
                                  ),
                                div(style = "flex: 0 0 auto;",
                                    actionButton("run_turn", "Run turn_filt()", style = "width: 120px;")
                                )
                              )
                            ),
                            hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                            # 10. lat long extent selector
                            div(
                              style = "display: flex; flex-direction: column; gap: 5px; width: 100%; max-width: 100%; box-sizing: border-box;",

                              htmltools::h5(tags$b("Select xy extent for valid fixes:")),

                              # Upper Left Row
                              div(
                                style = "display: flex; gap: 5px; align-items: center; width: 100%;",
                                div(style = "width: 80px;", htmltools::h5(tags$b("Upper left:"))),
                                div(style = "flex: 1; min-width: 0;", numericInput("lat1", "Latitude", "")),
                                div(style = "flex: 1; min-width: 0;", numericInput("lng1", "Longitude", ""))
                              ),

                              # Lower Right Row
                              div(
                                style = "display: flex; gap: 5px; align-items: center; margin-top: 5px; width: 100%;",
                                div(style = "width: 80px;", htmltools::h5(tags$b("Lower right:"))),
                                div(style = "flex: 1; min-width: 0;", numericInput("lat2", "Latitude", "")),
                                div(style = "flex: 1; min-width: 0;", numericInput("lng2", "Longitude", ""))
                              ),

                              # Button Row
                              div(
                                style = "display: flex; gap: 10px; margin-top: 10px; width: 100%; max-width: 100%; box-sizing: border-box;",
                                div(style = "flex: 1 1 0; min-width: 0;", actionButton("activate_rect", "Activate Rectangle", style = "width: 100%;")),
                                div(style = "flex: 1 1 0; min-width: 0;", actionButton("reset_rect", "Reset Rectangle", style = "width: 100%;")),
                                div(style = "flex: 1 1 0; min-width: 0;", actionButton("run_ll", "Run extent filter", style = "width: 100%;"))
                              )
                            )
                          )
                      )
                    #) # div
                ), # column
                column(
                  width = 8,
                  fluidRow(
                    box(title = "Leaflet Map", status = "info", width = 12,
                        div(
                          style = "display: flex; gap: 10px; align-items: center;",
                          #uiOutput("tid_dropdown_ui2"),
                          shinyWidgets::dropdown(
                            inputId = "nn2",
                            label = "Select animals",
                            shiny::uiOutput("tid2"),
                            #style = "unite",
                            status = "warning",
                            icon = icon("earlybirds"),
                            width = "150px"
                          ),
                          shinyWidgets::dropdown(
                              inputId = "flag_filter_dd",
                              label = "Filter 'bad' points",
                              icon = icon("filter"),
                              width = "150px",
                             # tooltip = "Filter annotated GPS fixes",
                             #style = "unite",
                             status = "danger",
                              tagList(
                                uiOutput("flag_source_ui"),
                                radioButtons(
                                  inputId = "flag_filter",
                                  label = "Select level",
                                  choices = c("Flagged", "OK", "NA", "All"),
                                  selected = "All",
                                  inline = FALSE
                                )
                              )
                          ), #dropdown close
                          shinyWidgets::dropdown(
                              inputId = "map_settings",
                              label = "Map settings",
                              icon = icon("sliders-h"),
                              status = "primary",
                              width = "250px",
                             # tooltip = "Adjust map point display",
                              tagList(
                                sliderInput(
                                  inputId = "point_size",
                                  label = "Point size:",
                                  min = 1, max = 10, value = 2, step = 1
                                ),
                                sliderInput(
                                  inputId = "point_opacity",
                                  label = "Point opacity:",
                                  min = 0.1, max = 1, value = 0.7, step = 0.01
                                ),
                                sliderInput(
                                  inputId = "line_size",
                                  label = "Line size:",
                                  min = 0.1, max = 10, value = 2, step = 0.1
                                ),
                                sliderInput(
                                  inputId = "line_opacity",
                                  label = "Line opacity:",
                                  min = 0.1, max = 1, value = 0.5, step = 0.01
                                ),
                                checkboxInput("show_lines", "Show trajectories", value = TRUE),
                                checkboxInput("show_points", "Show GPS fixes", value = TRUE),
                                checkboxInput("show_legend", "Show Legend", value = TRUE)
                              )
                            ),
                          # UI
                          shinyWidgets::dropdown(
                            inputId = "export_dropdown",
                            label = "Export data",
                            icon = icon("download"),
                            status = "success",
                            width = "250px",
                            up = FALSE,
                            tagList(
                              textInput("export_name", "Dataset name:", value = "data_annotated", width = "100%"),
                              actionButton(
                                "export_threshold",
                                "Export to R session",
                                icon = icon("arrow-up-right-from-square"),
                                width = "100%",
                                class = "btn-success"
                              )
                            )
                          )
                        ), # divider close
                        leafletOutput("mymap2", height = "700px"),
                        #leafgl::leafglOutput("mymap2", height = "700px"),
                        uiOutput("time_slider_threshold_ui")
                    ), # leaflet box close
                    box(
                      title = "Summary 'rake'",
                      status = "primary",
                      width = 12,
                      #fluidRow(
                        #column(
                        #box(title = "Leaflet Map", status = "info", width = 12,
                      div(
                        style = "display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-start;",
                        width = 12,
                        uiOutput("rake_circles")   # dynamically rendered circle summaries
                       #)
                        #)
                      ),
                      hr(style = "border-top: 1px solid #ddd; margin: 8px 0;"),
                      #fluidRow(
                      div(
                        #style = "width: 100%; margin-top: 10px; max-height: 300px; overflow-y: auto; overflow-x: auto;",
                        style = "width: 100%; margin-top: 10px; overflow-x: auto;",

                        #column(
                        #width = 12,
                        DT::dataTableOutput("rake_summary_table")  # dynamic table of animals/fixes & params
                      )
                    ) # Rake box close
                  ) # fluidRow inside container close
                ) # column close for right hand panel
              ) # fluidRow close
      ), # tab item
      # ---- Tab 2: Temporal Inspector ----
      tabItem(tabName = "temporal_inspect",

              fluidRow(
                column(5,
                       # Side-by-side covariate selector and plot height slider
                       fluidRow(
                         column(5,
                                uiOutput("covariates_ui")  # covariate selection
                         ),
                         column(4,
                                sliderInput(
                                  inputId = "plot_height",
                                  label = "XY plot height (px):",
                                  min = 80,
                                  max = 400,
                                  value = 150,
                                  step = 10
                                )
                         ),
                         column(3,
                                sliderInput(
                                  inputId = "plot_point_size",
                                  label = "XY point size:",
                                  min = 0.1,
                                  max = 3,
                                  value = 0.6,
                                  step = 0.1
                                )
                         )
                       ),
                       # Dynamic XY plots below the controls
                       uiOutput("xy_plots_ui")
                ),
                column(7,
                       fluidRow(
                         box(title = "Leaflet Map", status = "info", width = 12,
                             div(
                               style = "display: flex; gap: 10px; align-items: center;",
                                 #uiOutput("tid_dropdown_ui"),
                                 shinyWidgets::dropdown(
                                   inputId = "nn",
                                   label = "Select animals",
                                   shiny::uiOutput("tid"),
                                   #style = "unite",
                                   status = "warning",
                                   icon = icon("earlybirds"),
                                   width = "200px"
                                 ),
                               shinyWidgets::dropdown(
                                 inputId = "map_settings_ti",
                                 label = "Map settings",
                                 icon = icon("sliders-h"),
                                 status = "primary",
                                 width = "250px",
                                 #tooltip = "Adjust map point display"
                                 tagList(
                                   radioButtons(
                                     inputId = "plot_type",
                                     label = "Select plot type",
                                     choices = c("ggplot", "plotly"),
                                     selected = "ggplot",
                                     inline = FALSE
                                   ),
                                   sliderInput(
                                     inputId = "point_size_ti",
                                     label = "Point size:",
                                     min = 1, max = 10, value = 2, step = 1
                                   ),
                                   sliderInput(
                                     inputId = "point_opacity_ti",
                                     label = "Point opacity:",
                                     min = 0.1, max = 1, value = 0.7, step = 0.01
                                   ),
                                   sliderInput(
                                     inputId = "line_size_ti",
                                     label = "Line size:",
                                     min = 0.1, max = 10, value = 2, step = 0.1
                                   ),
                                   sliderInput(
                                     inputId = "line_opacity_ti",
                                     label = "Line opacity:",
                                     min = 0.1, max = 1, value = 0.7, step = 0.01
                                   ),
                                   checkboxInput("show_lines_ti", "Show trajectories", value = TRUE),
                                   checkboxInput("show_points_ti", "Show GPS fixes", value = TRUE),
                                   checkboxInput("show_legend_ti", "Show Legend", value = TRUE)
                                 )
                               )
                             ), # divider close
                        leafletOutput("mymap", height = "700px"),
                        #leafgl::leafglOutput("mymap", height = "700px"),
                        uiOutput("time_slider_ui")
                        ), # leaflet box close
                      ) # fluidRow
                )
              ),
              fluidRow(
                column(12,
                       # variable histogram box, collapsible
                       box(
                          title = "Histogram distribution of variables",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = 12,

                          div(
                            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                            div(style = "flex: 1;",
                                selectInput(
                                  "hist_var",
                                  "Histogram variable:",
                                  choices = NULL  # will populate dynamically
                                )
                            ),
                            div(style = "flex: 1;",
                                numericInput(
                                  "hist_bins",
                                  "Histogram bins:",
                                  value = 20
                                )
                            ),
                            div(style = "flex: 1;",
                                numericInput(
                                  "trunc_hist_upper",
                                  "Truncate upper:",
                                  value = NULL
                                )
                            ),
                            div(style = "flex: 1;",
                                numericInput(
                                  "trunc_hist_lower",
                                  "Truncate lower:",
                                  value = NULL
                                )
                            ),
                            div(style = "flex: 1;",
                                radioButtons(
                                  "hist_scale",
                                  "Scale:",
                                  choices = c("Linear" = "linear", "Log10" = "log10", "log e" = "log", "sqrt" = "sqrt"),
                                  selected = "linear",
                                  inline = TRUE
                                )
                            ),
                            plotOutput("hist_plot", height = "300px")
                          )
                          )
                       )
              ),
              fluidRow(
                column(12,
                       # time line
                       box(
                         title = "Density time line of animals tracked",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,
                         div(
                           style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                           div(style = "flex: 1;",
                              textInput("date_break",
                                        "Select time bin for x axis tick marks:",
                                         value = "1 month",
                                         width = "100%")
                           ),
                           div(style = "flex: 1;",
                               textInput("date_labels",
                                         "Select date labels:",
                                         value = "%b %y",
                                         width = "100%")
                           )
                         ),
                         plotOutput("timeline_plot", height = "300px")
                       )
                )
              )
      )
    )
  )
)
