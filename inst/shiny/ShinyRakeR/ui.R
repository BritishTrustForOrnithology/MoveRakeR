
ui <- dashboardPage(

  dashboardHeader(title = "ShinyRakeR",
                  rightUi = uiOutput("dup_status_icon")
  ),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Rake Overview", tabName = "rake_overview", icon = icon("leaf")),
      menuItem("Threshold Explorer", tabName = "threshold_explorer", icon = icon("sliders-h")),
      menuItem("Temporal Inspector", tabName = "temporal_inspect", icon = icon("clock")),
      menuItem("Read Me", tabName = "readme", icon = icon("book")),
      checkboxInput("link_maps", "Link maps to each other", value = FALSE),
      hr(),
      div(
        style = "padding-left: 15px;",
        uiOutput("map_status")
      ),
      #hr(),
      div(
        style = "padding-left: 15px;",
        uiOutput("map_status2")
      )
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

                          # ---------------------------------------------------------------------------------- #
                          # Step 1: Optional Gap Assignment
                          wellPanel(
                            tags$h4("Step 1: Identify gaps", style = "color: steelblue;"),
                            #checkboxInput("do_gap_section", "Run gap_section() first?", value = FALSE),
                            numericInput("gap_thresh", "Gap threshold (seconds):", value = 3600, min = 1, step = 60),
                            numericInput("gap_tol", "Tolerance (seconds):", value = 0.2, min = 0, step = 0.01),
                            actionButton("run_gap_section", "Run gap_section()"),
                            uiOutput("gap_done_ui"),
                            tags$p("Set gap parameters before applying annotation filters.", style = "font-size: 12px; color: gray;")
                          ),

                          # ---------------------------------------------------------------------------------- #
                          # Step 2: Duplication of data
                          wellPanel(
                            # 0. dup data
                            tags$h4("Step 2: Assess duplicate data", style = "color: slateblue;"),
                            div(style = "flex: 1;",
                                htmltools::h5(tags$b("Specify annotation and optional filtering of data for duplicates:"))
                            ),
                            div(
                              style = "display: flex; gap: 10px; width: 100%;",
                              actionButton(
                                "run_dup",
                                "Run duplicate_track()",
                                style = "flex: 1; white-space: normal; text-align: center;"
                              ),
                              actionButton(
                                "show_dup_summary",
                                "Summary",
                                style = "flex: 1; white-space: normal; text-align: center;"
                              ),
                              actionButton(
                                "resolve_dup",
                                "Resolve duplicates",
                                style = "flex: 1; white-space: normal; text-align: center;"
                              )
                            )
                          ),

                          # ---------------------------------------------------------------------------------- #
                          # Step 3: Annotation
                          wellPanel(

                            div(
                              style = "display: flex; justify-content: space-between; align-items: center;",

                              tags$h4("Step 3: Annotate data", style = "color: darkgreen;"),

                              actionButton("toggle_sections", "Expand / Collapse all"),
                            ),

                            # this enables the down arrow in the drop downs of individual sections to be displayed
                            tags$style(HTML("
                              summary {
                                display: list-item;
                                cursor: pointer;
                              }
                            ")),

                            # ------------------------------------------------------------------ #
                            # 1. Existing column flags built server side
                            uiOutput("custom_flag_ui"),

                            # ------------------------------------------------------------------ #
                            # 2. Too few data per animal
                            uiOutput("minfix_section"),

                            # ------------------------------------------------------------------ #
                            # 3. NA Lat long
                            uiOutput("NA_section"),

                            # ------------------------------------------------------------------ #
                            # 4. Nsats
                            uiOutput("nsats_section"),

                            # ------------------------------------------------------------------ #
                            # 5. PDOP
                            uiOutput("pdop_section"),

                            # ------------------------------------------------------------------ #
                            # 6. HDOP
                            uiOutput("hdop_section"),

                            # ------------------------------------------------------------------ #
                            # 7. Custom filter
                            uiOutput("custom_section"),

                            # ------------------------------------------------------------------ #
                            # 8. Single gapsections
                            uiOutput("gapsec_section"),

                            # ------------------------------------------------------------------ #
                            # 9. Iterative trajectory speed
                            uiOutput("speedfilt_section"),

                            # ------------------------------------------------------------------ #
                            # 10. Simple trajectory speed
                            uiOutput("custspeedfilt_section"),

                            # ------------------------------------------------------------------ #
                            # 11. Turning angle speed
                            uiOutput("turnangle_section"),

                            # ------------------------------------------------------------------ #
                            # 12. lat long extent selector
                            uiOutput("maxext_section"),

                            # ------------------------------------------------------------------ #
                            # 13. temporal extent
                            uiOutput("maxtemp_section")

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
                                checkboxInput("show_legend", "Show Legend", value = TRUE),

                                div(style = "flex: 0 0 auto;",
                                    actionButton("map_reset", "Map reset", style = "width: 120px;")
                                )
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
                          ),
                          shinyWidgets::dropdown(
                            inputId = "decisions",
                            label = "Current decisions",
                            icon = icon("clipboard-list"),
                            status = "info",
                            width = "250px",
                            up = FALSE,
                            tagList(
                              actionButton("show_decisions", "Current Decisions"),
                              actionButton("clear", "Reset")
                            )
                          ),
                          actionButton(
                            "activate_map",
                            "Activate / deactivate map",
                            class = "btn",
                            style = "
                              background-color: #6c757d;
                              color: white;
                              border-color: #6c757d;
                              "
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
                               ),
                               actionButton(
                                 "activate_map2",
                                 "Activate / deactivate map",
                                 class = "btn",
                                 style = "
                              background-color: #6c757d;
                              color: white;
                              border-color: #6c757d;
                              "
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
      ,
      tabItem(
        tabName = "readme",
        fluidRow(
          column(
            width = 12,
            box(
              title = "ShinyRakeR Documentation",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              includeMarkdown(
                system.file("shiny/ShinyRakeR/readme.md", package = "MoveRakeR")
              )
            )
          )
        )
      )
    )
  )
)
