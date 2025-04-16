#' MoveRakeR: An R package for loading, manipulating and analysing animal tracking data.
#'
#' The \code{MoveRakeR} package ('Move' = animal movement; 'Rake' = cleaning data) provides several categories of functions:
#' (1) reading in tracking data, carrying out (2) basic cleaning, (3) manipulation,
#' summary, trip-level simple analyses and (4) plotting tracks, pruior to further analytical
#' adventures. The package is not aiming to replace existing workflows in other packages, such as
#' \code{move} and \code{move2} but aim to provide some initial tools to address common manipulations of data routinely
#' encountered that often require a degree of subjectivity in decision making.
#'
#' The \code{MoveRakeR} package aims to be used primarily in the first stage of data manipulation and has been
#' borne out of application of regular processing of tracking data on seabirds.
#' However, the processes contained in this package should be relevant to many other species. The package is also
#' aimed at GPS data collected from bird-borne devices rather than other positional telemetry systems such as Argos or geolocator tags.
#'
#' The thought may occur: "Oh no, here's yet another tracking data manipulation package!". That is true, there are many out there already,
#' and cover some similar ground to \code{MoveRakeR}. However, we would say the
#' \code{MoveRakeR} package aims to wrap up some complex, frequently-encountered issues and pitfalls that can come
#' with a 'starting from scratch' approach using first principles. This is borne out of many years of wrangling
#' such data on different bird species and the devil is in the detail. There will always be things to add to the workflow or bespoke
#' considerations to the data that go beyond this package, and so we also suggest the reader to
#' check out \code{ExMove}, \code{move}, \code{trip}, and \code{AMT} for further analyses and approaches.
#'
#' @section 'Cleaning' the data:
#'
#' When a GPS dataset is obtained, as with any data, requires scrutiny and assessment prior to further analyses
#' being conducted to address the research question at hand. That goes without saying, however specifically for telemetry data
#' and even more specifically for GPS data, there are certain 'cleaning' steps of the data that are typically undertaken. These mainly
#' relate to measurement errors of the GPS data, perhaps not unlike other telemetry platforms. A first step is therefore understanding the
#' degree of error and potential bias present in the data. These biases without treatment/consideration could have an impact on further
#' data processing and analyses further down the line. This of course is highly dependent on the research question or intended use of data
#' as the error in GPS may be of key interest in itself, and thus a fully 'raw' dataset may be what is needed for example to model such sources of error and understand why they occur.
#' Some error can also be modelled/incorporated in other workflows such as continuous-time or behavioural movement modelling (e.g. R package momentuHMM, Michelot et al ****). However, a case in point is a more basic
#' summary of movement of animals, such as migration or central place movement where it may not be the task to model such measurement errors, but
#' instead filter the data to remove or 'clean' the data for less accurate/precise information. For example, a summary of 'foraging trips' of animals
#' or time spent away from a central place may be all that is required, that benefits from removing GPS locations that may be clearly incorrect and their
#' retention would inflate movement metrics. Basic area utilisation methods may also benefit from such treatment.
#'
#' GPS locations (in the xy horizontal dimension) may be subject to positional errors that arise from a number of sources, and thus this subject is highlight complex; for example signal interference,
#' obstruction (e.g. buildings, trees), neighbouring interference signals, configuration and number of satellites in the sky,
#' communication with the satellites, atmospheric conditions, and the 'start' of the tag i.e. cold-start/warm start,
#' potentially linked also to sampling rate of the GPS. The \code{MoveRakeR} does not unpick all of these drivers specifically, but
#' draws attention to these errors that may impact the data collected. Questions therefore arise as to how suitable the
#' GPS data are for feeding into these simpler movement metrics (such as trip duration, distance travelled, foraging range), and where should cut-offs be
#' placed to avoid biased estimates. Often these delineations are made based on expert judgement by the analyst by examining the precision information
#' available from the tags deployed (that can also vary greatly in quality and amount). Fewer satellites used for a fix may result increase the error in
#' location (minimum of three for a valid xy position and four for a vertical position) and therefore a minimum threshold of satellites available for a fix may be useful to consider.
#' If information on satellite configuration position in the sky is available (DOP) then this may be worth examining. GPS can sometimes give highly inaccurate
#' locations that are clearly impossible for the animal to have obtained, removed in a consecutive (forwards and backwards) way so that displacement of such fixes are captured. A simple trick is to use a maximum travel speed for the animal
#' beyond which fixes are deemed impossible and therefore erroneous. These steps require thresholds to be set in the data, for which an initial
#' function \code{\link{clean_GPS}} is provided. The speed filter used is a trajectory speed filter between consecutive fixes in date and time per animal,
#' however, the sensitivity of the filter at finer temporal scales e.g. less than 60s, requires consideration, provided in the \code{\link{tspeed_jit}} function.
#'
#' Another key aspect of tracking data is that data collected may be interrupted, for example due to battery depletion, causing hiatuses in the
#' recording. Through visual assessment of the data 'gaps' between GPS locations in date-time per animal can be identified and data
#' can be labelled to flag up 'gapiness'. Having these strings of fixes identified id useful; for example, it is perhaps unwise to consider movement between two locations of
#' with a large temporal gap as this may have missed a considerable amount of activity of the animal, particularly if distance-based metrics such as total distance
#' travelled are concerned. The function \code{\link{gap_section}} specifies the gap to label in the data; this gap definition is also allowed in \code{clean_GPS}. Additional operations
#' such as consideration of what to do with 'orphaned' fixes resulting from placement as a single gapsection are also provided.
#'
#' Another common task is to filter (or downgrade) the data from a finer temporal sampling rate to a coarser one. This has many applications such as
#' aligning data within and among animals across varying sampling rates (), dealing with statistical issues such as temporal autocorrelation (e.g. Davies et al. 2024), or perhaps simply
#' one fix every x time unit is required for a particular reason. data can of course be interpolated and there are many great R packages now available to
#' consider continuous time movement modelling process such as \code{ctmm} (), \code{crawl} (), \code{foieGras} (), \code{aniMotum} (), or simple linear interpolation over space and time \code{adehabitatLT} (Calenge 2006).
#' In fact \code{adehabitatLT} is used within \code{MoveRakeR} for some simple computations using the \code{adehabitatLT::as.ltraj} function.
#' Where estimation of presence in space via a modelled approach is not desired however, filtering steps are commonly used. other R packages are available
#' for this as well such as: \code{move}, \code{trip}, \code{ExMove}. However, here we build on these to consider some quirks encountered in
#' conducting filtering. In particular, a fundamental consideration is how to perform the filtering, which could be rounding the DateTimes to a regular
#' time unit, such as every minute, hour etc, or whether this should be conducted from the start time of the animal(s) rather than by the clock as such. More
#' importantly however, the sampling process can be considered quite reductionist and selective for specific fixes at the same rounding units. We therefore also
#' provide a sub-sampling approach to randomise this process; this is particularly useful to make maximal use of the data and important when an animal is traversing a
#' unit of space only once, such as on migration (Schwemmer et al. 2021). The function \code{\link{sub_samp}} provides these additional capabilities.
#'
#' @section Central place foraging 'trips':
#'
#' Beyond the basic cleaning and filtering stages above, further annotation of the data is often required for central place foraging consideration. As mentioned above,
#' metrics for an animal or population may be needed to assess how far typically animals move from their central place, how long trips are, and other metrics
#' such as tortuosity, bearing, total distance and probably many others. To do this, trips ideally need annotation in the data directly, which builds on
#' internal work streams conducted at the British trust for Ornithology, and uses a tidy approach as recommended by the \code{ExMove} toolkit. These processes
#' are provided as a function \code{\link{define_trips}} to remove repetition of code and permitting definitions of a central place such as a radius, rectangle or polygon around a central colony point,
#' or use of specific nest locations for individual animals; on the latter, nest locations may be known directly or perhaps where not, estimates from other
#' approaches such as nestR (Picardi et al. 2020) or recurse (Bracis et al. 2019) may be informative, or alternatively a single colony location may be provided. These locations are needed by the function
#' to estimate where to measure the trip start/end point in space and time. The function \code{\link{trip_stats}} can then be used to generate movement metric summaries
#' of trips for each animal and the group of animals. Further tabulation functions are provided such as \code{tabulate_trips} which takes
#' the output from \code{trip_stats} and summarise the information neatly, such as for reporting purposes.
#'
#' The sampling 'rate' of GPS tags may be known of course by the user, but if not, these can be estimated by simple binning of the data. This is a typically
#' trivial task, but a separate function \code{\link{assign_rates}} is provided to assist.
#'
#' In should also be noted that vertical information is not the focus of \code{MoveRakeR} as this data cleaning as flight height of birds is considered a separate topic, but there is a degree of
#' overlap in the way data may be assessed at the outset.
#'
#' @section Reading in data:
#'
#' We provide a specific set of functions to source data from the University of Amsterdam Bird-tracking system (UvA-BiTS) - see \url{https://www.uva-bits.nl/};
#' the primary function is \code{\link{read_track_UvA}} which links directly with the postGreSQL online database. This requires initial set up of the R ODBC database connection (see function help for information).
#' Further queries of that database are also made via functions to read in accelerometry data \code{\link{read_accn_UvA}} and
#' data from pressure sensors \code{\link{read_pressure_UvA}} and voltage information \code{\link{read_voltage_UvA}}; a specific function is also required for
#' plotting the voltage \code{plot.Vo}. It is often also helpful to query the UvA-BiTS database for TagIDs for the name of the repository and we
#' therefore provide. A function not currently available in \code{MoveRakeR} is for merging of accelerometer
#' classifications with \code{\link{Track}} data; this is not provided as it is considered up to the user how to do this but get in touch if this may be useful.
#'
#' It was not the task of \code{MoveRakeR} to fully replace any other work streams that source data from online repositories such as MoveBank. However,
#' we provide a wrapper function \code{\link{read_track_MB}} to access MoveBank repositories using the \code{move} package; this was to bring data
#' in line with the required layout for \code{MoveRakeR} and facilitated easy combining of MoveBank and UvA-BiTS datasets.
#' This may be quite a niche issue, in which case direct use of the \code{move} or \code{move2} packages are recommended.
#'
#' For \code{MoveRakeR} data are required with a minimum of four columns: TagID, DateTime, longitude and latitude.
#' These can be coerced to an in-house \code{Track} object
#' for further generic plotting and summarising of the Track object class. The main format is a single data.frame object. listed
#' objects are sometimes used, which are then named \code{TrackStack} and \code{TrackMultiStack} class objects. However, it is
#' simpler to work with \code{data.frame} or \code{tibble} objects of a single dataset. These data objects can be of multiple animals. MoveBank
#' data can also be coerced to a \code{Track} format using \code{move2Track} or \code{Track2move} for the inverse operation.
#'
#' On a specific note, the \code{MoveRakeR} package makes substantial use of the \code{tidyverse}, specifically using R packages
#' \code{dplyr}, \code{tibble} and \code{tidyr}. Given the aim of this package was to provide some useful tools,
#' we have sought to make the code contained within functions accessible if need be, to avoid code becoming a 'black-box' and
#' permit 'tidy' workflows (Langley et al. 2024). The {tidyverse} packages are used here to group data such as by TagID when carrying out specific tasks
#' requiring a grouping, although \code{MoveRakeR} does make substantial use of base R and data.table as well.
#'
#' @section Visualising data:
#'
#' This is totally up to the user of course, but some in-built plotting aids have been coded into \code{MoveRakeR} that use \code{leaflet}
#' as a \code{shiny} application for a more interactive experience, specifically one for overall visualisation called \code{\link{plot_leaflet}}
#' and one for once foraging trips have been defined on the data call \code{\link{plot_leaflet_trips}}, the latter which can help visualise
#' specific trips, distances travelled over time and in relation to the overall distribution and so forth. Further a generic \code{plot.Track}
#' S3 method is available if the data is coerced to a \code{Track} format. Additionally, \code{plot.Vo} is a further plot generic for voltage objects for
#' UvA-BiTS data.
#'
#' @section Other:
#'
#' An S3 generic print method is coded into \code{MoveRakeR} for giving a 'pretty' output in the R console, and a \code{summary} function
#' is provided that gives a very simple overview of the data such as number of fixes per animal, start and end periods of
#' data, for which there is further control over \code{set_options_mR}.
#'
#' The overall class of object for \code{MoveRakeR} is a \code{Track} object but functions are not strict in requiring this format, only the
#' named data columns as mentioned earlier. However functions for making a \code{Track} are present, as well as further listed formats of \code{TrackStack}
#' and \code{TrackMultiStack} (double listed).
#'
#' Further functions for GIS-based assessments are also available for working out offshore GPS fixes compared to terrestrial ones using the \code{offshore} function,
#' and determining the distance to coast using the \code{dist2coast} function; these function add additional columns of offshore and dist2coast to the the data
#' having ingested a shapefile to make the determination. These, however, are broad GIS tasks though manageable outside of \code{MoveRakeR} and are not central to this
#' pipeline. This is also perhaps more specific to certain taxa, with this task being borne out
#' of gull-specific work where birds were moving over terrestrial and marine environments, requiring distinction in foraging trips. In the present package
#' release these functions are note dovetailed into the trip process but this is being updated.
#'
"_PACKAGE"
#' @name MoveRakeR
NULL
