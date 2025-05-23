#' MoveRakeR: An R package for loading, manipulating and analysing animal tracking data.
#'
#' The \code{MoveRakeR} package ('Move' = animal movement; 'Rake' = cleaning data) provides several categories of functions:
#' (1) reading in tracking data, carrying out (2) basic cleaning, (3) manipulation,
#' summary, trip-level simple analyses and (4) plotting tracks, prior to further analytical
#' adventures.
#'
#' The \code{MoveRakeR} package is primarily focused at the first stages of data manipulation and cleaning and has been
#' borne out of application of regular processing of tracking data on a variety of bird species. The \code{MoveRakeR} package is intended to support
#' the other existing workflows in other R packages, such as
#' \code{move} (Kranstauber et al. 2018) and \code{move2} (Kranstauber et al. 2024) and provides some further
#' tools to address common manipulations of data routinely
#' encountered that often require a degree of subjectivity in decision making. We also direct the reader to
#' check out \code{ExMove}, (Langley et al. 2024), \code{trip}, (Sumner 2009, Sumner et al. 2011)
#' and \code{AMT} (Signer et al. 2019) for further analyses and data manipulation. Of note, is that the \code{MoveRakeR} package is also
#' currently aimed at GPS data collected from bird-borne devices, rather than other positional telemetry systems such as Argos or geolocator tags.
#'
#' @section Cleaning the data:
#'
#' When a GPS dataset is obtained, as with any data, requires scrutiny and assessment prior to further analyses
#' being conducted to address the research question at hand. That goes without saying, however specifically for telemetry data
#' and even more specifically for GPS data, there are certain 'cleaning' steps of the data that are typically undertaken. These mainly
#' relate to measurement errors of the GPS data, perhaps not unlike other telemetry platforms.
#'
#' Early steps in processing include dealing with duplicate records and constraining data per animal for
#' a certain number of records and time periods. These are not covered here but instead we direct the user to the
#' \code{move2} package liaising directly with MoveBank (Kranstauber et al. 2024). A further data preparation step is understanding the
#' degree of location error and potential bias present in the data. These biases without treatment/consideration could have an impact on further
#' data processing and analyses further down the line. This of course is highly dependent on the research question or intended use of data
#' as the error in GPS may be of key interest in itself, and thus a fully 'raw' dataset may be what is needed for example to model such sources of error and understand why they occur.
#' Some error can also be modelled/incorporated in other workflows such as continuous-time or behavioural movement modelling (e.g. R package \code{momentuHMM}, McClintock & Michelot 2018). However, a case in point is a more basic
#' summary of movement of animals, such as migration or central place movement where it may not be the task to model such measurement errors, but
#' instead filter the data to remove or 'clean' the data for less accurate/precise information. For example, a summary of 'foraging trips' of animals
#' or time spent away from a central place may be all that is required, that benefits from removing GPS locations that may be clearly incorrect and their
#' retention would inflate movement metrics. Basic area utilisation methods may also benefit from such treatment.
#'
#' GPS locations (in the xy horizontal dimension) may be subject to positional errors that arise from a number of sources, and thus this subject is highlight complex; for example signal interference,
#' obstruction (e.g. buildings, trees), interference signals, configuration and number of satellites in the sky,
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
#' The methods used in \code{MoveRakeR} being simple metrics to consider that may carry bias, are by no means all that can be done.
#' We recommend the R package \code{ctmm} to consider location error as well as trajectory speed estimates (Flemming et al. 2020), particularly the
#' function ctmm::outlie() that can suggest outlying locations using distance calculations using longitude & latitude,
#' and speeds over the timesteps, accounting for telemetry error.
#'
#' Another key aspect of tracking data is that data collected may be interrupted, for example due to battery depletion, causing hiatuses in the
#' recording. Through visual assessment of the data 'gaps' between GPS locations in date-time per animal can be identified and data
#' can be labelled to flag up 'gapiness'. Having these strings of fixes identified id useful; for example, it is perhaps unwise to consider movement between two locations of
#' with a large temporal gap as this may have missed a considerable amount of activity of the animal, particularly if distance-based metrics such as total distance
#' travelled are concerned. The function \code{\link{gap_section}} specifies the gap to label in the data; this gap definition is also allowed in \code{clean_GPS}. Additional operations
#' such as consideration of what to do with 'orphaned' fixes resulting from placement as a single gapsection are also provided.
#'
#' Another common task is to filter (or downgrade) the data from a finer temporal sampling rate to a coarser one. This has many applications such as
#' aligning data within and among animals across varying sampling rates, dealing with statistical issues such as temporal autocorrelation (e.g. Davies et al. 2024), or perhaps simply
#' one fix every x time unit is required for a particular reason. data can of course be interpolated and there are many great R packages now available to
#' consider continuous time movement modelling process such as \code{crawl} (Johnson et al. 2008), \code{ctmm} (Fleming & Calabrese 2023) and \code{aniMotum} (Jonsen et al. 2023)
#' or simple linear interpolation over space and time \code{adehabitatLT} (Calenge 2006).
#' The \code{adehabitatLT} package is used within \code{MoveRakeR} for some simple computations using the \code{adehabitatLT::as.ltraj} function.
#' Where estimation of presence in space via a modelled approach is not desired however, filtering steps are commonly used. other R packages are available
#' for this as well such as: \code{move}, (Kranstauber et al. 2018) \code{trip} (Sumner et al. 2009, Sumner 2011), \code{ExMove} (Langley et al. 2024). However, here we build on these to consider some quirks encountered in
#' conducting filtering. A fundamental consideration is how to perform the filtering, which could be rounding the DateTimes to a regular
#' time unit, such as every minute, hour etc, or whether this should be conducted from the start time of the animal(s) rather than by the clock as such. More
#' importantly however, the sampling process can be considered quite reductionist and selective for specific fixes at the same rounding units. We therefore also
#' provide a novel sub-sampling approach to randomise this process; this is particularly useful to make maximal use of the data and important when an animal is traversing a
#' unit of space only once, such as on migration (Schwemmer et al. 2021). The function \code{\link{sub_samp}} provides these additional capabilities.
#'
#' @section Central place foraging trips:
#'
#' Beyond the basic cleaning and filtering stages above, further annotation of the data is often required for central place foraging consideration. As mentioned above,
#' metrics for an animal or population may be needed to assess how far typically animals move from their central place, how long trips are, and other metrics
#' such as tortuosity, bearing, total distance and probably many others. To do this, trips ideally need annotation in the data directly and \code{MoveRakeR}
#' uses a tidy approach as recommended by the \code{ExMove} toolkit. These processes
#' are provided as a function \code{\link{define_trips}} to remove repetition of code and permitting definitions of a central place such as a radius, rectangle or polygon around a central colony point,
#' or use of specific nest locations for individual animals; on the latter, nest locations may be known directly or perhaps where not, estimates from other
#' approaches such as nestR (Picardi et al. 2020) or recurse (Bracis et al. 2019) may be informative, or alternatively a single colony location may be provided. These locations are needed by the function
#' to estimate where to measure the trip start/end point in space and time. The function \code{\link{trip_stats}} can then be used to generate movement metric summaries
#' of trips for each animal and the group of animals. Further tabulation functions are provided such as \code{tabulate_trips} which takes
#' the output from \code{trip_stats} and summarise the information neatly, such as for reporting purposes. Please note
#' that complexities can arise when manipulating data between the use of \code{define_trips} and \code{trip_stats} functions that could result
#' in a loss of a required attribute relating to trips that share a common start and end DateTime, which can be handled through adding
#' needed data rows using \code{add_cag_trips} back into the data.
#'
#' The sampling 'rate' of GPS tags may be known of course by the user, but if not, these can be estimated by simple binning of the data. This is a typically
#' trivial task, but a separate function \code{\link{assign_rates}} is provided to assist.
#'
#' In should also be noted that vertical information is not the focus of \code{MoveRakeR} as this data cleaning as flight height of birds is considered a separate topic, but there is a degree of
#' overlap in the way data may be assessed at the outset.
#'
#' @section Reading in data:
#'
#' We provide a specific set of functions to source data from the University of Amsterdam Bird-tracking system (UvA-BiTS, Bouten et al. 2013) - see \url{https://www.uva-bits.nl/};
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
#' @section References:
#'
#' Bouten, W., Baaij, E.W., Shamoun-Baranes, J. & Camphuysen, K.C.J. (2013) A flexible GPS tracking system for studying bird behaviour at
#' multiple scales. Journal of Ornithology, 54, 571–580.
#'
#' Bracis, C., Bildstein, K.L. & Mueller, T. (2018) Revisitation analysis uncovers spatio-temporal patterns in
#' animal movement data. Ecography, doi: 10.1111/ ecog.03618.
#'
#' Calenge, C. (2006) The package adehabitat for the R software: tool for the analysis of space and habitat use by animals. Ecological Modelling,
#' 197, 516-519
#'
#' Davies, J.G., Boersch-Supan, P.H., Clewley, G.D., Humphreys, E.M., O’Hanlon, N.J., Shamoun-Baranes, J., Thaxter, C.B., Weston, E. & Cook, A.S.C.P. (2024)
#' Influence of wind on kittiwake Rissa tridactyla flight and offshore wind turbine collision risk. Marine Biology, 171, DOI: 10.1007/s00227-024-04508-0
#'
#' Fleming, C.H. & Calabrese, J.M. (2023). ctmm: Continuous-Time Movement Modeling. R package version
#' 1.2.0. https://CRAN.R-project.org/package=ctmm
#'
#' Kranstauber, B., Smolla, M., & Scharf, A. K. (2018). move: Visualizing and analyzing animal track data. R package version 3.1.0. https://CRAN.R-project.org/package=move
#'
#' Kranstauber, B., Safi, S. & Scharf, A.K. (2024) move2: R package for processing movement data. Methods in Ecology  and Evolution, 15(9), 1561-1567.
#'
#' Langley, L.P., Lang, S.D.J., Ozsanlav-Harris, L. & Trevail, A.M. (2024) ExMove: An open-source toolkit for processing and exploring animal-tracking data in R.
#' Journal of Animal Ecology, 93(7), 784-795.
#'
#' McClintock, B.T. & Michelot, T. (2018) momentuHMM: R package for generalized hidden Markov models of animal movement. Methods in
#' Ecology and Evolution 9(6): 1518-1530.
#'
#' Picardi, S., Smith, B.J., Boone, M.E., Frederick, P.C., Cecere, J.G., Rubolini, D., Serra, L., Pirrello, S., Borkhataria, R.R. & Basille, M. (2020)
#' Analysis of movement recursions to detect reproductive events and estimate their fate in central place foragers.
#' Movement Ecology, 8, 24 https://doi.org/10.1186/s40462-020-00201-1
#'
#' Schwemmer, P., Pederson, P., Haecker, K., Bocher, P., Fort, J., Mercker, M., Jiguet, F., Elts, J., Marja, R., Piha, M., Rousseau, P. & Garthe S, (2023)
#' Assessing potential conflicts between offshore wind farms and migration patterns of a threatened shorebird species. Animal Conservation, 26(3), 303-316.
#'
#' Signer, J. Fieberg, J. & Avgar, T. (2019) Animal movement tools (AMT): R package for managing tracking data and conducting habitat selection
#' analysis. Ecology and Evolution, 9, 880-890.
#'
#' Sumner, M.D. (2011). The Tag Location Problem. Ph.D. thesis, University of Tasmania. <https://eprints.utas.edu.au/12273/3/sumner.pdf>.
#'
#' Sumner, M.D., Wotherspoon, S.J. & Hindell, M.A. (2009). Bayesian estimation of animal movement from archival and satellite tags.
#' PLoS ONE, 4(10). <http://dx.plos.org/10.1371/journal.pone.0007324>.
#'
#'
"_PACKAGE"
#' @name MoveRakeR
NULL
