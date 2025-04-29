README
================
Chris Thaxter
25/04/2025

<!-- README.md is generated from README.Rmd. Please edit that file, but only with package maintainer's permission -->

# MoveRakeR

### Current version: 1.0.3

[![DOI](https://zenodo.org/badge/948019784.svg)](https://doi.org/10.5281/zenodo.15275175)

This is an initial release of `MoveRakeR` and is subject to some ongoing
updates.

## Overview

The R package `MoveRakeR` contains a collection of simple functions to
help streamline data processing and common data wrangling tasks that
often occurs with GPS tracking data. `MoveRakeR` operates at the initial
stages after data acquisition. Although a number of R packages are now
available for analysis of animal movement data (Joo et al. 2020),
`MoveRakeR` tackles some fundamental data processing steps that are
often encountered to help streamline data preparation ahead of further
analytical steps. The `MoveRakeR` package is intended to support the
other existing workflows in other R packages, such as `move`
(Kranstauber et al. 2018) and `move2` (Kranstauber et al. 2024) and
provides some further tools to address common manipulations of data
routinely encountered that often require a degree of subjectivity in
decision making. Should the user be interested in exploring location
error in GPS devices in more detail, we would recommend the approach of
Flemming et al. (2020) and the `ctmm` R package (Fleming & Calabrese
2023).

## Dependencies

`MoveRakeR` needs the following packages installed:

adehabitatLT, dplyr, tibble, lubridate, tidyr, purrr, data.table, move,
RODBC, suncalc, geosphere, DT, leaflet, shiny, shinyWidgets, suncalc,
htmltools, sf, sfheaders, sp, ggplot2, R.rsp, readr, and units.

## Installation

Please install `MoveRakeR` by using:

``` r
devtools::install_github('BritishTrustForOrnithology/BTOTrackingTools', build_vignettes = TRUE)
```

## Vignettes

At present, there are no vignettes in this R package but some will be
added in future releases.

## Usage

By way of a brief history, `MoveRakeR` was borne out of seabird tracking
work. Although not entirely, much of the focus of `MoveRakeR` is on
movements of tracked animals from a central place, used within numerous
papers to date (e.g. Thaxter et al. 2015, Langley et al. 2021, Clewley
et al. 2023, O’Hanlon et al. in prep).

The `MoveRakeR` pipeline requires a dataset with the following columns:
*TagID*, *DateTime*, *latitude* and *longitude*. The `MoveRakeR` process
is built around a `Track` class object. This is not a strict requirement
but aids in visualisation of printing, plotting and summarising.

``` r
data <- read.csv(data.csv,sep = ",", header=TRUE)
data <- Track(data)

summary(data)
plot(data) # slower for large datasets
tabulate_history(data)
```

The `MoveRakeR` package includes a number of data sourcing functions
that allow data to be read into R from databases, for which the two
currently implemented are MoveBank through the `read_track_MB` function,
and the University of Amsterdam Bird-tracking System database (UvA-BiTS)
(Bouten et al. 2013), using the `read_track_UvA` function. For the
UvA-BiTS connection, you will need to first follow the steps outlined
under `?read_track_UvA` to set up an ODBC connection using R package
`RODBC`. The MoveBank login details are also required for use and create
this calling:

``` r
library(RODBC)
db <- odbcConnect("GPS") # if you called your connection "GPS"

library(move)
login <- MoveBankLogin("username","password")
```

Then data can be read in using `read_track_UvA` and `read_track_MB` that
allows concatenation of animal IDs and start/end times of sections of
data required:

``` r
# UvA-BiTS
TagID <- c('1', '2', '3') # example birds
start <- c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23") # example start times 
end <- c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45") # example end times 
dataUvA <- read_track_UvA(TagID, start=start, end=end, pressure = FALSE) # reading in data

# MoveBank
TagID <- c('4', '5', '6') 
start <- c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23")
end <- c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45")
repo = "your_MoveBank_repo"
dataMB <- read_track_MB(TagID, start=start, end=end, repo = repo)
```

The MoveBank read in depends on the `move` package, which of course can
be done directly using that package; the `read_track_MB` function seeks
to align different datasets to a common form for further combination.
The raw MoveBank form of the data can be preserved if need be. Note
also, that the read in for MoveBank data automatically filters out
duplicate data - see also `move2` (Kranstauber et al. 2024) for more
details of such filtering.

## Initial visualisation

Although this is of course up to the user, `MoveRakeR` provides leaflet
mapping options as two R `shiny` apps. This will be updated in the near
future to the `Bootstrap` layout with further functionality. In this
initial release, the first of these functions is called: `plot_leaflet`
and can simply allow visualisation of tracked animals, via:

``` r
plot_leaflet(data)
```

in `plot_leaflet` there are further options for plotting shapes,
additional point shape layers (using the `sf` package), and options to
*plotby* a different variable, e.g. a year, cohort, behaviour etc that
you may have in your data; further coloration and point/line options are
available. This function is a work in progress and replicated perhaps
other in further packages such as in the `bayesmove` package (Cullen
2020), and `ExMove` (Langley et al. 2024) but helps to have such a tool
built in.

A further extension to this app is provided separately in `MoveRakeR`
through the `plot_leaflet_trips` function. This function visualises
individual trips of animals in more detail with further tabs for data
summaries.

A generic S3 `plot` function for is also available for Track objects,
which uses base R graphics and can be overlain on top static maps and
shapefiles, with arrows indicating direction also included. For example:

``` r
ukmap <- sf::st_transform(ukmap, 4326) #n example base map of hour choice in geographic WGS84

# using the generic plot function for the S3 method for class 'Track' (which LBBGWalB201416 is)
Cpal <- grDevices::colorRampPalette(c("red", "green", "blue"))
par(mar=c(0,0,0,0))
plot(ukmap$geometry, xlim=c(-4.7,-2), ylim = c(53.2,54.5), col = "wheat", border= "wheat")
plot(data, anims = "uniq", 
     Lines = TRUE, 
     gap = FALSE,
     ADD=TRUE, 
     Legend = TRUE, 
     cex_p = 0.2, 
     col = Cpal, 
     p4s = 4326)
```

Should the user be interested in voltages, then these can also be
plotted through an S3 generic plot function, that plots objects of class
`Vo`. But, currently this is only available for University of Amsterdam
data at the moment, which is accessed through the `read_voltage_UvA`
function. This may be useful to see how tags are performing,
e.g. battery and solar charging after deployment to check sustainability
of sampling protocols, and plotted alongside GPS data. Further options
also exist to source acceleration and pressure data from UvA-BiTS
directly into R.

## Cleaning and identifying spurious data

The `MoveRakeR` package provides some functionality to examine the GPS
data to try and deduce likely reasoning behind erroneous points.
However, the functions supplied in `MoveRakeR` are mostly aimed at
filtering rather than modelling underlying error processes. It may
therefore be worth first investigating outliers in the data using the
`ctmm` package (Fleming & Calabrese 2023) through `ctmm::outlie()` that
uses available information on location error such as DOP values if
present, or if not number of satellites as well as trajectory speed
estimates (Flemming et al. 2020) in potential error generation.

``` r
# first converting back to a move object (only if if already using MoveRakeR, see move::move() function)
indata <- Track2move(indata)

library(ctmm)

indata_ctmm <- ctmm::as.telemetry(indata)
OUT <- ctmm::outlie(indata_ctmm)

plot(OUT,units=FALSE)
```

Alternatively, the user may deem fixes directly unsuitable by virtue of
a deemed poor quality measurement in these underlying measurement
parameters, for example through too few satellites or DOP values. These
can be examined and then filtered out using `MoveRakeR`. The function
`clean_GPS` can be used to remove data rows in the Track dataset that
are determined to be incorrect by the user. This process is a rather
blunt instrument in that currently this is done at the user’s
discretion. A further option exists to filter data based on the quality
of fix column (“flt_switch”) that may be available for your data from
MoveBank.

The function `clean_GPS` can also be used to filter by a trajectory
speed to remove clear jumps in the GPS that may be obvious from visual
inspection of the data. The `clean_GPS` function uses a trajectory speed
filter to sequentially removes fixes that are beyond a pre-determined
travel speed (‘Thres’) for the animal. However, within `MoveRakeR`, the
function `tspeed_jit` acknowledges that trajectory speed as used in the
`clean_GPS` function is dependent on sampling rate of tags, and small
changes in positional xy error will have a proportionally greater
influence on whether fixes collected at very fine scales, e.g. 60s or
less may be deemed incorrect by `clean_GPS`. The `tspeed_jit` function
helps understand this pattern through introducing bootstrapped error
jitter in the fixes allowing assessment of the error curve.

Finally, `clean_GPS` can label sections of GPS recording where the tag
was recording at the rate as programmed; as GPS data collection is
subject to battery constraints, ‘gaps’ may appear in the data, for which
it is often useful to label to avoid extrapolation over time periods
where there is greater uncertainty in position. Gaps can also be
labelled separately in the `gap_section` function. The user may know the
rate that tags are programmed to already, but if not, the function
`assign_rates` can categorise the GPS data using the time elapsed
between consecutive fixes.

``` r
clean_GPS(data = data, dropsats = 3, Thres = 30, GAP = 28800)

gap_section(data, GAP = 28800*2)
```

Among other packages, the `ExMove` toolkit (Langley et al. 2024)
provides a Shiny App to investigate the effect of filter effects of
speed and net displacement based on prior knowledge of the study system.
Within `MoveRakeR` a Shiny application *is planned for future releases*
to investigate distributions of parameters and leverage plots of
available location error data, and highlight potential gaps in the
DateTime GPS data record (see below).

## Further data manipulation

A common task is to sub-sample GPS data to a common rate; this may be
needed for further analytical steps or to align data in some way. As
with all these tasks, modelling solutions may exist to not require this
step. However,`MoveRakeR` provides a sub-sampling algorithm `sub_samp`
that builds upon existing tools available in some other packages such as
`AMT` (Signer et al. 2019) and `move` (Kranstauber et al. 2018), in
particular including a bootstrap method to sub-sample data and maximise
data availability, useful for animals traversing space for only a
limited time such as on migration. Other approaches using interpolation
and simulation are not provided in `MoveRakeR` for which other R
packages are available to provide those operations, such as
`adehabitatLT`, `move` and continuous-time movement modelling approaches
such as `crawl` (Johnson et al. 2008) `ctmm` (Fleming & Calabrese 2023)
and `aniMotum` (Jonsen et al. 2023).

Further annotation of the data is often required for central place
foraging consideration. Metrics for an animal or population may be
needed to assess how far typically animals move from their central
place, how long trips are, and other metrics such as tortuosity,
bearing, total distance and probably many others. The trips are first
identified provided using function `define_trips` using user-specified
definitions of the central place such as a radius around a centre point,
a rectangle or polygon to define the central place, or alternatively use
of specific nest locations for individual animals; on the latter, nest
locations may be known directly or perhaps where not, estimates from
other approaches such as `nestR` (Picardi et al. 2020) or `recurse`
(Bracis et al. 2019) may be informative; see also the `trip` package
(Sumner et al. 2009, Sumner 2011). These locations are needed by the
function to estimate where to measure the trip start/end point in space
and time. The function `trip_stats` can then be used to generate
movement metric summaries of trips for each animal and the group of
animals. Further tabulation functions are provided such as
`tabulate_trips` which takes the output from `trip_stats` and summarise
the information neatly. The `trip_stats` function will return a new
object of class `Trip`. The default of the `trip_stats` function is to
use the first and last fix of the animal within the central place
boundary to compute trip duration and distances.

``` r
newdata <- data %>% 
  clean_GPS(GAP = 3600*8, speedfilt = TRUE, drop = FALSE) %>% 
  subs_samp(dt=300, tol = 0.2, method = "sequencer") %>% # sub-sample data
  define_trips(method = "rect", lls = LLS, plot = TRUE, verbose = TRUE) # (e.g. LLS = c(-3.2, 54.0553,-3.1689, 54.0437))
  
plot_leaflet(newdata)

trips <- trip_stats(newdata) # a new dataset of individual trips using first and last known positions in CP
tabulate_trips(trips) # neater tabulation

# visualising individual trips
plot_leaflet_trips(newdata, trips = trips)
```

Note that complexities can arise for trips that are determined as
arriving and then departing again from the central place with a single
shared GPS fix for starts and ends of trips. These are handled in
`MoveRakeR` through duplicate row records, stored as an attribute from
the `define_trips` function. However should substantial data
manipulation, such as base R processing, be carried out between these
two functions being run, it is best to add these duplicate data directly
into the data before such manipulation and ahead of `trip_stats` being
run; beware that this data will likely need removing again ahead of
downstream analyses.

``` r

LLS <- c(-3.2, 54.0553)
newdata <- define_trips(data = data, method="circ", lls = LLS) %>%
   add_cag_trips() # this will add duplicate rows directly to the Track using  column "extra_row" 1 or 0.

### other potential data manipulation e.g.
newdata <- data.frame(newdata) # removes any attributes and Track data class

# trip_stats() can still be run given pseudo duplicate data directly included demarcate start/ends for all trips
trips <- trip_stats(newdata)

# but then best put the extra rows back as an attribute, removing them from the main Track data
newdata <- Track(newdata) %>% reset_cag_trips()
```

## Track-style layout of datasets

The `MoveRakeR` seeks to employ ‘tidy’ principles at heart, although
further `data.table` and base R operations are also well-utilised. The
use of `tibble` and `dplyr` ensured that operations were carried out at
very least at the grouping of the animal level. However, in case further
listed formats of data are wanted, the user can convert the `Track` data
to a list of animals `TrackStack` or a further multi-list
`TrackMultiStack` e.g. for separate stacks of years. These are just
convenience functions to convert to base R operations.

## A word on the vertical dimension

The `MoveRakeR` package is mainly focused on xy position, but further
functionality may be included for vertical assessment. For example
assessing fix-to-fix changes in GPS altitude and whether any outlying
points are worthy of considering as erroneous. Such consideration may be
useful for considering the flight altitude of birds, but this is another
topic that deserves separate consideration, and often includes modelling
of such sources of error (Ross-Smith et al. 2016, Johnston et al. 2023).

## Other common tasks, offshore, distance to coast

These are all routine GIS operations, that `MoveRakeR` doesn’t seek to
replicate. However, should you wish to add a distinction for offshore
and onshore (likely very specific circumstances) then a function
`offshore` is available to annotate as a simple binary vector. Should
you be interested in distance to coast as a metric for foraging trips,
then `dist2coast` is also provided, then can be fed through the
`trip_stats` routine. These functions depend on you definitions of the
coastline for which an `sf` shape is provided to each function, with a
buffer extent available to crop the overall shape for increased
efficiency.

``` r

newdata <- data %>% 
  offshore(shape = shape_sf, crop = TRUE, buffer_extent = 10000, p4s = 3035) %>%
  dist2coast(shape = shape_sf, crop = TRUE, buffer_extent = 50000, p4s = 3035)
```

## References

Bouten, W., Baaij, E.W., Shamoun-Baranes, J. & Camphuysen, K.C.J. (2013)
A flexible GPS tracking system for studying bird behaviour at multiple
scales. Journal of Ornithology, 54, 571–580.

Bracis, C., Bildstein, K.L. & Mueller, T. (2018) Revisitation analysis
uncovers spatio-temporal patterns in animal movement data. Ecography,
doi.10.1111/ ecog.03618.

Calenge, C. (2006) The package adehabitat for the R software: tool for
the analysis of space and habitat use by animals. Ecological Modelling,
197, 516-519

Clewley, G.D., Thaxter, C., Scragg, E.S., Masden, E.A., Barber, L.J.,
Conway, G., Clark, N.A. & Burton, N.H.K. (2023) Daily, seasonal and
annual variation in area use of Lesser Black-backed Gulls (*Larus
fuscus*) related to offshore renewable developments. Bird Study, 70,
<doi:10.1080/00063657.2023.2190080>

Cullen, J. (2020) R package ‘bayesmove’ v 0.1.0.
<https://github.com/joshcullen/bayesmove>

Fleming, C.H., Drescher-Lehman, J., Noonan, M.J., Akre, T.S.B., Brown,
D.J., Cochrane, M.M., Dejid, N., DeNicola, V., DePerno, C.S., Dunlop,
J.N., Gould, N.P., Harrison, A.-L., Hollins, J., Ishii, H., Kaneko, Y.,
Kays, R., Killen, S.S., Koeck, B., Lambertucci, S.A., LaPoint, S.D.,
Medici, E.P., Meyburg, B.-U., Miller, T.A., Moen, R.A., Mueller, T.,
Pfeiffer, T., Pike, K.N., Roulin, A., Safi, K., Séchaud, R., Scharf,
A.K., Shephard, J.M., Stabach, J.A., Stein, K., Tonra, C.M., Yamazaki,
K., Fagan, W.F. & Calabrese, J.M. (2020) A comprehensive framework for
handling location error in animal tracking data. bioRxiv
2020.06.12.130195; doi: <https://doi.org/10.1101/2020.06.12.130195>

Fleming, C.H. & Calabrese, J.M. (2023). ctmm: Continuous-Time Movement
Modeling. R package version 1.2.0.
<https://CRAN.R-project.org/package=ctmm>

Langley, L.P., Bearhop, S., Burton, N.H. Banks, A.N., Frayling, T.,
Thaxter, C.B., Clewley, G.D., Scragg, E. & Votier, S.C. (2021) GPS
tracking reveals landfill closures induce higher foraging effort and
habitat switching in gulls. Movement Ecology, 9, 56
<https://doi.org/10.1186/s40462-021-00278-2>

Johnson, D.S., London, J.M., Lea, M.-A. & Durban, J.W. (2008)
Continuous-time correlated random walk model for animal telemetry data.
Ecology, 89, 1208-1215. <doi:10.1890/07-1032.1>.

Jonsen, I.D., Grecian, W.J., Phillips, L., Carroll, G., McMahon, C.,
Harcourt, R.G., Hindell, M.A. & Patterson, T.A. (2023) aniMotum, an R
package for animal movement data: Rapid quality control, behavioural
estimation and simulation. Methods in Ecology & Evolution, 14(3),
806-816.

Joo, R., Boone, M.E., Clay, T.A., Patrick, S.C., Clusella-Trulas, S. &
Basille, M. (2020) Navigating through the R packages for movement. BES,
89, 248-267.

Kranstauber, B., Smolla, M., & Scharf, A. K. (2018). move: Visualizing
and analyzing animal track data. R package version 3.1.0.
<https://CRAN.R-project.org/package=move>

Kranstauber, B., Safi, S. & Scharf, A.K. (2024) move2: R package for
processing movement data. Methods in Ecology and Evolution, 15(9),
1561-1567.

Signer, J. Fieberg, J. & Avgar, T. (2019) Animal movement tools (AMT): R
package for managing tracking data and conducting habitat selection
analysis. Ecology and Evolution, 9, 880-890.

Sumner, M.D. (2011) The Tag Location Problem. Ph.D. thesis, University
of Tasmania. <https://eprints.utas.edu.au/12273/3/sumner.pdf>.

Sumner, M.D., Wotherspoon, S.J. & Hindell, M.A. (2009) Bayesian
estimation of animal movement from archival and satellite tags. PLoS
ONE, 4(10). <http://dx.plos.org/10.1371/journal.pone.0007324>.

Thaxter, C.B., Ross-Smith, V.H., Bouten, W., Clark, N.A., Conway, G.J.,
Rehfisch, M.M. & Burton, N.H.K. (2015) Seabird–wind farm interactions
during the breeding season vary within and between years: A case study
of lesser black-backed gull *Larus fuscus* in the UK. Biological
Conservation, 186 doi.10.1016/j.biocon.2015.03.027.
