.onLoad <- function(libname, pkgname) {
  reset_options_mR() # Run this so that get_options is available always on package load

  ### load sp option
  options("sp_evolution_status"=2)

  #packageStartupMessage(cat(paste0("\033[0;", 36, "m","-------------------","\n",
  #                      "Welome to MoveRakeR! Be sure to browseVignettes()","\n",
  #                      "and check out ?MoveRakeR for a full list of functions. Enjoy!", "\n",
  #                      "-------------------", "\033[0m", "\n")))
}


.onAttach <- function(libname, pkgname) {

  ### load sp option
  options("sp_evolution_status"=2)

  reset_options_mR() # Run this so that get_options is available always on package attach
}

