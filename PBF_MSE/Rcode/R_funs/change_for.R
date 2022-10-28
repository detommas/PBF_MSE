#' Change a forecast file to reflect the new end year
#'
#' This function changes an SS forecast file to
#' 1) changes benchmark years
#' 2) changes the first years for caps and allocations
#' 3) changes rebuilder option years
#' 
#' It then writes a new forecast file into the working directory
#'
#' @param ss_file_in filename of original forecast file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new forecast file with full path or relative to working directory
#' @param t_asmt how often assessments are run in years (e.g if every three years, 3)
#' @return A modified forecast file.
#' @author Desiree Tommasi

change_for <- function(ss_file_in, ss_file_out, t_asmt){
  
  #read in forecast file from previous assessment period
  om_for = SS_readforecast(ss_file_in)
  
  #create a new forecast file based on the old to be modified
  om_for_new = om_for
  
  #change end years of the benchmark years - only for recruitment
  om_for_new$Bmark_years[c(8,10)] = c((om_for$Bmark_years[8]+t_asmt),
                                          (om_for$Bmark_years[10]+t_asmt))
  
  #change first year for caps and allocations
  om_for_new$FirstYear_for_caps_and_allocations = om_for$FirstYear_for_caps_and_allocations+t_asmt
  
  #change Rebuilder first chatch
  om_for_new$Ydecl=om_for$Ydecl + t_asmt
  
  #change Rebuilder of current age structure
  om_for_new$Yinit=om_for$Yinit + t_asmt
  
  #Write a stock synthesis data file from the mse_dat list object
  SS_writeforecast(om_for_new, ss_file_out)
  
}
  