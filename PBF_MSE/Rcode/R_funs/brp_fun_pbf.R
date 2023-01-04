#' Computes a biomass reference point for North Pacific albacore tuna
#' given an SS model output file 
#'
#' @param ssoutput An SS Report.sso file already read into R using the function SS_ouput in the package r4ss
#' @param fraction specifies the fraction of maximum spawning biomass specified by the reference point
#' @return a biomass reference point
#' @author Desiree Tommasi

brp_fun_pbf <- function(ssoutput, fraction){
  
  #extract derived quantities
  Derived_quant = ssoutput$derived_quants
  
  #extract unfished SSB
  SSB0 = Derived_quant$Value[Derived_quant$Label=="SSB_unfished"]
  
  #Calculation of reference point given the percent of unfished SSB
  BRP = fraction*SSB0
  
  return(BRP)
}