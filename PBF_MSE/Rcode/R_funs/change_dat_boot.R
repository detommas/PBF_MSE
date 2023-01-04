#' Add to a Stock Synthesis data file new catch data
#'
#' This function changes an SS dat file to
#' 1) add more catch data
#' 2) changes the numbers of catch data
#' 3) changes the end year
#' 
#' It then writes a new data file into the working directory
#'
#' @param ss_file_in filename of original dat file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new dat file with full path or relative to working directory
#' @param t_asmt how often assessments are run in years (e.g if every three years, 3)
#' @param nrep how many times the catch matrix has to be replicated (1 means it remains as is)
#' @param cdat_new set of new catch data, based on previous assessment time TAC
#' @return A modified dat file.
#' @author Desiree Tommasi

change_dat_boot <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new){
  
  #read in data file from previous assessment period
  om_dat = SS_readdat(ss_file_in)
  
  #create a new data file based on the old to be modified
  om_dat_new = om_dat
  
  #change end year to the end of next assessment period
  om_dat_new$endyr = om_dat$endyr + t_asmt
  
  #generate a vector of years for the catch data based on the end year of the previous data file and the assessment frequency
  ldat=length(cdat_new$catch)
  cyear=rep(1,ldat*t_asmt)
  for (j in 1:t_asmt){
    cyear[(j*ldat-ldat+1):(j*ldat)] = rep((om_dat$endyr+j),ldat)}
  
  #repeat catch data according to the assessment period
  cdat_new3 = data.frame(year=cyear,seas=rep(cdat_new$Seas,ldat),fleet=rep(cdat_new$Fleet,ldat),catch=rep(cdat_new$catch,ldat),catch_se=rep(0.1,ldat*t_asmt))
  
  #change the catch data
  catch_new = rbind(om_dat$catch, cdat_new3)
  om_dat_new$catch = catch_new
  
  #Write a stock synthesis data file 
  SS_writedat(om_dat_new, ss_file_out)
  
}
