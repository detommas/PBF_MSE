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
#' @param cdat_new2 set of new catch data, based on TAC from two previous assessments
#' @return A modified dat file.
#' @author Desiree Tommasi

change_dat_boot <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new, cdat_new2){
  
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
  #the first line repeats the TAC for the assessment cycle (tasmt)
  #however, season 1 and 2 are based actually on the old TAC (cdat_new2)
  #so catches for those season on the first year need to be replaced
  cdat_new3 = data.frame(year=cyear,seas=rep(cdat_new$Seas,t_asmt),fleet=rep(cdat_new$Fleet,t_asmt),catch=rep(cdat_new$catch,t_asmt),catch_se=rep(0.1,ldat*t_asmt))
  cdat_new3$catch[cdat_new3$seas%in%c(1,2)&cdat_new3$year==(om_dat$endyr + 1)]=cdat_new2$catch[cdat_new2$Seas%in%c(1,2)]
  #change the catch data
  catch_new = rbind(om_dat$catch, cdat_new3)
  om_dat_new$catch = catch_new
  
  #Write a stock synthesis data file 
  SS_writedat(om_dat_new, ss_file_out)
  
}
