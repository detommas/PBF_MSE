#' Re-runs the last OM for each of the 100 itr of a specified hcr of the 
#' bluefin MSE to calculate the proportional impact 
#' and saves the output in a text file
#' @param pdir the parent directory path
#' @param hs the harvest strategy that impact is being calculated for
#' @param hcr the hcr that impact is being calculated for
#' @param scn the om that impact is being calculated for
#' @param itr the iteration impact is being calculated for
#' @param tstep the time step impact is being calculated for
#' @param idat the data frame that will contain the impact output
#' @param indx the index for each iteration to be added to the data frame
#' @param itr the iteration data is being extracted for

#' @return impact data frame with the the proportional impact for each grouping
#' @author Desiree Tommasi

impact_pbfMSE_prllitr_omdone <- function(pdir, hs, hcr,scn, itr,tstep,ssdir,idat,indx){

  itr_d= paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OM",sep = "")
  impact_calc_h1_omdone(Dir=itr_d, ssDir=ssdir)

}