#' Runs the PBF MSE framework for the specified number of iterations
#' Calculates catch per fleet using the relF and fmultiplier
#' Note that this was created to be run using the wrapper function PBR_MSE_prll.R
#' @param hsnum number characterizing the harvest strategy being run
#' @param hcrnum number characterizing the harvest control rule being run
#' @param scnnum number characterizing the uncertainity scenario being run
#' @param itr the iteration that one wants to run
#' @param Bthr the fraction of unifished biomass the threshold reference point refers to
#' @param FBtrh the fraction of Btrh to consider for the threshold reference point
#' @param Blim the fraction of unfished biomass the limit reference point refers to
#' @param sa will a stock assessment be carried out or not, sa=0=no stock assessment, sa=1=stock assessment/em
#' @param lag lag between data and assessement output and management, if 0 assessment output sets the TAC fir the following year
#' @param obse selects if perfect data (2) or data with error is fed into the EM, by specifying the datatype taken from the OM bootstrap data file
#' @param aspm aspm switch: = NULL (no ASPM, default),
#'                            "ASPM",
#'                            "ASPM-size" (w/ size for all fleets),
#'                            "ASPMR" (ASPM-R),
#'                            "ASPMR-size" (ASPM-R w/ size for all fleets),
#'                            "ASPMR-sizef1f12" (ASPM-R w/size for all fleets, F1 & F12 selectivities),
#'                            "ASPMR-f1f12" (ASPM-R w/ size & selectivities for F1 & F12)
#'@param yfor years over which selectivity is averaged for forecast calculations
#'@param tacl limit on TAc change from previous management period in %
#' Note that the F target is specified in the forecast file. The fishing intensity is measured as the spawning potential ratio (SPR)
#' @return a data frame of output for performance statistics
#' @author D.Tommasi

PBF_MSE_cage = function(hsnum,hcrnum,scnnum,itr, Bthr, Blim, sa, Fmin, lag, obse, aspm, yfor,tacl) { 

#specify the path for the harvest strategy that is being run
hs = paste(hsnum, "/", sep = "")
hsw = paste(hsnum, "\\", sep = "")

#specify the path for the harvest control rule (i.e. combination of reference points) within that hs that is being run
hcr = paste(hcrnum, "/", sep = "")
hcrw = paste(hcrnum, "\\", sep = "")

#specify the path for the scenario (i.e. om model type) that hs that is being run
scn = paste(scnnum, "/", sep = "")
scnw = paste(scnnum, "\\", sep = "")

#Specify parent directories path 
pdir = "D:/Desiree/PBF_MSE/"

#****************************************************************************
tstep=8
#Extract from the OM catch at age data 
out_dir = paste(pdir, hs, hcr, scn, itr,"/",tstep, "/OM/", sep="")
true_out = SS_output(out_dir, covar = FALSE, ncols = 250)

#select the catch at age from the simulation period (2023 onwards) and remove column with duplicate names
catcha_sim = true_out$catage[,-c(4,5,10)] %>% filter(Yr>2022&Yr<2046)

#assign to EPO/WPO based on fleet number
catcha_sim$Region = "WPO"
catcha_sim$Region[catcha_sim$Fleet %in% c(20:23,26)] = "EPO"

#sum catch across seasons
catcha_sum = as.data.frame(catcha_sim %>% group_by(Yr,Region) %>% summarise(
  c0=sum(`0`),
  c1=sum(`1`),
  c2=sum(`2`),
  c3=sum(`3`),
  c4=sum(`4`),
  c5=sum(`5`),
  c6=sum(`6`),
  c7=sum(`7`),
  c8=sum(`8`),
  c9=sum(`9`),
  c10=sum(`10`),
  c11=sum(`11`),
  c12=sum(`12`),
  c13=sum(`13`),
  c14=sum(`14`),
  c15=sum(`15`),
  c16=sum(`16`),
  c17=sum(`17`),
  c18=sum(`18`),
  c19=sum(`19`),
  c20=sum(`20`)))

#seprate so only year is in rows
credat = catcha_sum %>% filter(Region=="EPO")
crwdat = catcha_sum %>% filter(Region=="WPO")
Cadat = cbind(credat,crwdat[,2:23])

#save output to file
write.table(Cadat, paste(pdir,hs, hcr,scn, itr,"/calist.txt", sep =""))

return(Cadat)
}