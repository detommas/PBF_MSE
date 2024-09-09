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

PBF_MSE_cageb = function(hsnum,hcrnum,scnnum,itr, Bthr, Blim, sa, Fmin, lag, obse, aspm, yfor,tacl) { 

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
bwmat=true_out$ageselex%>%filter(Factor=="bodywt"&Yr%in%2023:2045)

catchab_sim=catcha_sim
for (y in 2023:2045){
  for(f in 1:26){
    for (s in 1:4){
      for (a in 0:20){
        catchab_sim[catchab_sim$Yr==y&catchab_sim$Fleet==f&catchab_sim$Seas==s,9+a]=bwmat[bwmat$Yr==y&bwmat$Fleet==f&bwmat$Seas==s,8+a]*catcha_sim[catcha_sim$Yr==y&catcha_sim$Fleet==f&catcha_sim$Seas==s,9+a]
      }
    }
}
}
#assign to EPO/WPO based on fleet number
catchab_sim$Region = "WPO"
catchab_sim$Region[catchab_sim$Fleet %in% c(20:23,26)] = "EPO"

#assign to a fleet type based on fleet number
catchab_sim$ftype = "EPO"
catchab_sim$ftype[catchab_sim$Fleet==26] = "EPOd"
catchab_sim$ftype[catchab_sim$Fleet %in% c(1:7)] = "WPOl"
catchab_sim$ftype[catchab_sim$Fleet %in% c(8:10,12:16)] = "WPOs"
catchab_sim$ftype[catchab_sim$Fleet %in% c(17:19)] = "WPOmj"
catchab_sim$ftype[catchab_sim$Fleet==11] = "WPOmk"
catchab_sim$ftype[catchab_sim$Fleet==25] = "WPOsd"
catchab_sim$ftype[catchab_sim$Fleet==24] = "WPOmd"

#sum catch across seasons
catchab_sum = as.data.frame(catchab_sim %>% group_by(Yr,ftype) %>% summarise(
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

#separate so only year is in rows
credat = catchab_sum %>% filter(ftype=="EPO")
crwsdat = catchab_sum %>% filter(ftype=="WPOs")
crwldat = catchab_sum %>% filter(ftype=="WPOl")
crwmjdat = catchab_sum %>% filter(ftype=="WPOmj")
crwmkdat = catchab_sum %>% filter(ftype=="WPOmk")
creddat = catchab_sum %>% filter(ftype=="EPOd")
crwsddat = catchab_sum %>% filter(ftype=="WPOsd")
crwmddat = catchab_sum %>% filter(ftype=="WPOmd")
Cabdat = cbind(credat,crwsdat[,2:23],crwldat[,2:23],crwmjdat[,2:23],crwmkdat[,2:23],creddat[,2:23],crwsddat[,2:23],crwmddat[,2:23])

#save output to file
write.table(Cabdat, paste(pdir,hs, hcr,scn, itr,"/cablist.txt", sep =""))

return(Cabdat)
}