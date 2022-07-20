#' Calculates the TAC based on HCR1a from the NC15 Annex F
#' 
#' This HCR has the following specifications
#' -computes a TAC
#' -no rebuilding plan, TAC = TAC min if SSBcur<SSBlim
#' -management action occurs when either SSBcur<SSBthreshold or when SSBcur<SSBlim with a 50% probability
#' -allocation of the TAC is simply based on the average 2017-2019 allocation ***need to develop this to be based on 
#' proportional fishing impact
#' -HCR controls all fleets
#' 
#' Here the exploitation rate is the ratio of the total catch in weight over the total biomass
#' @param dat specifies the data frame which contains the sprseries data extracted from the stock assessment output
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param Ftgt is the fishing intensity that produces the specified SPR target. It is computed as an exploitation rate, 
#' which is the ratio of the total annual catch to the biomass at the start of the year.
#' @param cr is the catch ratio per fleet (by gear/country)
#' @param err is the implementation error per fleet , if 1 no implementation error
 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR1a_pbf_byfleet <- function(dat, yr, SSBtrs, SSBlim, Ftgt,cr, err){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = (dat %>% filter(Yr==yr))$SSB
  
  #Extract the current total biomass
  Btot = (dat %>% filter(Yr==yr))$Bio_all.1
  
  #compute the f multiplier given the LRP and the current female spawning biomass
  #The fmulti determines how large the exploitation rate relative to Ftgt should be
  
  if (SSBcur >= SSBtrs){
    fmulti = 1
  } else if (SSBcur > SSBlim) {
    fmulti = SSBcur/SSBtrs
  } else {
    fmulti=0
  }
  
  #Calculates the exploitation rate given the status of the stock
  Er = Ftgt*fmulti
  
  #Calculate the total TAC given the current biomass
  TAC = Er*Btot

  #splits the total catch given the catch ratios
  cr$TAC_fleet = TAC*cr$cratio
  
  #add a fleet type factor
  TAC_table = cr %>%
    mutate(Ftype = case_when(
      Fleet %in% c(13:15,29,30) ~ "EPO",
      Fleet %in% c(2,3,5:9,18,19,20,26,27) ~ "WPOs",
      Fleet %in% c(1,4,10:12,16,17,28) ~ "WPOl"))
  
  
  #adds an implementation error - here it is the same across fleets.
  #Note that when the catch is split back across fleet for input into the OM
  #the error will already be included
  #If the error differed by fleet, it should be included when the TAC is split across fleets
  TAC_table$TAC_err = TAC_table$TAC_fleet*err
  
  #Computes the overall TAC by area and large vs. small fish for WP
  TACWs = TAC_table %>% filter(Ftype=="WPOs") %>% summarize(TAC=sum(TAC_err))
  TACWl = TAC_table %>% filter(Ftype=="WPOl") %>% summarize(TAC=sum(TAC_err))
  TACE = TAC_table %>% filter(Ftype=="EPO") %>% summarize(TAC=sum(TAC_err))
  
  #make sure catch limits are not exceeded, if so set to limit
  if (TACWs$TAC > 4475) {TACWs$TAC = 4475}
  if (TACWl$TAC > 7860) {TACWl$TAC = 7860}
  if (TACE$TAC > 3995) {TACE$TAC = 3995}
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACE=TACE, TAC_flt = TAC_table)
  
  return(TAC_dat)
}