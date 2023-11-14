#' Calculates the TAC based on HCR1a from the NC15 Annex F
#' 
#' This HCR has the following specifications
#' -computes a TAC
#' -no rebuilding plan, TAC = TAC min if SSBcur<SSBlim
#' -management action occurs when either SSBcur<SSBthreshold or when SSBcur<SSBlim with a 50% probability
#' -allocation of the TAC is simply based on the relF specified in the forecast
#' -HCR controls all fleets
#' 
#' Here the exploitation rate is the ratio of the total catch in weight over the total biomass
#' @param ssout SS output file
#' @param dat specifies the data frame which contains the sprseries data extracted from the stock assessment output
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param err is the implementation error per fleet , if 1 no implementation error
#' @param Fmin specifies the fraction of Ftarget that the minimum F is set to once the LRP is reached

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR1a_pbf_byfleet_f_25for <- function(ssout, dat, yr, SSBtrs, SSBlim, err, Fmin, hs,hcr,scn,itr,tstep, yrb,yrf){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = (dat %>% filter(Yr==yr))$SSB
  
  #Extract the current catch
  Ccur = (ssout$catch %>% filter(Yr==yr))[,c(1,5,16)]
  Ctot = sum(Ccur$ret_bio)
  
  #read the benchmark information from the forecast report file 
  ben_file_in = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/Forecast-report.SSO", sep = "")
  ben <- readLines(ben_file_in, warn = FALSE)
  
  #extract the F multiplier
  pattern = "Forecast_using_Fspr:"
  which.line = grep(pattern=pattern, x=ben)
  fmdat=ben[which.line]
  fmdat2 = unlist(strsplit(fmdat, split= " "))
  fmult = as.numeric(fmdat2[4])
  
  #Scale the fmult given the HCR and stock status
  
  if (SSBcur >= SSBtrs){
    Fm = fmult
  } else if (SSBcur > SSBlim) {
    Fm = ((fmult-(Fmin*fmult))/(SSBtrs-SSBlim))*(SSBcur-SSBlim)+Fmin*fmult
  } else {
    Fm=Fmin*fmult
  }
  
  #Calculate the total TAC given the current numbers at age, fmultiplier and biology and exploitation pattern 
  cr = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=ben,fmult=fmult,ffraction=(Fm/fmult))
  
  #add a fleet type factor
  TAC_table = cr %>%
    mutate(Ftype = case_when(
      Fleet %in% c(13:15,29) ~ "EPO",
      Fleet %in% c(2,6:8,16,18,19,20) ~ "WPOs",
      Fleet %in% c(1,4,5,12,17,28) ~ "WPOl",
      Fleet %in% c(3,9:11) ~ "WPOm",
      Fleet %in% c(26,27,30) ~ "Disc"))
  
  #Ensure change in TAC is not more than 25% 
  Ttot = sum(TAC_table$yield)
  Cchange = (Ttot-Ctot)/Ctot
  #compute catch ratios from TAC calculation
  #need to ensure that these ratios, not those of catch current are used
  Ccur$taccr=TAC_table$yield/sum(TAC_table$yield)
  Ctot25p=Ctot*1.25
  Ctot25n=Ctot*0.75
  
  #adds an implementation error - here it is the same across fleets.
  #Note that discrds are already accounted for by the specific fleet, this is an implementaiton error in addition to that
  #Note that when the catch is split back across fleet for input into the OM
  #the error will already be included
  #If the error differed by fleet, it should be included when the TAC is split across fleets
  if (abs(Cchange) > 0.25 & Cchange > 0){
    TAC_table$TAC_err = Ccur$taccr*Ctot25p*err
    } else if (abs(Cchange) > 0.25 & Cchange < 0){
      TAC_table$TAC_err = Ccur$taccr*Ctot25n*err
    } else {
      TAC_table$TAC_err = TAC_table$yield*err
    }
  
  if (SSBlim > SSBcur){
    TAC_table$TAC_err = TAC_table$yield*err
  }
  
  #Computes the overall TAC and by area and large vs. small fish for WP
  TAC = sum(TAC_table$TAC_err)
  TACWs = TAC_table %>% filter(Ftype=="WPOs") %>% summarize(TAC=sum(TAC_err))
  TACWl = TAC_table %>% filter(Ftype=="WPOl") %>% summarize(TAC=sum(TAC_err))
  TACWm = TAC_table %>% filter(Ftype=="WPOm") %>% summarize(TAC=sum(TAC_err))
  TACE = TAC_table %>% filter(Ftype=="EPO") %>% summarize(TAC=sum(TAC_err))
  Discard = TAC_table %>% filter(Ftype=="Disc") %>% summarize(TAC=sum(TAC_err))
  
  #make sure catch limits are not exceeded, if so set to limit
  #if (TACWs$TAC > 4341) {TACWs$TAC = 4341}
  #if (TACWl$TAC > 7624) {TACWl$TAC = 7624}
  #if (TACWm$TAC > 370) {TACWm$TAC = 370}
  #if (TACE$TAC > 3995) {TACE$TAC = 3995}
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACWm=TACWm, TACE=TACE, Discards=Discard, TAC_flt = TAC_table)
  
  
  return(TAC_dat)
}