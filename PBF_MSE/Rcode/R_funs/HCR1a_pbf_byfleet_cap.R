#' Calculates the TAC based on HCR1a from the NC15 Annex F with a catch limit specified by the CMM
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
#' @param ssout SS output file
#' @param dat specifies the data frame which contains the sprseries data extracted from the stock assessment output
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param Ftgt is the fishing intensity that produces the specified SPR target. It is computed as an exploitation rate, 
#' which is the ratio of the total annual catch to the biomass at the start of the year.
#' @param cr is the catch ratio per fleet (by gear/country)
#' @param err is the implementation error per fleet , if 1 no implementation error
#' @param Fmin specifies the fraction of Ftarget that the minimum F is set to once the LRP is reached

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR1a_pbf_byfleet_cap <- function(ssout, dat, yr, SSBtrs, SSBlim, Ftgt,cr, err, Fmin, hs,hcr,scn,itr,tstep, yrb,yrf){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = (dat %>% filter(Yr==yr))$SSB
  
  #Extract the current total biomass
  Btot = (dat %>% filter(Yr==yr))$Bio_Smry.1
  
  #read the benchmark infrormaiton from the forecast report file 
  ben_file_in = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/Forecast-report.SSO", sep = "")
  benf=read.table(ben_file_in, fill = TRUE, quote = "",
                  colClasses = "character", nrows = -1)
  ben <- readLines(ben_file_in, warn = FALSE)
  
  #extract the F multiplier
  fmdat=ben[36]
  fmdat = gsub("^\\s+|\\s+$", "", fmdat) # remove leading blank
  fmdat = gsub("\\s+", " ", fmdat)       # remove >1 blanks
  fmdat = as.numeric(unlist(strsplit(fmdat, split= " ")))
  fmult = fmdat[2]
  
  #Scale the fmult given the HCr and stock status
  
  if (SSBcur >= SSBtrs){
    Fm = fmult
  } else if (SSBcur > SSBlim) {
    Fm = ((fmult-(Fmin*fmult))/(SSBtrs-SSBlim))*(SSBcur-SSBlim)+Fmin*fmult
  } else {
    Fm=Fmin*Fmult
  }
  
  #Calculate the total TAC given the current numbers at age, fmultiplier and biology and exploitation pattern 
  cr = catch_calc(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=ben,fmult=Fm)

  #add a fleet type factor
  TAC_table = cr %>%
    mutate(Ftype = case_when(
      Fleet %in% c(13:15,29,30) ~ "EPO",
      Fleet %in% c(2,6:8,16,18,19,20,27) ~ "WPOs",
      Fleet %in% c(1,4,5,12,17,28) ~ "WPOl",
      Fleet %in% c(3,9:11,26) ~ "WPOm"))
  
  #adds an implementation error - here it is the same across fleets.
  #Note that when the catch is split back across fleet for input into the OM
  #the error will already be included
  #If the error differed by fleet, it should be included when the TAC is split across fleets
  TAC_table$TAC_err = TAC_table$yield*err
  
  #Computes the overall TAC and by area and large vs. small fish for WP
  TAC = sum(TAC_table$TAC_err)
  TACWs = TAC_table %>% filter(Ftype=="WPOs") %>% summarize(TAC=sum(TAC_err))
  TACWl = TAC_table %>% filter(Ftype=="WPOl") %>% summarize(TAC=sum(TAC_err))
  TACWm = TAC_table %>% filter(Ftype=="WPOm") %>% summarize(TAC=sum(TAC_err))
  TACE = TAC_table %>% filter(Ftype=="EPO") %>% summarize(TAC=sum(TAC_err))
  
  #make sure catch limits are not exceeded, if so set to limit
  if (TACWs$TAC > 4341) {TACWs$TAC = 4341}
  if (TACWl$TAC > 7624) {TACWl$TAC = 7624}
  if (TACWm$TAC > 370) {TACWm$TAC = 370}
  if (TACE$TAC > 3995) {TACE$TAC = 3995}
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACWm=TACWm, TACE=TACE, TAC_flt = TAC_table)
  
  return(TAC_dat)
}