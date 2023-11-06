#' Calculates the TAC based on HCR1a from the NC15 Annex F
#' 
#' This HCR has the following specifications
#' -computes a TAC
#' -no rebuilding plan, TAC = TAC min if SSBcur<SSBlim
#' -management action occurs when either SSBcur<SSBthreshold or when SSBcur<SSBlim with a 50% probability
#' -allocation of the TAC is simply based on the relF specified in the forecast file
#' -HCR controls all fleets
#' 
#' Here the exploitation rate is the ratio of the total catch in weight over the total biomass
#' @param ssout SS output file
#' @param dat specifies the data frame which contains the sprseries data extracted from the stock assessment output
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param err is the implementation error per fleet , if 1 no implementation error
#' @param Cmin specifies the the minimum catch once the LRP has been breached

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR8_pbf_byfleet_f <- function(ssout, dat, yr, SSBtrs, SSBlim, err, Cmin, hs,hcr,scn,itr,tstep, yrb,yrf){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = (dat %>% filter(Yr==yr))$SSB
  
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
    cr = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=ben,fmult=fmult,ffraction=(Fm/fmult))
    
    #add a fleet type factor
    TAC_table = cr %>%
      mutate(Ftype = case_when(
        Fleet %in% c(13:15,29) ~ "EPO",
        Fleet %in% c(2,6:8,16,18,19,20) ~ "WPOs",
        Fleet %in% c(1,4,5,12,17,28) ~ "WPOl",
        Fleet %in% c(3,9:11) ~ "WPOm",
        Fleet %in% c(26,27,30) ~ "Disc"))
    
    #adds an implementation error - here it is the same across fleets.
    #Note that discrds are already accounted for by the specific fleet, this is an implementaiton error in addition to that
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
    Discard = TAC_table %>% filter(Ftype=="Disc") %>% summarize(TAC=sum(TAC_err))
    
    TAC_dat = list(TAC=TAC, TACWs=TACWs$TAC, TACWl=TACWl$TAC, TACWm=TACWm$TAC, TACE=TACE$TAC, Discards=Discard$TAC, TAC_flt = TAC_table)
  } else if (SSBcur > SSBlim) {
    Fm = (fmult/SSBtrs)*SSBcur
    cr = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=ben,fmult=fmult,ffraction=(Fm/fmult))
    
    #add a fleet type factor
    TAC_table = cr %>%
      mutate(Ftype = case_when(
        Fleet %in% c(13:15,29) ~ "EPO",
        Fleet %in% c(2,6:8,16,18,19,20) ~ "WPOs",
        Fleet %in% c(1,4,5,12,17,28) ~ "WPOl",
        Fleet %in% c(3,9:11) ~ "WPOm",
        Fleet %in% c(26,27,30) ~ "Disc"))
    
    #adds an implementation error - here it is the same across fleets.
    #Note that discards are already accounted for by the specific fleet, this is an implementaiton error in addition to that
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
    Discard = TAC_table %>% filter(Ftype=="Disc") %>% summarize(TAC=sum(TAC_err))
    
    TAC_dat = list(TAC=TAC, TACWs=TACWs$TAC, TACWl=TACWl$TAC, TACWm=TACWm$TAC, TACE=TACE$TAC, Discards=Discard$TAC, TAC_flt = TAC_table)
  } else {
    cr = Cmin
    names(cr)[3]=c("Catch")
    cr$yield=cr$Catch
    #add a fleet type factor
    TAC_table = cr %>%
      mutate(Ftype = case_when(
        Fleet %in% c(13:15,29) ~ "EPO",
        Fleet %in% c(2,6:8,16,18,19,20) ~ "WPOs",
        Fleet %in% c(1,4,5,12,17,28) ~ "WPOl",
        Fleet %in% c(3,9:11) ~ "WPOm",
        Fleet %in% c(26,27,30) ~ "Disc"))
    
    TAC_table$TAC_err = TAC_table$yield*err
    
    TAC_dat = list(TAC=16330, TACWs=2718.068, TACWl=7624, TACWm=305.706, TACE=3961.289, Discards=1720.936, TAC_flt = TAC_table)
    
  }
  
  return(TAC_dat)
}