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
#' @param forf specifies the the forecast report file from which the Fmult will be extracted
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param err is the implementation error per fleet , if 1 no implementation error
#' @param Cmin specifies the the minimum catch once the LRP has been breached

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR8_pbf_byfleet_f_25for <- function(ssout, dat, forf, yr, SSBtrs, SSBlim, Ftgt,cr, err, Cmin, hs,hcr,scn,itr,tstep, yrb,yrf,tacl,TACdt,TACEdt,TACWldt,TACWsdt, TACmat){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = dat[(dat$Yr==yr),]$SSB
  
  #Read in TAC from previous management period
  Ccur = TACdt[(asmt_t[tstep]+(tasmt-2))]
  CcurWs = TACWsdt[(asmt_t[tstep]+(tasmt-2))]
  CcurWl = TACWldt[(asmt_t[tstep]+(tasmt-2))]
  CcurE = TACEdt[(asmt_t[tstep]+(tasmt-2))]
  
  #extract the F multiplier
  pattern = "Forecast_using_Fspr:"
  which.line = grep(pattern=pattern, x=forf)
  fmdat=forf[which.line]
  fmdat2 = unlist(strsplit(fmdat, split= " "))
  fmult = as.numeric(fmdat2[4])
  
  #Scale the fmult given the HCR and stock status
  
  if (SSBcur >= SSBtrs){
    Fm = fmult
    cage = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=forf,fmult=fmult,ffraction=(Fm/fmult))
    
    #Computes the overall TAC and by area and large vs. small fish for WP
  #add a fleet type factor
  cage2 = cage %>%
    mutate(Size = case_when(
      Age %in% c(0:2) ~ "small",
      Age %in% c(3:20) ~ "Large"))
  
  #Calculate the potential TAC by fleet and season
  #sum across ages to get TAC by fleet and seas
  TAC_table = as.data.frame(cage2 %>% group_by(Fleet, Seas) %>% summarize(TAC=sum(yield)))
  
  #calculate catch ratio with which to split the medium fllet TAC by fleet and season
  mcr=TAC_table$TAC[TAC_table$Fleet %in% c(11,17:19)]/sum(TAC_table$TAC[TAC_table$Fleet %in% c(11,17:19)])
  
  #Calculate the potential TAC by fleet segment
  Chcr = sum(cage2$yield[cage2$Fleet %in% c(1:23)])
  ChcrWs = sum(cage2$yield[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
  ChcrWl = sum(cage2$yield[cage2$Fleet %in% c(1:7)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
  ChcrE = sum(cage2$yield[cage2$Fleet %in% c(20:23)])
  
  #compute the potential change in TAC by fleet segment
  Cchange = (Chcr-Ccur)/Ccur
  CchangeWs = (ChcrWs-CcurWs)/CcurWs
  CchangeWl = (ChcrWl-CcurWl)/CcurWl
  CchangeE = (ChcrE-CcurE)/CcurE
  
  #Generate an initial TAC table by fleet and season that will be fed back into OM
  #import one from previous time step
  #use data from existing 2025 CMM for tstep =1
  if (tstep==1){
    names(ciom25)[3]="seas"
    cmmdat = rbind((ciom %>% filter (year==2024&seas%in% c(3,4))),ciom25)
    #order by fleet and season
    cmmdat2 = cmmdat[order(cmmdat$fleet, cmmdat$seas),]
    TAC_tablei = data.frame(Fleet=cmmdat2$fleet,Seas=cmmdat2$seas, TAC=cmmdat2$catch)
  } else {
    TAC_tablei = TACmat$TAC_flt
  }
  
  #generate a TAC table that can be modified
  TAC_tablei2 = TAC_tablei
  
  #Calculates the TAC by fleet and age if the HCR determined TAC has a change greater than tacl
  if (tacl>0){
    if (abs(CchangeWs) > (tacl/100) & CchangeWs > 0){
      TACWs = CcurWs*(1+tacl/100)
      #set the TAC to the previous + tacl for the specific small fleets
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(8:10,12:16)]*(1+tacl/100)
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    } else if (abs(CchangeWs) > (tacl/100) & CchangeWs < 0){
      TACWs = CcurWs*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(8:10,12:16)]*(1-tacl/100)
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    } else {
      TACWs = sum(cage2$yield[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_table$TAC[TAC_table$Fleet %in% c(8:10,12:16)]
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    }
    #do for Wl
    if (abs(CchangeWl) > (tacl/100) & CchangeWl > 0){
      TACWl = CcurWl*(1+tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(1:7)]*(1+tacl/100)
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    } else if (abs(CchangeWl) > (tacl/100) & CchangeWl < 0){
      TACWl = CcurWl*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(1:7)]*(1-tacl/100)
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    } else {
      TACWl = sum(cage2$yield[cage2$Fleet %in% c(1:7)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_table$TAC[TAC_table$Fleet %in% c(1:7)]
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    }
    
    #Apply the TAC to the mixed fleets
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(11,17:19)]=(ms+ml)*mcr
    
    #do for EPO fleets
    if (abs(CchangeE) > (tacl/100) & CchangeE > 0){
      TACE = CcurE*(1+tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(20:23)]*(1+tacl/100)
      #cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1+tacl/100)
    } else if (abs(CchangeE) > (tacl/100) & CchangeE < 0){
      TACE = CcurE*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(20:23)]*(1-tacl/100)
      #cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1-tacl/100)
    } else {
      cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$yield[cage2$Fleet %in% c(20:23)]
      TACE = sum(cage2$TAC[cage2$Fleet %in% c(20:23)])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_table$TAC[TAC_table$Fleet %in% c(20:23)]
    }
    
    #Compute total TAC
    TAC=TACE+TACWl+TACWs
  } else {
    cage2$TAC = cage2$yield
    TAC = sum(cage2$TAC[cage2$Fleet %in% c(1:23)])
    TACWs = sum(cage2$TAC[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
    TACWl = sum(cage2$TAC[cage2$Fleet %in% c(1:7)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
    TACE = sum(cage2$TAC[cage2$Fleet %in% c(20:23)])
    TAC_tablei$TAC=TAC_table$TAC
  }
  
  #SET DISCARDS
  #might be useful to have a "perfect management" run where discards are as set by management
  #so if err=0 discards fleets are equal to yield, if not fleet 24 = 5% of all WPO fleets except 14, 
  #F25 = 100% of F14 and F26 1.2% of F22
  if (err==0){
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24:26)] = TAC_table$TAC[TAC_table$Fleet %in% c(24:26)]
  } else {
    tempc=TAC_tablei2 %>% filter(Fleet %in% c(1:13,15:19))
    tempcs= tempc %>% group_by(Seas) %>% summarize(TACl=sum(TAC))
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24)]=0.05*tempcs$TACl
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(25)]=1*TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(14)]
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(26)]=0.012*TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(22)]
  }
  
  Discard = sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24:26)]) #this are the discards assumed by the EM when figuring out the TAC
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACE=TACE, Discards=Discard, Ftarget=fmult, Fmultiplier=Fm, TAC_flt = TAC_tablei2)
  
  } else if (SSBcur > SSBlim) {
    Fm = (fmult/SSBtrs)*SSBcur
    cage = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=forf,fmult=fmult,ffraction=(Fm/fmult))
    
#Computes the overall TAC and by area and large vs. small fish for WP
  #add a fleet type factor
  cage2 = cage %>%
    mutate(Size = case_when(
      Age %in% c(0:2) ~ "small",
      Age %in% c(3:20) ~ "Large"))
  
  #Calculate the potential TAC by fleet and season
  #sum across ages to get TAC by fleet and seas
  TAC_table = as.data.frame(cage2 %>% group_by(Fleet, Seas) %>% summarize(TAC=sum(yield)))
  
  #calculate catch ratio with which to split the medium fllet TAC by fleet and season
  mcr=TAC_table$TAC[TAC_table$Fleet %in% c(11,17:19)]/sum(TAC_table$TAC[TAC_table$Fleet %in% c(11,17:19)])
  
  #Calculate the potential TAC by fleet segment
  Chcr = sum(cage2$yield[cage2$Fleet %in% c(1:23)])
  ChcrWs = sum(cage2$yield[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
  ChcrWl = sum(cage2$yield[cage2$Fleet %in% c(1:7)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
  ChcrE = sum(cage2$yield[cage2$Fleet %in% c(20:23)])
  
  #compute the potential change in TAC by fleet segment
  Cchange = (Chcr-Ccur)/Ccur
  CchangeWs = (ChcrWs-CcurWs)/CcurWs
  CchangeWl = (ChcrWl-CcurWl)/CcurWl
  CchangeE = (ChcrE-CcurE)/CcurE
  
  #Generate an initial TAC table by fleet and season that will be fed back into OM
  #import one from previous time step
  #use data from existing 2025 CMM for tstep =1
  if (tstep==1){
    names(ciom25)[3]="seas"
    cmmdat = rbind((ciom %>% filter (year==2024&seas%in% c(3,4))),ciom25)
    #order by fleet and season
    cmmdat2 = cmmdat[order(cmmdat$fleet, cmmdat$seas),]
    TAC_tablei = data.frame(Fleet=cmmdat2$fleet,Seas=cmmdat2$seas, TAC=cmmdat2$catch)
  } else {
    TAC_tablei = TACmat$TAC_flt
  }
  
  #generate a TAC table that can be modified
  TAC_tablei2 = TAC_tablei
  
  #Calculates the TAC by fleet and age if the HCR determined TAC has a change greater than tacl
  if (tacl>0){
    if (abs(CchangeWs) > (tacl/100) & CchangeWs > 0){
      TACWs = CcurWs*(1+tacl/100)
      #set the TAC to the previous + tacl for the specific small fleets
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(8:10,12:16)]*(1+tacl/100)
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    } else if (abs(CchangeWs) > (tacl/100) & CchangeWs < 0){
      TACWs = CcurWs*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(8:10,12:16)]*(1-tacl/100)
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    } else {
      TACWs = sum(cage2$yield[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)] = TAC_table$TAC[TAC_table$Fleet %in% c(8:10,12:16)]
      #find leftover small TAC for medium sized fleet
      ms=TACWs-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(8:10,12:16)])
    }
    #do for Wl
    if (abs(CchangeWl) > (tacl/100) & CchangeWl > 0){
      TACWl = CcurWl*(1+tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(1:7)]*(1+tacl/100)
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    } else if (abs(CchangeWl) > (tacl/100) & CchangeWl < 0){
      TACWl = CcurWl*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(1:7)]*(1-tacl/100)
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    } else {
      TACWl = sum(cage2$yield[cage2$Fleet %in% c(1:7)])+sum(cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)] = TAC_table$TAC[TAC_table$Fleet %in% c(1:7)]
      #find leftover large TAC for medium sized fleet
      ml=TACWl-sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(1:7)])
    }
    
    #Apply the TAC to the mixed fleets
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(11,17:19)]=(ms+ml)*mcr
    
    #do for EPO fleets
    if (abs(CchangeE) > (tacl/100) & CchangeE > 0){
      TACE = CcurE*(1+tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(20:23)]*(1+tacl/100)
      #cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1+tacl/100)
    } else if (abs(CchangeE) > (tacl/100) & CchangeE < 0){
      TACE = CcurE*(1-tacl/100)
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_tablei$TAC[TAC_tablei$Fleet %in% c(20:23)]*(1-tacl/100)
      #cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1-tacl/100)
    } else {
      cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$yield[cage2$Fleet %in% c(20:23)]
      TACE = sum(cage2$TAC[cage2$Fleet %in% c(20:23)])
      TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(20:23)] = TAC_table$TAC[TAC_table$Fleet %in% c(20:23)]
    }
    
    #Compute total TAC
    TAC=TACE+TACWl+TACWs
    
  } else {
    cage2$TAC = cage2$yield
    TAC = sum(cage2$TAC[cage2$Fleet %in% c(1:23)])
    TACWs = sum(cage2$TAC[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
    TACWl = sum(cage2$TAC[cage2$Fleet %in% c(1:7)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
    TACE = sum(cage2$TAC[cage2$Fleet %in% c(20:23)])
    TAC_tablei2$TAC=TAC_table$TAC
  }
  
  #SET DISCARDS
  #might be useful to have a "perfect management" run where discards are as set by management
  #so if err=0 discards fleets are equal to yield, if not fleet 24 = 5% of all WPO fleets except 14, 
  #F25 = 100% of F14 and F26 1.2% of F22
  if (err==0){
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24:26)] = TAC_table$TAC[TAC_table$Fleet %in% c(24:26)]
  } else {
    tempc=TAC_tablei2 %>% filter(Fleet %in% c(1:13,15:19))
    tempcs= tempc %>% group_by(Seas) %>% summarize(TACl=sum(TAC))
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24)]=0.05*tempcs$TACl
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(25)]=1*TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(14)]
    TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(26)]=0.012*TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(22)]
  }
  
  Discard = sum(TAC_tablei2$TAC[TAC_tablei2$Fleet %in% c(24:26)]) #this are the discards assumed by the EM when figuring out the TAC
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACE=TACE, Discards=Discard, Ftarget=fmult, Fmultiplier=Fm, TAC_flt = TAC_tablei2)
  
  } else {
    TAC_table = Cmin #this is a table with the catch by fleet set by the old CMM plus the catches for the rec and discard fleets

    names(TAC_table)[3] = "TAC"
    
    TAC_dat = list(TAC=16294, TACWs=4725, TACWl=6591, TACE=4978, Discards=965.5956, Ftarget=fmult, Fmultiplier=NA, TAC_flt = TAC_table)
    
  }
  
  return(TAC_dat)
}