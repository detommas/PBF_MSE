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
#' @param forf specifies the the forecast report file from which the Fmult will be extracted
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the threshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param err is the implementation error per fleet , if 1 no implementation error
#' @param Fmin specifies the fraction of Ftarget that the minimum F is set to once the LRP is reached
#' @param tacl specifies limit on TAC change relative to previous period in % 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR1a_pbf_byfleet_f_25for <- function(ssout, dat, forf, yr, SSBtrs, SSBlim, err, Fmin, hs,hcr,scn,itr,tstep, yrb,yrf,tacl){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessment
  SSBcur = dat[(dat$Yr==yr),]$SSB
  
  #Extract current catch at age
  Curcage=ssout$catage[,-c(4,5)] %>% filter(Yr==yr)
  #extract current weight at age
  Wage=ssout$ageselex %>% filter(Yr==yr&Factor=="bodywt"&Fleet %in% c(1:26))
  #order by fleet and season as catch at age
  Wage2 = Wage[order(Wage$Fleet, Wage$Seas),]
  #Calculate Biomass at age
  Curyage = Curcage[,c(10:30)]*Wage2[,c(8:28)]
  Yage = cbind(Wage2[,c(2:4)],Curyage)
  
  #Compute current total catch and catch per fleet segment
  Ccur = sum((Yage %>% filter(Fleet %in% c(1:23)))[,c(4:24)])
  CcurWs = sum((Yage %>% filter(Fleet %in% c(8:10,12:16)))[,c(4:24)])+sum((Yage %>% filter(Fleet %in% c(11,17:19)))[,c(4:6)])
  CcurWl = sum((Yage %>% filter(Fleet %in% c(1:7)))[,c(4:24)])+sum((Yage %>% filter(Fleet %in% c(11,17:19)))[,c(7:24)])
  CcurE = sum((Yage %>% filter(Fleet %in% c(20:23)))[,c(4:24)])
 
  #extract the F multiplier
  pattern = "Forecast_using_Fspr:"
  which.line = grep(pattern=pattern, x=forf)
  fmdat=forf[which.line]
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
  cage = catch_calc_f(ssout=ssout,yearsb=yrb,yearsf=yrf,ben=forf,fmult=fmult,ffraction=(Fm/fmult))
  
  #Computes the overall TAC and by area and large vs. small fish for WP
  #add a fleet type factor
  cage2 = cage %>%
    mutate(Size = case_when(
      Age %in% c(0:2) ~ "small",
      Age %in% c(3:20) ~ "Large"))
  
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
  
  #Turn current catch to long form
  Yagel = gather(Yage, Age, Ycur, `0`:`20`, factor_key=TRUE)
  #order in the same way as cage2
  Yagel2 = Yagel[order(Yagel$Fleet, Yagel$Seas, Yagel$Age),]
  
  #add current catch column to cage2
  cage2$Ycur = Yagel2$Ycur
  
  #Calculates the TAC by fleet and age if the HCR determined TAC has a change greater than tacl
  if (abs(CchangeWs) > (tacl/100) & CchangeWs > 0){
    cage2$TAC[cage2$Fleet %in% c(8:10,12:16)]=cage2$Ycur[cage2$Fleet %in% c(8:10,12:16)]*(1+tacl/100)
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]=cage2$Ycur[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]*(1+tacl/100)
  } else if (abs(CchangeWs) > (tacl/100) & CchangeWs < 0){
    cage2$TAC[cage2$Fleet %in% c(8:10,12:16)]=cage2$Ycur[cage2$Fleet %in% c(8:10,12:16)]*(1-tacl/100)
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]=cage2$Ycur[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]*(1-tacl/100)
  } else {
    cage2$TAC[cage2$Fleet %in% c(8:10,12:16)]=cage2$yield[cage2$Fleet %in% c(8:10,12:16)]
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]=cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"]
  }
    
  #do for Wl
  if (abs(CchangeWl) > (tacl/100) & CchangeWl > 0){
    cage2$TAC[cage2$Fleet %in% c(1:7)]=cage2$Ycur[cage2$Fleet %in% c(1:7)]*(1+tacl/100)
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]=cage2$Ycur[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]*(1+tacl/100)
  } else if (abs(CchangeWl) > (tacl/100) & CchangeWl < 0){
    cage2$TAC[cage2$Fleet %in% c(1:7)]=cage2$Ycur[cage2$Fleet %in% c(1:7)]*(1-tacl/100)
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]=cage2$Ycur[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]*(1-tacl/100)
  } else {
    cage2$TAC[cage2$Fleet %in% c(1:7)]=cage2$yield[cage2$Fleet %in% c(1:7)]
    cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]=cage2$yield[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"]
  }
  
  #do for E fleets
  if (abs(CchangeE) > (tacl/100) & CchangeE > 0){
    cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1+tacl/100)
  } else if (abs(CchangeE) > (tacl/100) & CchangeE < 0){
    cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$Ycur[cage2$Fleet %in% c(20:23)]*(1-tacl/100)
  } else {
    cage2$TAC[cage2$Fleet %in% c(20:23)]=cage2$yield[cage2$Fleet %in% c(20:23)]
  }
  
  if (SSBlim > SSBcur){
    cage2$TAC = cage2$yield
  }
  
  #SET DISCARDS
  #might be useful to have a "perfect management" run where discards are as set by management
  #so if err=0 discards fleets are equal to yield, if not fleet 24 = 5% of all WPO fleets except 14, 
  #F25 = 100% of F14 and F26 1.2% of F22
  if (err==0){
    cage2$TAC[cage2$Fleet %in% c(24:26)]=cage2$yield[cage2$Fleet %in% c(24:26)]
  } else {
    tempc=cage2 %>% filter(Fleet %in% c(1:13,15:19))
    tempcs= tempc %>% group_by(Seas,Age) %>% summarize(TACl=sum(TAC))
    cage2$TAC[cage2$Fleet %in% c(24)]=0.05*tempcs$TACl
    cage2$TAC[cage2$Fleet %in% c(25)]=1*cage2$TAC[cage2$Fleet %in% c(14)]
    cage2$TAC[cage2$Fleet %in% c(26)]=0.012*cage2$TAC[cage2$Fleet %in% c(22)]
  }
  
  #Compute the TAC by fleet segment
  TAC = sum(cage2$TAC[cage2$Fleet %in% c(1:23)])
  TACWs = sum(cage2$TAC[cage2$Fleet %in% c(8:10,12:16)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="small"])
  TACWl = sum(cage2$TAC[cage2$Fleet %in% c(1:7)])+sum(cage2$TAC[cage2$Fleet %in% c(11,17:19)&cage2$Size=="Large"])
  TACE = sum(cage2$TAC[cage2$Fleet %in% c(20:23)])
  Discard = sum(cage2$TAC[cage2$Fleet %in% c(24:26)]) #this are the discards assumed by the EM when figuring out the TAC

  #sum across ages to get TAC by fleet and seas
  TAC_table = as.data.frame(cage2 %>% group_by(Fleet, Seas) %>% summarize(TAC=sum(TAC)))
  
  TAC_dat = list(TAC=TAC, TACWs=TACWs, TACWl=TACWl, TACE=TACE, Discards=Discard, Ftarget=fmult, Fmultiplier=Fm, TAC_flt = TAC_table)
  
  return(TAC_dat)
}