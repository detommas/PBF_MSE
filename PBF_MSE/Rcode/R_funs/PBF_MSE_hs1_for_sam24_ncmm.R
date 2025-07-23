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
#'                            "ASPMR-f1f3f6f21" (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth & F21EPO_COMM(2002-) & F6JPN_TPS_SOJ),
#'                            "ASPMR-f1f3f21"   (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth & F21EPO_COMM(2002-)),
#'                            "ASPMR-f1f3"      (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth),
#'                            "ASPMR-f3"        (ASPM-R w/ size & selectivities for F3TWN_LLSouth only)
#' @param yfor years over which selectivity is averaged for forecast calculations
#' @param tacl limit on TAc change from previous management period in %
#' @param is_qcreep switch for q creep of TW LL CPUE for robustness test: = FALSE (default), TRUE (q creep)
#' @param is_hi_discard specifies higher discards for robustness test : = FALSE (default), TRUE (higher discards) 
#' Note that the F target is specified in the forecast file. The fishing intensity is measured as the spawning potential ratio (SPR)
#' @return a data frame of output for performance statistics
#' @author D.Tommasi

PBF_MSE_hs1_for_sam24_ncmm = function(hsnum,hcrnum,scnnum,itr, Bthr, Blim, sa, Fmin, lag, obse, aspm, yfor,tacl,is_qcreep,is_hi_discard) { 

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
pdir = "J:/Desiree/PBF_MSE/"
pwin = "J:\\Desiree\\PBF_MSE\\"
#pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/PBF_MSE_main/PBF_MSE/"
#pwin = "C:\\Users\\desiree.tommasi\\Documents\\Bluefin\\Github\\PBF_MSE_main\\PBF_MSE\\"


#Specify the path of conditioned initial OM
#sdir = "D:/Desiree/PBF_MSE/Condition/"
sdir = paste0(pdir, "Condition\\")
#sdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/PBF_MSE_main/PBF_MSE/Condition/"

#Specify vectors where to save output (output is from OM unless otherwise specified) for the future simulation years
Rdat = 1:(length(asmt_t)*tasmt) # current recruits
SPBdat = 1:(length(asmt_t)*tasmt) # current spawning biomass
Btot = 1:(length(asmt_t)*tasmt) # current total biomass
Tdat = 1:(length(asmt_t)*tasmt) # current total catch
TACdt = 1:(length(asmt_t)*tasmt) # current total TAC
TACWsdt = 1:(length(asmt_t)*tasmt) # current total TAC for WPO small fish fleets
TACWldt = 1:(length(asmt_t)*tasmt) # current total TAC for WPO large fish fleets
TACEdt = 1:(length(asmt_t)*tasmt) # current total TAC for EPO
Ddat = 1:(length(asmt_t)*tasmt) # current depletion
SPRdat = 1:(length(asmt_t)*tasmt) # current fishing intensity (1-SPR)
B0dat = 1:(length(asmt_t)*tasmt) # SSB0 from em
B0dat_om = 1:(length(asmt_t)*tasmt) # SSB0 from om
Ftgt_om = 1:(length(asmt_t)*tasmt) # F target (exploitation intensity leading to the SPR specified in the forcast file) from om
Ftgt_em = 1:(length(asmt_t)*tasmt) # F target (exploitation intensity leading to the SPR specified in the forcast file) from em
Fhcr = 1:(length(asmt_t)*tasmt) #F set by the hcr
R_em = 1:(length(asmt_t)*tasmt)
SPB_em = 1:(length(asmt_t)*tasmt)
D_em = 1:(length(asmt_t)*tasmt)
SPR_em = 1:(length(asmt_t)*tasmt)
Btot_em = 1:(length(asmt_t)*tasmt)
C_em = 1:(length(asmt_t)*tasmt)

#Create directory
dir.create(paste0(pdir, hs, hcr))
dir.create(paste0(pdir, hs, hcr, scn))

#set working directory 
setwd(paste(pdir,hs, hcr, scn, sep = ""))

#create directory for each iteration (i.e. different recruitment and time varying selectivity errors)
cmddir = paste("mkdir", itr)
shell(cmd = cmddir)

#**************************************************************************************
#Generate recruitment deviations

#set working directory to base iteration
setwd(paste(pdir, hs, hcr, scn, itr, sep = ""))

#Create a folder where to store deviations for each MSE iteration (i.e. 30 year simulation), and the implementation error
shell(cmd = "mkdir Rec_dev")

rec_devs = recdevs_mse(itr, (length(asmt_t)*tasmt), 0.6)

write.table(rec_devs, paste(pdir, hs, hcr, scn, itr,"/Rec_dev/rec_devs.txt", sep = ""))

#**************************************************************************************

for (tstep in 1:length(asmt_t)){
  
  #create directory for new time step where the new dat file will be saved
  setwd(paste(pdir,hs, hcr, scn, itr, sep = ""))
  cmddir = paste("mkdir", tstep)
  shell(cmd = cmddir)
  
  #*************************************************************************************
  #Step 1: Modify original dat file to include the TAC as catch for the next three years
  #For the first year we choose the catch limits of the first projection scenario of the 2022 update
  
  #*************************************************************************************
  if (tstep == 1) {
    new_cdat = ciom
    new_cdat2 = NULL
    
  } else {
    TAC_file = paste(pdir, hs, hcr, scn, itr,"/TAC",tstep,".RData", sep = "")
    TACmat = readRDS(TAC_file)
    TACWs = TACmat$TACWs #catch limit for small fish fleets in western pacific
    TACWl= TACmat$TACWl #catch limit for large fish fleets in western pacific
    TACE= TACmat$TACE #catch limit for EPO
    TACdat = TACWs+TACWl+TACE
    
    new_cdat=TACmat$TAC_flt
    names(new_cdat)[3]="catch"
  }
  
  #Record the TAC - based on calendar year (everything else is FY), set in the previous time step but effective for the current, recorded for the calendar year
  #for the first tstep TAC is pre-specified (based on actual Cal 2023 catches and the CMMs, not the simulated output)
  if (tstep==1){
    TACdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = c(17493,17736.76,26052.76)
    TACWsdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = c(4180.541,4215.958,4962.379)
    TACWldt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = c(7924.952,8118.042,12031.62)
    TACEdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = c(5387.502,5402.761,9058.761)
  } else {
    TACdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = rep(TACdat,tasmt)
    TACWsdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = rep(TACWs,tasmt)
    TACWldt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = rep(TACWl,tasmt)
    TACEdt[(asmt_t[tstep]-1):(asmt_t[tstep]+(tasmt-2))] = rep(TACE,tasmt)
  }
  
  if (sa==0) {
    
    #Step 2: Estimate data with error and no error as input to EM and OM via SS Bootstrap routine
    OMBoot_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, new_cdat2, rec_devs, sdev, yfor)
    
    #****************************************************************************
    #Step 3: Generate files for operating model
    OM_fun_tvry_adj(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt)
    #create another OM that uses lagged data 
    if (lag>0){
      OM_fun_tvry_adj_lag(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, rec_devs,sdev, lag=lag,yfor=yfor)
    }
    #*********************************************************************************
    #Step 5: Compute TAC using OM model output
    
    #read OM output file and OM forecast report file
    if (lag==0) {
      out_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OM/", sep = "")
      ben_file_in = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/Forecast-report.SSO", sep = "")
    } else {
      out_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OMlag/", sep = "")
      ben_file_in = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OMlag/Forecast-report.SSO", sep = "")
    }
    
    om_out = SS_output(out_dir, covar = FALSE, ncols = 250)
    ben = readLines(ben_file_in, warn = FALSE)
    
    yr_end = om_out$endyr
    
    #In this example we are using candidate HCR1a from the NC15 Annex F
    #Compute the SSB associated with the threshold and limit biomass reference points 
    ssb_thr = brp_fun_pbf(ssoutput=om_out, fraction=Bthr)
    ssb_lim = brp_fun_pbf(ssoutput=om_out, fraction=Blim)
    
    #Extract SPR series data
    SPRmat = om_out$sprseries
    
    #Generate TAC based on current harvest control rule
    #specify years over which to compute biology (yrb) and exploitation pattern (yrf)
    TAC_mat = HCR1a_pbf_byfleet_f_25for(ssout=om_out, dat = SPRmat, forf=ben, yr=yr_end, SSBtrs=ssb_thr, SSBlim=ssb_lim, err=1, Fmin=Fmin,hs=hs,hcr=hcr,scn=scn,itr=itr,tstep=tstep,yrb=c(2002:2004),yrf=yfor,tacl=tacl,is_hi_discard=is_hi_discard, TACdt=TACdt,TACEdt=TACEdt,TACWldt=TACWldt,TACWsdt=TACWsdt, TACmat=TACmat)
    
    #Save the TAC
    file_tac = paste(pdir, hs, hcr, scn, itr,"/TAC",(tstep+1),".RData", sep = "")
    saveRDS(TAC_mat, file_tac)
    
    #Extract information from EM relative to performance metrics and EM model assessment
    if (lag==0) {
      if (tstep==1){
        ryr=1
      }else{
        ryr=2
      }
      B0dat[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$SSBzero[1]
      Ftgt_em [(asmt_t[tstep]-ryr):asmt_t[tstep]] = rep(TAC_mat$Ftarget,(ryr+1))
      Fhcr [(asmt_t[tstep]-ryr):asmt_t[tstep]] = rep(TAC_mat$Fmultiplier,(ryr+1))
      R_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$Recruits[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)]
      SPB_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$SSB[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)]
      D_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$Deplete[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)]
      SPR_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$SPR[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)]
      #ensure headings are compatible
      names(SPRmat)[16]="Bio_Smry.1"
      Btot_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$Bio_Smry.1[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)] #Bio_Smry
      C_em[(asmt_t[tstep]-ryr):asmt_t[tstep]] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-(ryr+1)):(dim(SPRmat)[1]-1)]
      
    } else {
      if (tstep==1){
        B0dat[(asmt_t[tstep]-1)] = SPRmat$SSBzero[1]
        Ftgt_em [(asmt_t[tstep]-1)] = TAC_mat$Ftarget
        Fhcr [(asmt_t[tstep]-1)] = TAC_mat$Fmultiplier
        R_em[(asmt_t[tstep]-1)] = SPRmat$Recruits[(dim(SPRmat)[1]-1)]
        SPB_em[(asmt_t[tstep]-1)] = SPRmat$SSB[(dim(SPRmat)[1]-1)]
        D_em[(asmt_t[tstep]-1)] = SPRmat$Deplete[(dim(SPRmat)[1]-1)]
        SPR_em[(asmt_t[tstep]-1)] = SPRmat$SPR[(dim(SPRmat)[1]-1)]
        #ensure headings are compatible
        names(SPRmat)[16]="Bio_Smry.1"
        Btot_em[(asmt_t[tstep]-1)] = SPRmat$Bio_Smry.1[(dim(SPRmat)[1]-1)] #Bio_Smry
        C_em[(asmt_t[tstep]-1)] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-1)]
        
      } else {
        B0dat[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SSBzero[1]
        Ftgt_em [(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = rep(TAC_mat$Ftarget,tasmt)
        Fhcr [(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = rep(TAC_mat$Fmultiplier,tasmt)
        R_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Recruits[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        SPB_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SSB[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        D_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Deplete[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        SPR_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SPR[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        #ensure headings are compatible
        names(SPRmat)[16]="Bio_Smry.1"
        Btot_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Bio_Smry.1[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)] #Bio_Smry
        C_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
      }
    }
        
    #save the catch limit for this time step as the first two seasons will be used for the next
    #CHANGE FOR TSTEP STEP 1 to HAVE ciom25 as the new_cdat2 AND TEST FOR STEP2
    if (tstep==1){
      new_cdat2 = ciom25
    } else {
      new_cdat2 = new_cdat
    }
    
  } else {
    
    #***************************************************************************
    #Step 2: Estimate data with error and no error as input to EM and OM via SS Bootstrap routine
    OMBoot_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, new_cdat2, rec_devs, sdev, yfor)
    
    #****************************************************************************
    #Step 3: Generate files for operating model
    OM_fun_tvry_adj(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt)
    #create another OM that uses lagged data 
    if (lag>0){
      OM_fun_tvry_adj_lag(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, rec_devs,sdev, lag=lag,yfor=yfor)
    }    
    #*****************************************************************************************
    #Step 4: Run the estimation model , can choose to run with (datatype =3) or without observation error (datatype=2)
    EM_fun_adj(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, datatype=obse,lag=lag, aspm=aspm,yfor=yfor,is_qcreep=is_qcreep)
    
    #****************************************************************************
    #Step 5: Compute TAC using EM model output
    
    #read EM output file
    out_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/", sep = "")
    em_out = SS_output(out_dir, covar = FALSE, ncols = 250)
    ben_file_in = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/EM/Forecast-report.SSO", sep = "")
    ben = readLines(ben_file_in, warn = FALSE)
    
    yr_end = em_out$endyr
    
    #In this example we are using candidate HCR1a from the NC15 Annex F
    #Compute the SSB associated with the threshold and limit biomass reference points 
    ssb_thr = brp_fun_pbf(ssoutput=em_out, fraction=Bthr)
    ssb_lim = brp_fun_pbf(ssoutput=em_out, fraction=Blim)
    
    #Extract SPR series data
    SPRmat = em_out$sprseries
    
    #Generate TAC based on current harvest control rule
    #specify years over which to compute biology (yrb) and exploitation pattern (yrf)
    TAC_mat = HCR1a_pbf_byfleet_f_25for(ssout=em_out, dat = SPRmat, forf=ben, yr=yr_end, SSBtrs=ssb_thr, SSBlim=ssb_lim, err=1, Fmin=Fmin,hs=hs,hcr=hcr,scn=scn,itr=itr,tstep=tstep,yrb=c(2002:2004),yrf=yfor,tacl=tacl,is_hi_discard=is_hi_discard, TACdt=TACdt,TACEdt=TACEdt,TACWldt=TACWldt,TACWsdt=TACWsdt, TACmat=TACmat)
    
    #Save the TAC
    file_tac = paste(pdir, hs, hcr, scn, itr,"/TAC",(tstep+1),".RData", sep = "")
    saveRDS(TAC_mat, file_tac)
    
      if (tstep==1){
        B0dat[(asmt_t[tstep]-1)] = SPRmat$SSBzero[1]
        Ftgt_em [(asmt_t[tstep]-1)] = TAC_mat$Ftarget
        Fhcr [(asmt_t[tstep]-1)] = TAC_mat$Fmultiplier
        R_em[(asmt_t[tstep]-1)] = SPRmat$Recruits[(dim(SPRmat)[1]-1)]
        SPB_em[(asmt_t[tstep]-1)] = SPRmat$SSB[(dim(SPRmat)[1]-1)]
        D_em[(asmt_t[tstep]-1)] = SPRmat$Deplete[(dim(SPRmat)[1]-1)]
        SPR_em[(asmt_t[tstep]-1)] = SPRmat$SPR[(dim(SPRmat)[1]-1)]
        #ensure headings are compatible
        names(SPRmat)[16]="Bio_Smry.1"
        Btot_em[(asmt_t[tstep]-1)] = SPRmat$Bio_Smry.1[(dim(SPRmat)[1]-1)] #Bio_Smry
        C_em[(asmt_t[tstep]-1)] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-1)]
        
      } else {
        B0dat[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SSBzero[1]
        Ftgt_em [(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = rep(TAC_mat$Ftarget,tasmt)
        Fhcr [(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = rep(TAC_mat$Fmultiplier,tasmt)
        R_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Recruits[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        SPB_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SSB[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        D_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Deplete[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        SPR_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$SPR[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
        #ensure headings are compatible
        names(SPRmat)[16]="Bio_Smry.1"
        Btot_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Bio_Smry.1[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)] #Bio_Smry
        C_em[(asmt_t[tstep]-tasmt):(asmt_t[tstep]-1)] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-tasmt):(dim(SPRmat)[1]-1)]
      }
    
    #save the catch limit for this time step as the first two seasons will be used for the next
    if (tstep==1){
      new_cdat2 = ciom25
    } else {
      new_cdat2 = new_cdat
    }
    
  }
  
}

#****************************************************************************
#Step 6: Run the OM on the last time step with the data with no error
#Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/OM/", sep="")
#filename_om  <-paste(Path,"ssnohess.bat",sep="")
#batchtext_om = paste(pwin,"SS_model\\ss -maxfn 0 -phase 50 -nohess",sep="")
#writeLines(batchtext_om,filename_om)

#setwd(Path)
#command_run_om="ssnohess.bat"
#shell(cmd= command_run_om)

#****************************************************************************
#Step 7: Extract from the OM data relevant to calculation of performance metrics
out_dir = paste(pdir, hs, hcr, scn, itr,"/",tstep, "/OM/", sep="")
true_out = SS_output(out_dir, covar = FALSE, ncols = 250)

#Extract spr series quantities from the SS output
spr.om = true_out$sprseries

#start of simulation
st=length(1983:2022)+1
#extract from spr series quantities relevant to performance metrics
Rdat[1:((tasmt*tstep)-1)] = spr.om$Recruits[st:(length(spr.om$Recruits)-1)]
SPBdat[1:((tasmt*tstep)-1)] = spr.om$SSB[st:(length(spr.om$SSB)-1)]
Ddat[1:((tasmt*tstep)-1)] = spr.om$Deplete[st:(length(spr.om$Deplete)-1)]
SPRdat[1:((tasmt*tstep)-1)] = spr.om$SPR[st:(length(spr.om$SPR)-1)]
#ensure headings are compatible
names(spr.om)[16]="Bio_Smry.1"
Btot[1:((tasmt*tstep)-1)] = spr.om$Bio_Smry.1[st:(length(spr.om$SPR)-1)] #
Tdat[1:((tasmt*tstep)-1)] = spr.om$Retain_Catch[st:(length(spr.om$SPR)-1)]
Ftgt_om [1:((tasmt*tstep))] = rep(true_out$derived_quants$Value[which(true_out$derived_quants$Label == "annF_SPR")],(tasmt*tstep))
B0dat_om [1:((tasmt*tstep))] = rep(spr.om$SSBzero[1],(tasmt*tstep))
#extract the catch by fleet and season and year 
catch = true_out$catch

#extract the terminal year
yr_end = true_out$endyr

#select the catch from the simulation period (2023 onwards) and remove column with duplicate names
catch_sim = catch %>% filter(Yr> 2022) 

#sum catch across seasons
catch_sum = as.data.frame(catch_sim %>% group_by(Yr,Fleet) %>% summarise(ctot=sum(sel_bio)))

#set to wide format to match other data, so that year is in rows and column is the catch by fishery
Cdat = dcast(catch_sum, Yr ~ Fleet, value.var="ctot")

#Combine all output into a list
outmat1 = data.frame(Year = 2023:((length(asmt_t)*3)+2021), R = Rdat[1:23], Rem = R_em[1:23], SSB = SPBdat[1:23], SSBem = SPB_em[1:23], Depletion = Ddat[1:23], Dem = D_em[1:23], SPR = SPRdat[1:23], SPRem = SPR_em[1:23], Catch = Tdat[1:23], Cem = C_em[1:23], TAC = TACdt[1:23], TACws = TACWsdt[1:23],TACwl = TACWldt[1:23], TACepo = TACEdt[1:23], B0em = B0dat[1:23], B0om = B0dat_om[1:23], Bsmry= Btot[1:23], Bsmryem= Btot_em[1:23], Ftgtom = Ftgt_om[1:23], Ftgtem = Ftgt_em[1:23],fhcr = Fhcr[1:23])
outmat=cbind(outmat1, Cdat[,2:27])
outlist= list(outmat=outmat)

#save output to file
write.table(outlist, paste(pdir,hs, hcr,scn, itr,"/outlist.txt", sep =""))

#extract the catch at age
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

return(outmat)
}