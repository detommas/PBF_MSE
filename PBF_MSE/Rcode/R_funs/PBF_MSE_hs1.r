#' Runs the PBF MSE framework for the specified number of iterations
#'
#' Note that this was created to be run using the wrapper code PBF_MSE_prll.R or the PBF_MSE_itr1.R
#' @param hsnum number characterizing the harvest strategy being run
#' @param hcrnum number characterizing the harvest control rule being run
#' @param scnnum number characterizing the uncertainty scenario being run
#' @param itr the iteration that one wants to run
#' @param Bthr the fraction of unifished biomass the threshold reference point refers to
#' @param Blim the fraction of unfished biomass the limit reference point refers to
#' Note that the F target is specified in the forecast file. The fishing intensity is measured as the spawning potential ratio (SPR)
#' @return a data frame of output for performance statistics
#' @author D.Tommasi

PBF_MSE_hs1 = function(hsnum,hcrnum,scnnum,itr, Bthr, Blim) { 

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
pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/"
pwin = "C:\\Users\\desiree.tommasi\\Documents\\Bluefin\\PBF_MSE\\"

#Specify the path of conditioned initial OM
sdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/Condition/"

#Specify vectors where to save output (output is from OM unless otherwise specified) for the future simulation years
Rdat = 1:30 # recruits
SPBdat = 1:30 # spawning biomass
Btot = 1:30 # summary biomass as specified in om 
Tdat = 1:30 # total catch
TACdt = 1:30 # total TAC
TACWsdt = 1:30 # total TAC for WPO small fish
TACWldt = 1:30 # total TAC for WPO large fish
TACEdt = 1:30 #  total TAC for EPO
Ddat = 1:30 # depletion
SPRdat = 1:30 # fishing intensity (1-SPR)
B0dat = 1:30 # SSB0 from em
B0dat_om = 1:30 # SSB0 from om
Ftgt_om = 1:30 # F target (exploitation intensity leading to the SPR specified in the forcast file) from om
Ftgt_em = 1:30 # F target (exploitation intensity leading to the SPR specified in the forcast file) from em
R_em = 1:30 # recruits from em
SPB_em = 1:30 # spawning biomass from em
D_em = 1:30 # depletion from em
SPR_em = 1:30 # spr from em
Btot_em = 1:30 # summary biomass from em
C_em = 1:30 # total catch from em

#set working directory 
setwd(paste(pdir,hs, hcr, scn, sep = ""))

#create directory for each iteration (i.e. different recruitment deviations)
cmddir = paste("mkdir", itr)
shell(cmd = cmddir)

#**************************************************************************************
#Generate recruitment deviations

#set working directory to base iteration
setwd(paste(pdir, hs, hcr, scn, itr, sep = ""))

#Create a folder where to store deviations for each MSE iteration (i.e. 30 year simulation)
shell(cmd = "mkdir Rec_dev")

#generate random recruitment deviations for the 30 years of the simulation given the specified sigma (e.g. 0.6)
rec_devs = recdevs_mse(itr, 30, 0.6)

write.table(rec_devs, paste(pdir, hs, hcr, scn, itr,"/Rec_dev/rec_devs.txt", sep = ""))

#**************************************************************************************

for (tstep in 1:length(asmt_t)){
  
  #create directory for new time step where the new dat file will be saved
  setwd(paste(pdir,hs, hcr, scn, itr, sep = ""))
  cmddir = paste("mkdir", tstep)
  shell(cmd = cmddir)
  
  #*************************************************************************************
  #Step 1: Set the catch for the next two years (based on assessment frequency)
  #For the first two year we choose the catch limits of the first projection scenario of the 2022 update
  #for the remainder of the simulation the TAC will be set by the HCR
  
  #*************************************************************************************
  if (tstep == 1) {
    TACWs = 4475 #catch limit for small fish in western pacific
    TACWl= 7860 #catch limit for large fish in western pacific
    TACE= 3995 #catch limit for EPO
    TACdat = TACWs+TACWl+TACE
  } else {
    TAC_file = paste(pdir, hs, hcr, scn, itr,"/TAC",tstep,".RData", sep = "")
    TACmat = readRDS(TAC_file)
    TACWs = TACmat$TACWs$TAC #catch limit for small fish in western pacific
    TACWl= TACmat$TACWl$TAC #catch limit for large fish in western pacific
    TACE= TACmat$TACE$TAC #catch limit for EPO
    TACdat = TACWs+TACWl+TACE
  }
  
  #Record the TAC - set in the previous time step but effective for the current
  TACdt[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = rep(TACdat,tasmt)
  TACWsdt[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = rep(TACWs,tasmt)
  TACWldt[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = rep(TACWl,tasmt)
  TACEdt[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = rep(TACE,tasmt)
  
  #Generate catch by fleet and season using the Ws, Wl, epo catch ratios (cr_hcr) produced by the
  #catch_ratios.R code and the TAC
  #Note this is based on the 2017-2019 catch ratios
  new_cdatwl = cr_wl
  new_cdatwl$catch = cr_wl$cratio*TACWl
  
  new_cdatws = cr_ws
  new_cdatws$catch = cr_ws$cratio*TACWs
  
  new_cdate = cr_epo
  new_cdate$catch = cr_epo$cratio*TACE
  
  new_cdat = rbind(new_cdatwl,new_cdatws,new_cdate)
  
  #***************************************************************************
  #Step 2: Estimate data with error and no error as input to EM and OM via SS Bootstrap routine
  OMBoot_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs)
  
  #****************************************************************************
  #Step 3: Generate files for operating model
  OM_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs)
  
  #*****************************************************************************************
  #Step 3: Run the estimation model , can choose to run with (datatype =3) or without observation error (datatype=2)
  EM_fun(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, datatype=2)
  
  #****************************************************************************
  #Step 5: Compute TAC using EM model output
  
  #read EM output file
  out_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/", sep = "")
  em_out = SS_output(out_dir, covar = FALSE, ncols = 250)
  
  yr_end = em_out$endyr
  
  #In this example we are using candidate HCR1a from the NC15 Annex F
  #Compute the SSB associated with the threshold and limit biomass reference points 
  ssb_thr = brp_fun_pbf(ssoutput=em_out, fraction=Bthr)
  ssb_lim = brp_fun_pbf(ssoutput=em_out, fraction=Blim)
  
  #Extract SPR series data
  SPRmat = em_out$sprseries
  
  #Fstd/Fbtarget with Btarget at 0.3 is 1.23596e-01
  #Extract the fishing intensity (as exploitation rate) that results in the SPR target reference point (F target) specified in the forecast file
  em_ftgt = (em_out$derived_quants %>% filter(Label == "Fstd_SPR"))$Value
  
  #Generate TAC based on current harvest control rule
  TAC_mat = HCR1a_pbf_byfleet(dat = SPRmat, yr=yr_end, SSBtrs=ssb_thr, SSBlim=ssb_lim, Ftgt=em_ftgt, cr=cr_all, err=1)

  #Save the TAC
  file_tac = paste(pdir, hs, hcr, scn, itr,"/TAC",(tstep+1),".RData", sep = "")
  saveRDS(TAC_mat, file_tac)
  
  #Extract information from EM relative to performance metrics and EM model assessment
  B0dat[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$SSBzero[1]
  Ftgt_em [asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = rep(em_ftgt,tasmt)
  R_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$Recruits[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)]
  SPB_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$SSB[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)]
  D_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$Deplete[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)]
  SPR_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$SPR[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)]
  Btot_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$Bio_Smry[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)] #Bio_Smry
  C_em[asmt_t[tstep]:(asmt_t[tstep]+(tasmt-1))] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-(tasmt)):(dim(SPRmat)[1]-1)]
  
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
om_out = SS_output(out_dir, covar = FALSE, ncols = 250)

#Extract spr series quantities from the SS output
spr.om = om_out$sprseries

#start of simulation
st=length(1983:2020)+1
#extract from spr series quantities relevant to performance metrics
Rdat[1:(tasmt*tstep)] = spr.om$Recruits[st:(length(spr.om$Recruits)-1)]
SPBdat[1:(tasmt*tstep)] = spr.om$SSB[st:(length(spr.om$SSB)-1)]
Ddat[1:(tasmt*tstep)] = spr.om$Deplete[st:(length(spr.om$Deplete)-1)]
SPRdat[1:(tasmt*tstep)] = spr.om$SPR[st:(length(spr.om$SPR)-1)]
Btot[1:(tasmt*tstep)] = spr.om$Bio_Smry[st:(length(spr.om$SPR)-1)] #
Tdat[1:(tasmt*tstep)] = spr.om$Retain_Catch[st:(length(spr.om$SPR)-1)]
Ftgt_om [1:(tasmt*tstep)] = rep(om_out$derived_quants$Value[which(om_out$derived_quants$Label == "Fstd_SPR")],(tasmt*tstep))
B0dat_om [1:(tasmt*tstep)] = rep(spr.om$SSBzero,(tasmt*tstep))
#extract the catch by fleet and season and year 
catch = om_out$catch

#extract the terminal year
yr_end = om_out$endyr

#select the catch from the simulation period (2021 onwards) and remove column with duplicate names
catch_sim = catch %>% filter(Yr> 2020) 

#sum catch across seasons
catch_sum = as.data.frame(catch_sim %>% group_by(Yr,Fleet) %>% summarise(ctot=sum(sel_bio)))

#set to wide format to match other data, so that year is in rows and column is the catch by fishery
Cdat = dcast(catch_sum, Yr ~ Fleet, value.var="ctot")

#Combine all output into a list
outmat1 = data.frame(Year = 2016:2045, R = Rdat, Rem = R_em, SSB = SPBdat, SSBem = SPB_em, Depletion = Ddat, Dem = D_em, SPR = SPRdat, SPRem = SPR_em, Catch = Tdat, Cem = C_em, TAC = TACdt, TACws = TACWsdt,TACwl = TACWldt, TACepo = TACEdt, B0em = B0dat, B0om = B0dat_om, Bsmry= Btot, Bsmryem= Btot_em, Ftgtom = Ftgt_om, Ftgtem = Ftgt_em)
outmat=cbind(outmat1, Cdat[,2:26])
outlist= list(outmat=outmat)

#save output to file
write.table(outlist, paste(pdir,hs, hcr,scn, itr,"/outlist.txt", sep =""))

return(outmat)
}