#' Prepares the needed files to run the PBF EM model in SS and run the model
#' 
#' @param pdir the parent directory path
#' @param sdir the path to directory of OM (initial)
#' @param hs the harvest strategy being run
#' @param hcr the hcr being run
#' @param scn the scenario being run
#' @param hsw the harvest strategy being run in Windows notation
#' @param hcrw the hcr being run in Windows notation
#' @param scnw the scenario being run in Windows notation
#' @param pwin he parent directory path in Windows notation
#' @param itr iteration number
#' @param tstep time step of the OM
#' @param tasmt frequency of assessments
#' @param datatype specifies section of new datafile from which to extract,  2 = perfect data, 3 = data with error
#' @param lag lag between data and assessment availbility and management 
#' @param aspm aspm switch: = NULL (no ASPM, default),
#'                            "ASPMR-f1f3f6f21" (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth & F21EPO_COMM(2002-) & F6JPN_TPS_SOJ),
#'                            "ASPMR-f1f3f21"   (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth & F21EPO_COMM(2002-)),
#'                            "ASPMR-f1f3"      (ASPM-R w/ size & selectivities for F1JPN_LL(S4) & F3TWN_LLSouth),
#'                            "ASPMR-f3"        (ASPM-R w/ size & selectivities for F3TWN_LLSouth only)
#' @param yfor years over which to compute the sel and relF for the forecast file
#' 
#' @author Desiree Tommasi and Norio Takahashi

EM_fun_adj <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, datatype, lag, aspm, yfor){
 
#*****************************CHANGE DAT FILE*******************************************   
  # Enter new catch data given the TAC into the PAR file
  
  #create directory for the estimation model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir EM"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #read the new .dat file with error from the bootstrap run
  if (datatype==2){
    boot_dat=SS_readdat(file = "data_expval.ss")
  } else {
    boot_dat=SS_readdat(file = "data_boot_001.ss")
  }
  
  #extract end year
  endYear = boot_dat$endyr - lag
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the boot.dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
    
    boot_new$endyr=endYear
    boot_new$catch=boot_old$catch[which(boot_old$catch$year<=boot_new$endyr),]
    boot_new$CPUE=boot_old$CPUE[which(boot_old$CPUE$year<=boot_new$endyr),]
    for (j in c(1:23)){
      sdat=boot_old$sizefreq_data_list[[j]]
      boot_new$sizefreq_data_list[[j]]=sdat %>% filter(Yr<=boot_new$endyr)
      boot_new$Nobs_per_method[j]=dim(boot_new$sizefreq_data_list[[j]])[1]
      #need to change the effective sample size of the new bootstrap data back to original
      #according to Lee's in ISC21/PBFWG-1/07
      boot_new$sizefreq_data_list[[j]]$Nsamp=boot_new$sizefreq_data_list[[j]]$Nsamp/10
    }
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/EM/EMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file)
    boot_new = boot_old
    #Extract the new catch data
    newCatchDat = boot_dat$catch %>% filter (year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
    
    #add the new catch data
    boot_new$catch = rbind(boot_old$catch, newCatchDat)
    
    #There are three survey indices the Japan LL index (21), the Jpn Troll (24), and the TWLL one (25). 
    #Extract the new bootstrap
    new_cpue31 = boot_dat$CPUE %>% filter(index==31&year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))

    #add the new CPUE data
    boot_new$CPUE = rbind(boot_old$CPUE, new_cpue31)
    
    #extract the new size frequency data
    sf_dat = boot_dat$sizefreq_data_list
    sf_old = boot_old$sizefreq_data_list
    
    #need to change the effective sample size of the new bootstrap data back to original
    #according to Lee's in ISC21/PBFWG-1/07
    for (j in 1:23){
      sf_dat[[j]]$Nsamp=boot_dat$sizefreq_data_list[[j]]$Nsamp/10
    }
    
    sf_new = sf_old
    for (j in c(1:13,18,21,22)){
      sf_add = sf_dat[[j]] %>% filter (Yr %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
      sf_new[[j]]=rbind(sf_old[[j]],sf_add)
    }
    
    boot_new$sizefreq_data_list = sf_new
    
    #modify end year
    boot_new$endyr = boot_dat$endyr -lag
    
    #change the number of size frequency observations
    for (j in c(1:13,18,21,22)){
      boot_new$Nobs_per_method[j]=dim(boot_new$sizefreq_data_list[[j]])[1]
    }
  }
  
  #need to change the added constant back to original
  #according to Lee's in ISC21/PBFWG-1/07
  boot_new$mincomp_per_method[10]=0.01
  boot_new$mincomp_per_method[11]=0.01
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/EM/EMdat.ss",sep="")
  SS_writedat(boot_new, path_dat)
  
  #***************************MODIFY FORECAST FILE*************************************
  if (tstep == 1){
    file_in = paste(pdir, hs, hcr, "forecast.ss", sep="")
    #read in forecast file from previous assessment period
    em_for = SS_readforecast(file_in)
    #create a new forecast file based on the old to be modified
    em_for_new = em_for
    #change end years of the benchmark years - only for recruitment
    em_for_new$Bmark_years[c(3,4,5,6,8,10)] = c(yfor[1], yfor[length(yfor)], yfor[1], yfor[length(yfor)],endYear,endYear)
    em_for_new$Fcast_years[c(1,2,3,4,6)] = c(yfor[1], yfor[length(yfor)], yfor[1], yfor[length(yfor)],endYear)
    #change first year for caps and allocations
    em_for_new$FirstYear_for_caps_and_allocations = endYear
    #change Rebuilder first catch
    em_for_new$Ydecl= endYear
    #change Rebuilder of current age structure
    em_for_new$Yinit= endYear
  } else {
    file_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep),"/Boot/forecast.ss",sep="")
    #read in forecast file from previous assessment period
    em_for = SS_readforecast(file_in)
    #create a new forecast file based on the old to be modified
    em_for_new = em_for
    #change end years of the benchmark years - only for recruitment
    em_for_new$Bmark_years[c(8,10)] = c((em_for$Bmark_years[8]-1),
                                        (em_for$Bmark_years[10]-1))
    em_for_new$Fcast_years[6] = em_for$Fcast_years[6]-1
    #change first year for caps and allocations
    em_for_new$FirstYear_for_caps_and_allocations = em_for$FirstYear_for_caps_and_allocations-1
    #change Rebuilder first catch
    em_for_new$Ydecl=em_for$Ydecl -1
    #change Rebuilder of current age structure
    em_for_new$Yinit=em_for$Yinit -1
  }
  
  file_out= paste(pdir, hs, hcr, scn, itr,"/", tstep,"/EM",sep="")
  #Write a stock synthesis data file from the mse_dat list object
  SS_writeforecast(em_for_new, file_out)
  
#***************************CHANGE CTL FILE*************************************
  if(is.null(aspm)){ # not use ASPM option
    
    ctl_in = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest.ss", sep = "")
    
  } else { # use ASPM option
    
    aspm <- str_to_lower(aspm)
    
    ctl_in <- switch(aspm,
                     # w/o size
                     #"aspm"  = paste(sdir, scn, "SAM/control_simple_1719_2021_ASPM.ss", sep = ""),
                     #"aspmr" = paste(sdir, scn, "SAM/control_simple_1719_2021_ASPMR.ss", sep = ""),
                     # w/ size
                     #"aspm-size"  = paste(sdir, scn, "SAM/control_simple_1719_2021_ASPM_size.ss", sep = ""),
                     #"aspmr-size" = paste(sdir, scn, "SAM/control_simple_1719_2021_ASPMR_size.ss", sep = ""),
                     #"aspmr-sizef1f12" = paste(sdir, scn, "SAM/control_simple_1719_2021_ASPMR_sizeF1F12.ss", sep = ""),
                     "aspmr-f1f3f6f21" = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest_ASPMR_F1F3f6F21.ss", sep = ""),
                     "aspmr-f1f3f21"   = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest_ASPMR_F1F3F21.ss"  , sep = ""),
                     "aspmr-f1f3"      = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest_ASPMR_F1F3.ss"     , sep = ""),
                     "aspmr-f3"        = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest_ASPMR_F3.ss"       , sep = "")
    )
    
    if(is.null(ctl_in)){
      ctl_in = paste(sdir, scn, "SAM/ctl_PBF_2024_0309_BC_qest_ASPMR_F1F3.ss", sep = "")
      message("aspm argument is not correct, set to ASPMR-F1F3 anyway to continue....")
    }
    
  }
  
  ctl_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/EM.ctl", sep="")
  
  blk_end = 2020 + asmt_t[tstep] + (tasmt-2)
  
  #change control file
  change_ctl(ctl_in, ctl_out, blk_end, vadj = 1)
  
#*************************CHANGE STARTER FILE****************************************
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss",sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "EMdat.ss"
  
  #specify the new ctl file name
  starter_dat$ctlfile = "EM.ctl"
  
  #specify to not use the ss3.par as initial parameter values
  starter_dat$init_values_src = 0
  
  #change last year with uncertainty to end year
  starter_dat$maxyr_sdreport= blk_end
  
  #turn off estimation of parameters 
  #starter_dat$last_estimation_phase = 0
  
  #write new starter file
  dir_start = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM/", sep = "")
  SS_writestarter(starter_dat, dir_start)
  
#*************************RUN THE EM MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM/", sep = "")
  filename_em  <-paste(Path,"runem.bat",sep="")
  batchtext_em = paste(pwin,"SS_model\\ss -nohess -cbs 500000000", sep="")
  writeLines(batchtext_em,filename_em)
  
  setwd(Path)
  command_run_em="runem.bat"
  shell(cmd= command_run_em)
  
  
}
  
  