#' Prepares the needed files to run the PBF OM model in SS 
#' 
#' @param pdir the parent directory path 
#' @param sdir path to directory of OM (initial)
#' @param hs the harvest strategy being run
#' @param hcr the hcr being run
#' @param scn the scenario being run
#' @param hsw the harvest strategy being run in Windows notatation
#' @param hcrw the hcr being run in Windows notation
#' @param scnw the scenario being run in Windows notation
#' @param pwin he parent directory path in Windows notation
#' @param itr iteration number
#' @param tstep time step of the OM
#' @param tasmt frequency of assessments
#' 
#' @author Desiree Tommasi

OM_fun_tvry_adj <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt){
 
#*****************************CREATE DAT FILE*******************************************   
  #The OM catch data corresponds to the bootstrap data with no error
  
  #create directory for the operating model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir OM"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #extract the data with no error from the bootstrap run
  boot_dat=SS_readdat(file = "data_expval.ss")
  
  #extract end year
  endYear = boot_dat$endyr
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the boot.dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
    #need to change the effective sample size of the new bootstrap data back to original
    #according to Lee's in ISC21/PBFWG-1/07
    for (j in c(1:23)){
      boot_new$sizefreq_data_list[[j]]$Nsamp=boot_old$sizefreq_data_list[[j]]$Nsamp/10
    }
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/OM/OMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file)
    boot_new = boot_old
    #Extract the new catch data
    newCatchDat = boot_dat$catch %>% filter (year %in% c((endYear-tasmt+1):endYear))
    
    #add the new catch data
    boot_new$catch = rbind(boot_old$catch, newCatchDat)
    
    #There is one survey index the the TWLL one (31). 
    #Extract the new bootstrap
    new_cpue31 = boot_dat$CPUE %>% filter(index==31&year %in% c((endYear-tasmt+1):endYear))

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
      #ensure headings compatible
      names(sf_dat[[j]])[2]="Yr"
      names(sf_old[[j]])[2]="Yr"
      names(sf_new[[j]])[2]="Yr"
      
      sf_add = sf_dat[[j]] %>% filter (Yr %in% c((endYear-tasmt+1):endYear))
      sf_new[[j]]=rbind(sf_old[[j]],sf_add)
    }
    
    boot_new$sizefreq_data_list = sf_new
    
    #modify start year
    boot_new$endyr = boot_dat$endyr
    
    #change the number of size frequency observations
    boot_new$Nobs_per_method = boot_dat$Nobs_per_method
  }

  #need to change the added constant back to original
  #according to Lee's in ISC21/PBFWG-1/07
  boot_new$mincomp_per_method[10]=0.01
  boot_new$mincomp_per_method[11]=0.01
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/OMdat.ss",sep="")
  SS_writedat(boot_new, path_dat)
  
#*****************************COPY PAR, CTL, and FORECAST FILES FROM OM BOOT FOLDER**********************************************
  #Move to the Bootstrap model directory 
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot/", sep=""))

  command_mv = paste("for %I in (forecast.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\OM\\", sep ="")
  shell(cmd = command_mv)
  command_mv = paste("for %I in (ss.PAR) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\OM\\", sep ="")
  shell(cmd = command_mv)
  command_mv = paste("for %I in (Boot.ctl) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\OM\\", sep ="")
  shell(cmd = command_mv)
  
#*************************CHANGE STARTER FILE****************************************
  #Modify starter file to run OM with new catch data without estimating parameters 
  
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss", sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "OMdat.ss"
  
  #specify the new dat file name
  starter_dat$ctlfile = "Boot.ctl"
  
  #specify to use the ss3.par as parameters
  starter_dat$init_values_src = 1
  
  #turn off estimation of parameters 
  starter_dat$last_estimation_phase = 0
  
  #change last year with uncertainty to end year
  starter_dat$maxyr_sdreport= endYear
  
  #write new starter file
  path_start = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/",sep="")
  SS_writestarter(starter_dat, path_start)
  
#*************************RUN THE OM MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/OM/", sep="")
  filename_om  <-paste(Path,"ssnohess.bat",sep="")
  batchtext_om = paste(pwin,"SS_model\\ss -maxfn 0 -phase 50 -nohess",sep="")
  writeLines(batchtext_om,filename_om)
  
  setwd(Path)
  command_run_om="ssnohess.bat"
  shell(cmd= command_run_om)
  
}
  
  