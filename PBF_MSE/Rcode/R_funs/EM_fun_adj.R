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
#' @param datatype specifies section of new datafile from which to extract,  2 = perfect data, 3 = data with error 
#' 
#' @author Desiree Tommasi

EM_fun_adj <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, datatype){
 
#*****************************CHANGE DAT FILE*******************************************   
  # Enter new catch data given the TAC into the PAR file
  
  #create directory for the operating model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir EM"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #read the new .dat file with error from the bootstrap run
  boot_dat=SS_readdat(file = "data.ss_new", section=datatype)
  
  #extract end year
  endYear = boot_dat$endyr
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the boot.dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
    #need to change the effective sample size of the new bootstrap data back to original
    #according to Lee's in ISC21/PBFWG-1/07
    for (j in c(1:22)){
    boot_new$sizefreq_data_list[[j]]$Nsamp=boot_old$sizefreq_data_list[[j]]$Nsamp/10
    }
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/EM/EMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file)
    boot_new = boot_old
    #Extract the new catch data
    newCatchDat = boot_dat$catch %>% filter (year %in% c((endYear-tasmt+1):endYear))
    
    #add the new catch data
    boot_new$catch = rbind(boot_old$catch, newCatchDat)
    
    #There are three survey indices the Japan LL index (21), the Jpn Troll (24), and the TWLL one (25). 
    #Extract the new bootstrap
    new_cpue21 = boot_dat$CPUE %>% filter(index==21&year %in% c((endYear-tasmt+1):endYear))
    new_cpue24 = boot_dat$CPUE %>% filter(index==24&year %in% c((endYear-tasmt+1):endYear))
    new_cpue25 = boot_dat$CPUE %>% filter(index==25&year %in% c((endYear-tasmt+1):endYear))
    
    #add the new CPUE data
    boot_new$CPUE = rbind(boot_old$CPUE, new_cpue21, new_cpue24, new_cpue25)
    
    #extract the new size frequency data
    sf_dat = boot_dat$sizefreq_data_list
    sf_old = boot_old$sizefreq_data_list
    
    #need to change the effective sample size of the new bootstrap data back to original
    #according to Lee's in ISC21/PBFWG-1/07
    for (j in 1:22){
      sf_dat[[j]]$Nsamp=boot_dat$sizefreq_data_list[[j]]$Nsamp/10
    }
    
    sf_new = sf_old
    for (j in c(1:6,12,14,15,17,18,20,21)){
      sf_add = sf_dat[[j]] %>% filter (Yr %in% c((endYear-tasmt+1):endYear))
      sf_new[[j]]=rbind(sf_old[[j]],sf_add)
    }
    
    boot_new$sizefreq_data_list = sf_new
    
    #modify end year
    boot_new$endyr = boot_dat$endyr
    
    #change the number of size frequency observations
    boot_new$Nobs_per_method = boot_dat$Nobs_per_method
  }
  
  #need to change the added constant back to original
  #according to Lee's in ISC21/PBFWG-1/07
  boot_new$mincomp_per_method[10]=0.01
  boot_new$mincomp_per_method[11]=0.01
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/EM/EMdat.ss",sep="")
  SS_writedat(boot_new, path_dat)
  
  #***************************COPY FORECAST FILE*************************************
  #Move to the hcr directory 
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot/", sep=""))
  
  command_mv = paste("for %I in (forecast.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM\\", sep ="")
  shell(cmd = command_mv)
  
#***************************CHANGE CTL FILE*************************************
  ctl_in = paste(sdir, scn, "SAM/control_simple_1719_2021.ss", sep = "")
  ctl_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/EM.ctl", sep="")
  
  blk_end = 2020 + asmt_t[tstep] + (tasmt-1)
  
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
  batchtext_em = paste(pwin,"SS_model\\ss -nohess", sep="")
  writeLines(batchtext_em,filename_em)
  
  setwd(Path)
  command_run_em="runem.bat"
  shell(cmd= command_run_em)
  
  
}
  
  
