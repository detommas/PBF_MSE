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
#' @param new_cdat new catch data obtained from TAC
#' @param rec_devsn new recruitment deviations
#' @param lag lag between data and assessment availbility and management
#' 
#' @author Desiree Tommasi

OM_fun_tvry_adj_lag <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs,lag){
 
#*****************************CREATE DAT FILE*******************************************   
  #The OM catch data corresponds to the bootstrap data with no error
  
  #create directory for the operating model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir OMlag"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #extract the data with no error from the bootstrap run
  boot_dat=SS_readdat(file = "data.ss_new", section=2)
  
  #extract end year
  endYear = boot_dat$endyr - lag
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the boot.dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
    
	  boot_new$endyr=endYear
    boot_new$catch=boot_old$catch[which(boot_old$catch$year<=boot_new$endyr),]
    boot_new$CPUE=boot_old$CPUE[which(boot_old$CPUE$year<=boot_new$endyr),]
    for (j in 1:22){
      sdat=boot_old$sizefreq_data_list[[j]]
      boot_new$sizefreq_data_list[[j]]=sdat %>% filter(Yr<=boot_new$endyr)
      boot_new$Nobs_per_method[j]=dim(boot_new$sizefreq_data_list[[j]])[1]
      #need to change the effective sample size of the new bootstrap data back to original
      #according to Lee's in ISC21/PBFWG-1/07
      boot_new$sizefreq_data_list[[j]]$Nsamp=boot_new$sizefreq_data_list[[j]]$Nsamp/10
      }
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/OMlag/OMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file)
    boot_new = boot_old
    #Extract the new catch data
    newCatchDat = boot_dat$catch %>% filter (year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
    
    #add the new catch data
    boot_new$catch = rbind(boot_old$catch, newCatchDat)
    
    #There are three survey indices the Japan LL index (21), the Jpn Troll (24), and the TWLL one (25). 
    #Extract the new bootstrap
    new_cpue21 = boot_dat$CPUE %>% filter(index==21&year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
    new_cpue24 = boot_dat$CPUE %>% filter(index==24&year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
    new_cpue25 = boot_dat$CPUE %>% filter(index==25&year %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
    
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
      sf_add = sf_dat[[j]] %>% filter (Yr %in% c((boot_dat$endyr-tasmt-lag+1):endYear))
      sf_new[[j]]=rbind(sf_old[[j]],sf_add)
    }
    
    boot_new$sizefreq_data_list = sf_new
    
    #modify end year
    boot_new$endyr = boot_dat$endyr -lag
    
    #change the number of size frequency observations
     for (j in c(1:6,12,14,15,17,18,20,21)){
      boot_new$Nobs_per_method[j]=dim(boot_new$sizefreq_data_list[[j]])[1]
    }
  }

  #need to change the added constant back to original
  #according to Lee's in ISC21/PBFWG-1/07
  boot_new$mincomp_per_method[10]=0.01
  boot_new$mincomp_per_method[11]=0.01
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OMlag/OMdat.ss",sep="")
  SS_writedat(boot_new, path_dat)
  
  #*****************************MODIFY FORECAST FILE****************
  if (tstep == 1){
    file_in = paste(pdir, hs, hcr, "forecast.ss", sep="")
    #read in forecast file from previous assessment period
    om_for = SS_readforecast(file_in)
    #create a new forecast file based on the old to be modified
    om_for_new = om_for
    #change end years of the benchmark years - only for recruitment
    om_for_new$Bmark_years[c(8,10)] = c((om_for$Bmark_years[8]+(tasmt-1)),
                                        (om_for$Bmark_years[10]+(tasmt-1)))
    om_for_new$Fcast_years[6] = om_for$Fcast_years[6]+(tasmt-1)
    #change first year for caps and allocations
    om_for_new$FirstYear_for_caps_and_allocations = om_for$FirstYear_for_caps_and_allocations+(tasmt-1)
    #change Rebuilder first chatch
    om_for_new$Ydecl=om_for$Ydecl + (tasmt-1)
    #change Rebuilder of current age structure
    om_for_new$Yinit=om_for$Yinit + (tasmt-1)
  } else {
    file_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep),"/Boot/forecast.ss",sep="")
    #read in forecast file from previous assessment period
    om_for = SS_readforecast(file_in)
    #create a new forecast file based on the old to be modified
    om_for_new = om_for
    #change end years of the benchmark years - only for recruitment
    om_for_new$Bmark_years[c(8,10)] = c((om_for$Bmark_years[8]-1),
                                        (om_for$Bmark_years[10]-1))
    om_for_new$Fcast_years[6] = om_for$Fcast_years[6]-1
    #change first year for caps and allocations
    om_for_new$FirstYear_for_caps_and_allocations = om_for$FirstYear_for_caps_and_allocations-1
    #change Rebuilder first chatch
    om_for_new$Ydecl=om_for$Ydecl -1
    #change Rebuilder of current age structure
    om_for_new$Yinit=om_for$Yinit -1
  }
  
  file_out= paste(pdir, hs, hcr, scn, itr,"/", tstep,"/OMlag",sep="")
  #Write a stock synthesis data file from the mse_dat list object
  SS_writeforecast(om_for_new, file_out)
  
  #*****************************CHANGE PAR FILE**********************************************
  #include recruitment and selectivity deviations for future time steps
  
  #Specify inputs to change_rec_devs function
  if (tstep == 1){
    pfile_in = paste(sdir,scn, "ss.PAR",sep="")
  } else {
    pfile_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/ss.PAR", sep = "")
  }
  
  pfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OMlag/ss.PAR", sep="")
  
  #Modify par file to include the recruitment deviations for the next assessment cycle
  rec_devsn = rec_devs[asmt_t[tstep]:((asmt_t[tstep])+(tasmt-2))]
  change_rec_devs(recdevs_new = rec_devsn, par_file_in = pfile_in, par_file_out = pfile_out)

  #***************************CHANGE CTL FILE*************************************
  #Modify end of blocks, end of main recruitment deviations, and set rec devs as fixed in control file
  blk_in = paste(sdir, scn, "control_simple_1719_2021.ss", sep = "")
  blk_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OMlag/Boot.ctl", sep="")
  
  blk_end = 2020 + asmt_t[tstep] + (tasmt-2)
  
  #change control file
  change_ctl(blk_in, blk_out, blk_end, vadj = 2)

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
  path_start = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OMlag/",sep="")
  SS_writestarter(starter_dat, path_start)
  
#*************************RUN THE OM MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/OMlag/", sep="")
  filename_om  <-paste(Path,"ssnohess.bat",sep="")
  batchtext_om = paste(pwin,"SS_model\\ss -maxfn 0 -phase 50 -nohess",sep="")
  writeLines(batchtext_om,filename_om)
  
  setwd(Path)
  command_run_om="ssnohess.bat"
  shell(cmd= command_run_om)
  
}
  
  