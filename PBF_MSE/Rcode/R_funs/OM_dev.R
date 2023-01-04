# function set up the OM and run a loop to find the F corresponding to the catches

#' based on SSMSE code
#'
#' @param fleetnames A vector of fleet names, in the order they appear in the
#'  ss model.
#' @importFrom tidyr gather separate
#' @return a list containing: F_df, a long dataframe with F by Yr, Era, Seas,
#'  and fleet; F_rate, a data frame with F for the time frame of the model
#'  only by Yr, Seas, and Fleet, ordered as the ss.par file expects; init_F,
#'  a named vector of initial F values by Seaon and Fleet, ordered (and named)
#'  as SS expects; and F_rate_fcast, a dataframe of forecasted F by Yr, Seas,
#'  and fleet, ordered as SS would expect in F_rate.
#'  

OM_dev = function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs){
  
  #create directory for the Bootstrap routine
  setwd(paste(pdir, hs, hcr, scn, itr,"/",tstep, sep = ""))
  cmddir = "mkdir Boot"
  shell(cmd = cmddir)
  
  #*****************************CHANGE FORECAST FILE*******************************************   
  #Specify inputs to the change_for function
  if (tstep == 1){
    file_in = paste(pdir, hs, hcr, "forecast.ss", sep="")
  } else {
    file_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/forecast.ss",sep="")
  }
  
  file_out= paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot",sep="")
  
  change_for(ss_file_in = file_in, ss_file_out = file_out, t_asmt = tasmt)
  
  #*****************************CHANGE DAT FILE*******************************************   
  # Enter new catch data given the TAC and put dummy values for CPUE and size compositions
  
  #Specify inputs to the change_dat function
  if (tstep == 1){
    file_in = paste(sdir,scn,"dat_PBF_nonum.ss",sep="")
  } else {
    file_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/Bootdat.txt",sep="")# file in for catch, as it should be the true one up to the time of the assessment
  }
  
  file_out= paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot/Bootdat.txt",sep="")
  
  dat <- r4ss::SS_readdat(file.path(file_in),
                          verbose = FALSE,
                          section = 1
  )
  
  om_dat=dat
  
  #generate a vector of years for the catch data based on the end year of the previous data file and the assessment frequency
  ldat=length(new_cdat$catch)
  cyear=rep(1,ldat*tasmt)
  for (j in 1:tasmt){
    cyear[(j*ldat-ldat+1):(j*ldat)] = rep((om_dat$endyr+j),ldat)}
  
  #repeat catch data according to the assessment period
  cdat_new3 = data.frame(year=cyear,seas=rep(new_cdat$Seas,tasmt),fleet=rep(new_cdat$Fleet,tasmt),catch=rep(new_cdat$catch,tasmt),catch_se=rep(0.1,ldat*tasmt))
  
  catch = cdat_new3

  catch_new = rbind(dat$catch, cdat_new3)
  catch_new2 = catch_new[order(catch_new$fleet,catch_new$year,catch_new$seas),]
  dat$catch = catch_new2
  dat$endyr = om_dat$endyr + tasmt

#************************SET UP INPUTS*******************************************************
harvest_rate = NULL
catch_basis = NULL
F_limit = NULL
seed = NULL

#***************************CHANGE CTL FILE*************************************
#Modify end of blocks, end of main recruitment deviations, and set rec devs as fixed in control file
blk_in = paste(sdir, scn, "control_simple.ss", sep = "")
blk_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/Boot.ctl", sep="")

blk_end = 2020 + asmt_t[tstep] + (tasmt-1)

#change control file
change_ctl(blk_in, blk_out, blk_end, vadj = 2)

#*************************CHANGE STARTER FILE****************************************
#Modify starter file to run OM with new catch data without estimating parameters 

#read into R the ss starter file using the r4ss SS_readstarter function
start=SS_readstarter(paste(pdir, hs, "starter.ss", sep=""))

#specify the new dat file name
start$datfile = "Bootdat.txt"

#specify the new ctl file name
start$ctlfile = "Boot.ctl"

#specify to use the ss.par as parameters
start$init_values_src = 1

#turn off estimation of parameters 
start$last_estimation_phase = 0

#add 1 data bootstrap file
start$N_bootstraps = 3

#change last year with uncertainty to end year
start$maxyr_sdreport= blk_end

#write new starter file
path_start = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/Boot/",sep="")
SS_writestarter(start, path_start)

#*****************************CHANGE PAR FILE**********************************************
#include recruitment and selectivity deviations for future time steps

#Specify inputs to change_rec_devs_alb function
if (tstep == 1){
  pfile_in = paste(sdir,scn, "ss.PAR",sep="")
} else {
  pfile_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/ss.PAR", sep = "")
}

pfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/ss.PAR", sep="")

#Modify par file to include the recruitment deviations for the next assessment cycle
rec_devsn = rec_devs[asmt_t[tstep]:((asmt_t[tstep])+(tasmt-1))]
#change_rec_devs(recdevs_new = rec_devsn, par_file_in = pfile_in, par_file_out = pfile_out)

#*****************************************************************************************
# read in original control file
ctl <- SS_readctl(
  file = blk_out,
  version = "3.30", use_datlist = TRUE, datlist = dat,
  verbose = FALSE
)

# read in parameter file
parlist <- SS_readpar_3.30(
  parfile = pfile_in,
  datsource = om_dat, ctlsource = ctl,
  verbose = FALSE
)

rec_devsn = rec_devs[asmt_t[tstep]:((asmt_t[tstep])+(tasmt-1))]

rdev1=parlist[["recdev2"]]
rdevnew = rbind(rdev1, data.frame(year=c((dat$endyr-1):dat$endyr),recdev=rec_devsn))
parlist[["recdev2"]]=rdevnew

#lengthen parlist with dummy F
tmp=parlist[["F_rate"]]
tmpF=catch[,1:4]
names(tmpF)=names(tmp)
tmpF$F=0.0001

tmpF2=rbind(tmp,tmpF)
tmpF3=tmpF2[order(tmpF2$fleet,tmpF2$year,tmpF2$seas),]

if (is.null(seed)) {
  seed <- stats::runif(1, 1, 9999999)
}

parlist[["F_rate"]]=tmpF3

catch_intended <- rbind(catch, harvest_rate)

#remove duplicate elements if any
catch_intended <- catch_intended[!duplicated(catch_intended[, 1:3]), ]

#extend catch data frame to include other inputs
catch_intended <- cbind(
  catch_intended, catch_intended[, "catch"], catch_intended[, "catch"],
  rep(1, length(catch_intended[, "catch"])), rep(1, length(catch_intended[, "catch"])),
  rep(1, length(catch_intended[, "catch"])), rep(1.5, length(catch_intended[, "catch"])),
  rep(2, length(catch_intended[, "catch"])), catch_intended[, "catch"],
  catch_intended[, "catch"], catch_intended[, "catch"], catch_intended[, "catch"]
)

colnames(catch_intended) <- c(
  "year", "seas", "fleet", "catch", "F", "F_ref",
  "Catch_ref", "basis", "basis_2", "scale", "F_lim",
  "last_adjust", "catch_targ", "F_targ", "catch_imp", "F_imp"
)
impl_error = NULL

#fill in other variables in addition to catch line by line
for (i in seq_along(catch_intended[, "catch"])) {
  
  #set value for the F_limit - specifies the maximum F
  if (!is.null(F_limit)) {
    F_lim <- F_limit[(F_limit[, "year"] == catch_intended[i, "year"]) &
                       (F_limit[, "seas"] == catch_intended[i, "seas"]) &
                       (F_limit[, "fleet"] == catch_intended[i, "fleet"]), "limit"]
    if (length(F_lim) != 1) {
      F_lim <- 1.5
    }
  } else {
    F_lim <- 1.5
  }
  catch_intended[i, "F_lim"] <- F_lim
  
  #fill in the catch_basis column if 1 it is retained biomass, if 2 it's dead biomass
  if (!is.null(catch_basis)) {
    basis_2 <- catch_basis[(catch_basis[, "year"] == catch_intended[i, "year"]) &
                             (catch_basis[, "seas"] == catch_intended[i, "seas"]) &
                             (catch_basis[, "fleet"] == catch_intended[i, "fleet"]), "basis"]
    if (length(basis_2) != 1) {
      basis_2 <- 1
    }
  } else {
    basis_2 <- 1
  }
  catch_intended[i, "basis_2"] <- basis_2
  
  #finds the F and catch from previous year
  #check if the output from the par file has the years of the catch intended
  last_F <- parlist[["F_rate"]][which(parlist[["F_rate"]][, c("year")] == (catch_intended[i, c("year")] - 1) &
                                        parlist[["F_rate"]][, c("seas")] == catch_intended[i, c("seas")] &
                                        parlist[["F_rate"]][, c("fleet")] == catch_intended[i, c("fleet")]), "F"]
  
  if (length(last_F) == 0) {
    last_F <- 0
  }
  
  last_catch <- dat[["catch"]][which(dat[["catch"]][, c("year")] == (catch_intended[i, c("year")] - 1) &
                                       dat[["catch"]][, c("seas")] == catch_intended[i, c("seas")] &
                                       dat[["catch"]][, c("fleet")] == catch_intended[i, c("fleet")]), "catch"]
  
  if (length(last_catch) == 0) {
    last_catch <- 0
  }
  
  if (length(which(parlist[["F_rate"]][, c("year")] == catch_intended[i, c("year")] &
                   parlist[["F_rate"]][, c("seas")] == catch_intended[i, c("seas")] &
                   parlist[["F_rate"]][, c("fleet")] == catch_intended[i, c("fleet")])) == 1) {
    catch_intended[i, "F_ref"] <- which(parlist[["F_rate"]][, c("year")] == catch_intended[i, c("year")] &
                                          parlist[["F_rate"]][, c("seas")] == catch_intended[i, c("seas")] &
                                          parlist[["F_rate"]][, c("fleet")] == catch_intended[i, c("fleet")])
  } else {
    catch_intended[i, "F_ref"] <- 0
  }
  
  catch_intended[i, "Catch_ref"] <- which(dat[["catch"]][, c("year")] == catch_intended[i, c("year")] &
                                            dat[["catch"]][, c("seas")] == catch_intended[i, c("seas")] &
                                            dat[["catch"]][, c("fleet")] == catch_intended[i, c("fleet")])
  
  if (is.na(catch_intended[i, "Catch_ref"])) {
    catch_intended[i, "Catch_ref"] <- 0
  }
  
  if (is.na(catch_intended[i, "F_ref"])) {
    catch_intended[i, "F_ref"] <- 0
  }
  
  if (is.na(catch_intended[i, "F_ref"]) | catch_intended[i, "F_ref"] == 0) {
    catch_intended[i, "F"] <- 0
  } else {
    catch_intended[i, "F"] <- parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"]
  }
  
  if (is.na(catch_intended[i, "Catch_ref"]) | catch_intended[i, "Catch_ref"] == 0) {
    catch_intended[i, "catch"] <- 0
  } else {
    catch_intended[i, "catch"] <- dat[["catch"]][catch_intended[i, "Catch_ref"], "catch"]
  }
  
  
  if (!is.null(catch)) {
    temp_catch <- catch[catch[, "year"] == catch_intended[i, "year"] &
                          catch[, "seas"] == catch_intended[i, "seas"] &
                          catch[, "fleet"] == catch_intended[i, "fleet"], "catch"]
    if (length(temp_catch) == 1) {
      catch_intended[i, "catch"] <- temp_catch
      catch_intended[i, "basis"] <- 1
    } else {
      catch_intended[i, "catch"] <- 0.001
      catch_intended[i, "basis"] <- 2
    }
  } else {
    catch_intended[i, "catch"] <- 0.001
    catch_intended[i, "basis"] <- 2
  }
  
  if (!is.null(harvest_rate)) {
    temp_harvest_rate <- harvest_rate[harvest_rate[, "year"] == catch_intended[i, "year"] &
                                        harvest_rate[, "seas"] == catch_intended[i, "seas"] &
                                        harvest_rate[, "fleet"] == catch_intended[i, "fleet"], "catch"]
    if (length(temp_harvest_rate) == 1) {
      catch_intended[i, "F"] <- temp_harvest_rate
    } else {
      if (last_catch <= 0) {
        if (catch_intended[i, "catch"] == 0) {
          catch_intended[i, "F"] <- 0
        } else {
          if (last_F == 0) {
            catch_intended[i, "F"] <- 0.01
          } else {
            catch_intended[i, "F"] <- last_F
          }
        }
      } else {
        if (last_F <= 0) {
          catch_intended[i, "F"] <- 0.01
        } else {
          catch_intended[i, "F"] <- min(((catch_intended[i, "catch"] / max(last_catch, 0.01)) * (max(last_F, 0.01))), 0.3)
        }
      }
    }
    
    if (catch_intended[i, "basis"] == 2) {
      catch_intended[i, "catch"] <- max(last_catch, 0.01) * ((catch_intended[i, "F"]) / (max(last_F, 0.01)))
    } else if (catch_intended[i, "basis"] == 1) {
      if (catch_intended[i, "catch"] > 0) {
        catch_intended[i, "F"] <- max(0.01, catch_intended[i, "F"])
      } else {
        catch_intended[i, "catch"] <- 0
        catch_intended[i, "F"] <- 0
      }
    }
  } else {
    if (last_catch <= 0) {
      if (catch_intended[i, "catch"] == 0) {
        catch_intended[i, "F"] <- 0
      } else {
        if (last_F <= 0.01) {
          catch_intended[i, "F"] <- 0.01
        } else {
          catch_intended[i, "F"] <- last_F
        }
      }
    } else {
      if (last_F <= 0) {
        catch_intended[i, "F"] <- 0.01
      } else {
        catch_intended[i, "F"] <- min(((catch_intended[i, "catch"] / max(last_catch, 0.01)) * (max(last_F, 0.01))), 0.3)
      }
    }
  }
  
  catch_intended[i, c("catch_targ", "F_targ")] <- catch_intended[i, c("catch", "F")]
  catch_intended[i, c("catch_imp", "F_imp")] <- catch_intended[i, c("catch", "F")]
  if (!is.null(impl_error)) {
    # TODO: This code is here as a place holder if we eventually update to fleet and season specific implementation error
    # temp_impl_error <- impl_error[impl_error[,"year"]==temp_catch[i,"year"] &
    #                               impl_error[,"seas"]==temp_catch[i,"seas"] &
    #                               impl_error[,"fleet"]==temp_catch[i,"fleet"] ,"error"]
    temp_impl_error <- impl_error[impl_error[, "year"] == catch_intended[i, "year"], "error"]
    temp_impl_error <- 1
    if (length(temp_impl_error) == 1) {
      if (temp_impl_error >= 0) {
        catch_intended[i, c("catch_imp", "F_imp")] <- catch_intended[i, c("catch", "F")] * temp_impl_error
      }
    }
  }
  
  if (catch_intended[i, "catch"] == 0) {
    catch_intended[i, "F"] <- 0
  }
  if (catch_intended[i, "F"] == 0) {
    catch_intended[i, "catch"] <- 0
  }
  
  if (!is.na(catch_intended[i, "Catch_ref"]) & catch_intended[i, "Catch_ref"] > 0) {
    if (is.na(catch_intended[i, "F_ref"]) | catch_intended[i, "Catch_ref"] == 0) {
      stop("Error in catch search OM has catch reference but no F reference. This is a likely a bug please contact developers for support")
    } else {
      if (catch_intended[i, "catch"] == 0) {
        if (catch_intended[i, "F"] != 0) {
          stop("Catch is 0 but F isn't. This is a likely a bug please contact developers for support")
        } else {
          dat[["catch"]][catch_intended[i, "Catch_ref"], "catch"] <- 0.001
          parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"] <- 0
        }
      } else {
        if (catch_intended[i, "F"] == 0) {
          stop("F is 0 but Catch isn't. This is a likely a bug please contact developers for support")
        } else {
          dat[["catch"]][catch_intended[i, "Catch_ref"], "catch"] <- catch_intended[i, "catch"]
          parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"] <- catch_intended[i, "F"]
        }
      }
    }
  } else {
    if (!is.na(catch_intended[i, "F_ref"]) & catch_intended[i, "F_ref"] > 0) {
      stop("Error in catch search OM has F reference but no Catch reference. This is a likely a bug please contact developers for support")
    }
  }
}
#****************************************
#*
n_F_search_loops = 20

catch_intended <- catch_intended[catch_intended[, "catch"] > 0, , drop = FALSE]
catch_intended <- catch_intended[catch_intended[, "F"] > 0, , drop = FALSE]

if (length(catch_intended[, 1]) > 0) {
  search_log <- catch_intended[, c("year", "seas", "fleet", "catch_targ", "F_targ", "catch_imp", "F_imp", rep(c("catch_imp", "F_imp", "F_imp"), n_F_search_loops))]
  colnames(search_log)[1:7] <- c("year", "seas", "fleet", "catch_target", "F_target", "catch_implemented", "F_implemented")
  for (i in 1:n_F_search_loops) {
    colnames(search_log)[(1:3) + 7 + (i - 1) * 3] <- c(paste0("catch_achieved_", i), paste0("F_achieved_", i), paste0("F_update_", i))
    search_log[, (1:3) + 7 + (i - 1) * 3] <- NA
  }
} else {
  search_log <- catch_intended[, c("year", "seas", "fleet", "catch_targ", "F_targ", "catch_imp", "F_imp")]
}

#set the directory for the OM F search
OM_dir=paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot",sep="")

# write new files
r4ss::SS_writepar_3.30(
  parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
  overwrite = TRUE, verbose = FALSE
)

r4ss::SS_writedat(dat,
                  outfile = file.path(file_out),
                  overwrite = TRUE,
                  verbose = FALSE
)

#read in new ctl file
ctl <- SS_readctl(
  file = blk_out,
  version = "3.30", use_datlist = TRUE, datlist = dat,
  verbose = FALSE
)
#********************************************************
if (length(catch_intended[, 1]) > 0) {
  achieved_Catch <- FALSE
} else {
  achieved_Catch <- TRUE
}
search_loops <- 0

#starting F search 
tolerance_F_search = 0.001

while (achieved_Catch == FALSE) {
  if (max(abs(catch_intended[, "last_adjust"] - 1)) > tolerance_F_search & search_loops < n_F_search_loops) {
    achieved_Catch <- FALSE
    
    r4ss::SS_writepar_3.30(
      parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
      overwrite = TRUE, verbose = FALSE
    )
    
    search_loops <- search_loops + 1
    
    run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess",
                 verbose = FALSE,
                 debug_par_run = FALSE
    )
    
    # Load the SS results
    outlist <- r4ss::SS_output(OM_dir,
                               verbose = FALSE, printstats = FALSE,
                               covar = FALSE, warn = FALSE, readwt = FALSE
    )
    
    # Extract the achieved F and Catch
    F_list <- get_F(
      timeseries = outlist[["timeseries"]],
      fleetnames = dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "fleetname"]
    )
    
    units_of_catch <- dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "units"]
    
    names(units_of_catch) <- as.character(which(dat[["fleetinfo"]][["type"]] %in% c(1, 2)))
    
    ret_catch <- get_retained_catch(
      timeseries = outlist[["timeseries"]],
      units_of_catch = units_of_catch
    )
    
    dead_catch <- get_dead_catch(
      timeseries = outlist[["timeseries"]],
      units_of_catch = units_of_catch
    )
    
    F_achieved <- F_list[["F_df"]][, c("Yr", "Seas", "Fleet", "F")]
    colnames(F_achieved) <- c("year", "seas", "fleet", "F")
    
    for (i in 1:length(catch_intended[, 1])) {
      scale_ratio <- catch_intended[i, "basis"]
      
      if (catch_intended[i, "basis"] == 1) {
        intended_landings <- catch_intended[i, "catch"]
        
        if (catch_intended[i, "basis_2"] == 1) {
          achieved_landings <- ret_catch[ret_catch[, "Yr"] == catch_intended[i, "year"] &
                                           ret_catch[, "Seas"] == catch_intended[i, "seas"] &
                                           ret_catch[, "Fleet"] == catch_intended[i, "fleet"], "retained_catch"]
        } else if (catch_intended[i, "basis_2"] == 2) {
          achieved_landings <- dead_catch[dead_catch[, "Yr"] == catch_intended[i, "year"] &
                                            dead_catch[, "Seas"] == catch_intended[i, "seas"] &
                                            dead_catch[, "Fleet"] == catch_intended[i, "fleet"], "retained_catch"]
        } else {
          stop(paste0("intended basis 2 should be equal to 1 or 2 but it is not.
                             This occured for fleet ", catch_intended[i, "fleet"], "
                             in year ", catch_intended[i, "year"], "
                             and season ", catch_intended[i, "seas"], "."))
        }
        
        achieved_F <- F_achieved[F_achieved[, "year"] == catch_intended[i, "year"] &
                                   F_achieved[, "seas"] == catch_intended[i, "seas"] &
                                   F_achieved[, "fleet"] == catch_intended[i, "fleet"], "F"]
        
        if (intended_landings == 0) {
          target_F <- 0
        } else if (achieved_landings == 0) {
          if (achieved_F > 0) {
            catch_intended[i, "basis_2"] <- 2
            warning(paste0("It appears that you set a fleet basis to retianed catch for a discard only fleet if that is not the case something is wrong.
                      We automaticaly changed your basis to total dead catch to compensate and allow convervence.
                             This occured for fleet ", catch_intended[i, "fleet"], "
                             in year ", catch_intended[i, "year"], "
                             and season ", catch_intended[i, "seas"], "."))
          } else {
            target_F <- catch_intended[i, "F"] + 0.01
            catch_intended[i, "F"] <- target_F
          }
        } else {
          target_F <- (intended_landings / achieved_landings) * achieved_F
        }
      } else if (catch_intended[i, "basis"] == 2) {
        target_F <- catch_intended[i, "F"]
        
        achieved_F <- F_achieved[F_achieved[, "year"] == catch_intended[i, "year"] &
                                   F_achieved[, "seas"] == catch_intended[i, "seas"] &
                                   F_achieved[, "fleet"] == catch_intended[i, "fleet"], "F"]
      } else {
        stop(paste0("Something is wrong basis should be 1 or 2.
                             This occured for fleet ", catch_intended[i, "fleet"], "
                             in year ", catch_intended[i, "year"], "
                             and season ", catch_intended[i, "seas"], "."))
      }
      
      if (target_F == 0) {
        catch_intended[i, "last_adjust"] <- 1
        catch_intended[i, "scale"] <- 0
      } else if (achieved_F == 0) {
        catch_intended[i, "last_adjust"] <- 1
        catch_intended[i, "scale"] <- 1
      } else {
        catch_intended[i, "last_adjust"] <- target_F / achieved_F
        catch_intended[i, "scale"] <- ((target_F / achieved_F) - 1) *
          stats::runif(1, 0.75, 1) + 1
      }
      
      if (!is.na(catch_intended[i, "F_ref"])) {
        parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"] <- max(0, min(achieved_F * catch_intended[i, "scale"], catch_intended[i, "F_lim"]))
        if (parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"] == 0 | parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"] == catch_intended[i, "F_lim"]) {
          catch_intended[i, "last_adjust"] <- 1
        }
      } else {
        stop(paste0("Error NA F_refs should have been removed already someing is wrong.
                      This occured for fleet ", catch_intended[i, "fleet"], "
                      in year ", catch_intended[i, "year"], "
                      and season ", catch_intended[i, "seas"], "."))
      }
      
      search_log[i, (1:3) + 7 + (search_loops - 1) * 3] <- c(achieved_landings, achieved_F, parlist[["F_rate"]][catch_intended[i, "F_ref"], "F"])
    }
  } else {
    achieved_Catch <- TRUE
    
    parlist <- r4ss::SS_readpar_3.30(
      parfile = file.path(OM_dir, "ss.par"),
      datsource = dat, ctlsource = ctl,
      verbose = FALSE
    )
    
    
    if (file.exists(file.path(OM_dir, "OM_catch_search_log.csv"))) {
      utils::write.table(
        x = search_log,
        file = file.path(OM_dir, "OM_catch_search_log.csv"),
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        sep = ",",
        dec = ".",
        qmethod = "double"
      )
    } else {
      utils::write.table(
        x = search_log,
        file = file.path(OM_dir, "OM_catch_search_log.csv"),
        append = TRUE,
        row.names = FALSE,
        col.names = TRUE,
        sep = ",",
        dec = ".",
        qmethod = "double"
      )
    }
  }
}

}

