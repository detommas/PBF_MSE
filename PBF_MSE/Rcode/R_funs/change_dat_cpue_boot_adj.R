#' Add to a Stock Synthesis data file new catch data
#'
#' This function changes an SS dat file to
#' 1) add more catch data
#' 2) changes the numbers of catch data
#' 3) changes the end year
#' 4) adds dummy CPUE data and error for each of the two NPALB indexes: 0.1 and 0.12
#' 5) adds dummy size comp data and effective sample size
#' 
#' It then writes a new data file into the working directory
#'
#' @param ss_file_in filename of original dat file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new dat file with full path or relative to working directory
#' @param t_asmt how often assessments are run in years (e.g if every three years, 3)
#' @param nrep how many times the catch matrix has to be replicated (1 means it remains as is)
#' @param cdat_new set of new catch data, based on previous assessment time TAC
#' @return A modified dat file.
#' @author Desiree Tommasi

change_dat_cpue_boot_adj <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new){
  
  #read in data file from previous assessment period
  om_dat = SS_readdat(ss_file_in)
  
  #create a new data file based on the old to be modified
  om_dat_new = om_dat
  
  #change end year to the end of next assessment period
  om_dat_new$endyr = om_dat$endyr + t_asmt
  
  #generate a vector of years for the catch data based on the end year of the previous data file and the assessment frequency
  ldat=length(cdat_new$catch)
  cyear=rep(1,ldat*t_asmt)
  for (j in 1:t_asmt){
    cyear[(j*ldat-ldat+1):(j*ldat)] = rep((om_dat$endyr+j),ldat)}
  
  #repeat catch data according to the assessment period
  cdat_new3 = data.frame(year=cyear,seas=rep(cdat_new$Seas,t_asmt),fleet=rep(cdat_new$Fleet,t_asmt),catch=rep(cdat_new$catch,t_asmt),catch_se=rep(0.1,ldat*t_asmt))
  
  #change the catch data
  catch_new = rbind(om_dat$catch, cdat_new3)
  om_dat_new$catch = catch_new
  
  #change the CPUE data
  #In this scenario we assume that in the future data from the Japan LL index (21) and the Jpn Troll (24) will continue
  #in addition to the TWLL one (25). This might be overly optimistic, can try different data scenarios.
  #these are all used in the likelihood already according to the ctl file
  
  cpue_old = om_dat$CPUE
  cpue21 = cpue_old %>% filter(index==21)
  cpue24 = cpue_old %>% filter(index==24)
  cpue25 = cpue_old %>% filter(index==25)
  
  #Add CPUE dummy data for the additional future years of this time step
  #Note that the SE used here was the one in the 2020 assessment. 
  #This can be increased/decreased to change the observation error
  yearn = ((om_dat$endyr+1):(om_dat$endyr+t_asmt))
  cpue21n = data.frame(year=yearn,seas=cpue21$seas[1:t_asmt],index=rep(21,t_asmt),obs=rep(1.5,t_asmt),se_log=rep(0.2,t_asmt))
  cpue24n = data.frame(year=yearn,seas=cpue24$seas[1:t_asmt],index=rep(24,t_asmt),obs=rep(1.5,t_asmt),se_log=rep(0.2,t_asmt))
  cpue25n = data.frame(year=yearn,seas=cpue25$seas[1:t_asmt],index=rep(25,t_asmt),obs=rep(1.5,t_asmt),se_log=rep(0.2,t_asmt))
  
  i212425 = which(cpue_old$index==21|cpue_old$index==24|cpue_old$index==25)
  cpue_new = rbind(cpue_old[-i212425,],cpue21, cpue21n, cpue24, cpue24n, cpue25, cpue25n)
  
  om_dat_new$CPUE = cpue_new
  
  #Create dummy length composition data
  #Note that the Nsamp taken was the mean of the Nsamp specified in the 2020 stock assessment dat file for that fleet
  #So in this case the comps can be more informative than for any average year
  #Nsamp can be changed to increase/decrease the observation error
  len_old = om_dat$sizefreq_data_list
  
  #To address bootstrapping bias all the effective sample sizes need to be multiplied by 10 based on the procedure 4  
  #in ISC21/PBFWG-1/07 written by H. Lee 
  for (j in 1:22){
    len_old[[j]]$Nsamp=len_old[[j]]$Nsamp*10
  }
  
  #generate new dummy data data for fleet 1
  #select a random 2 years as a template - note only data from season 11.5 is currently being used in SAM
  len1n = len_old[[1]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016))
  len1n$Yr = yearn
  len1n$Nsamp = rep(round(mean(len_old[[1]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 2 have two seasons
  len2ns1 = len_old[[2]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016)&Seas==2.5)
  len2ns1$Yr = yearn
  len2ns1$Nsamp = rep(round(mean((len_old[[2]]%>% filter(Seas==2.5))$Nsamp)),t_asmt)
  len2ns2 = len_old[[2]] %>% filter(Yr %in% c((2018-(t_asmt-1)):2018)&Seas==11.5)
  len2ns2$Yr = yearn
  len2ns2$Nsamp = rep(round(mean((len_old[[2]]%>% filter(Seas==11.5))$Nsamp)),t_asmt)
  len2n=rbind(len2ns1,len2ns2)
  
  #Lcomps for fleet 3 
  len3n = len_old[[3]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016))
  len3n$Yr = yearn
  len3n$Nsamp = rep(round(mean(len_old[[3]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 4 
  len4n = len_old[[4]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016))
  len4n$Yr = yearn
  len4n$Nsamp = rep(round(mean(len_old[[4]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 5 
  len5n = len_old[[5]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016))
  len5n$Yr = yearn
  len5n$Nsamp = rep(round(mean(len_old[[5]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 6 - here samples come from 3 seasons, but not all are consistently sampled ever year
  #here we assume they are
  len6ns1 = len_old[[6]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016)&Seas==5.5)
  len6ns1$Yr = yearn
  len6ns1$Nsamp = rep(round(mean((len_old[[6]]%>% filter(Seas==5.5))$Nsamp)),t_asmt)
  len6ns2 = len_old[[6]] %>% filter(Yr %in% c((2012-(t_asmt-1)):2012)&Seas==8.5)
  len6ns2$Yr = yearn
  len6ns2$Nsamp = rep(round(mean((len_old[[6]]%>% filter(Seas==8.5))$Nsamp)),t_asmt)
  len6ns3 = len_old[[6]] %>% filter(Yr %in% c((2008-(t_asmt-1)):2008)&Seas==11.5)
  len6ns3$Yr = yearn
  len6ns3$Nsamp = rep(round(mean((len_old[[6]]%>% filter(Seas==11.5))$Nsamp)),t_asmt)
  len6n=rbind(len6ns1,len6ns2,len6ns3)

  #Lcomps for fleet 12 
  len12n = len_old[[12]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016))
  len12n$Yr = yearn
  len12n$Nsamp = rep(round(mean(len_old[[12]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 14 
  len14n = len_old[[14]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020))#recently switched form 11.5 to 8.5 season
  len14n$Yr = yearn
  len14n$Nsamp = rep(round(mean(len_old[[14]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 15 
  len15ns1 = len_old[[15]] %>% filter(Yr %in% c((2016-(t_asmt-1)):2016)&Seas==2.5)
  len15ns1$Yr = yearn
  len15ns1$Nsamp = rep(round(mean((len_old[[15]]%>% filter(Seas==2.5))$Nsamp)),t_asmt)
  temp=len_old[[15]]
  len15ns2 = temp[dim(len_old[[15]])[1]:(dim(len_old[[15]])[1]-2),]
  len15ns2$Seas = rep(5.5,t_asmt)
  len15ns2$Yr = yearn
  len15ns2$Nsamp = rep(round(mean((len_old[[15]]%>% filter(Seas==5.5))$Nsamp)),t_asmt)
  len15ns3 = len15ns2
  len15ns2$Seas = rep(11.5,t_asmt)
  len15ns3$Yr = yearn
  len15ns3$Nsamp = rep(round(mean((len_old[[15]]%>% filter(Seas==11.5))$Nsamp)),t_asmt)
  len15n=rbind(len15ns1,len15ns2,len15ns3)
  
  #Lcomps for fleet 17 
  len17n = len_old[[17]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020))
  len17n$Yr = yearn
  len17n$Nsamp = rep(round(mean(len_old[[17]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 18 
  len18n = len_old[[18]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020))
  len18n$Yr = yearn
  len18n$Nsamp = rep(round(mean(len_old[[18]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 20 
  len20n = len_old[[20]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020))
  len20n$Yr = yearn
  len20n$Nsamp = rep(round(mean(len_old[[20]]$Nsamp)),t_asmt)
  
  #Lcomps for fleet 21 - in recent years sampling 2 seasons 
  len21ns1 = len_old[[21]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020)&Seas==8.5)
  len21ns1$Yr = yearn
  len21ns1$Nsamp = rep(round(mean((len_old[[21]]%>% filter(Seas==8.5))$Nsamp)),t_asmt)
  len21ns2 = len_old[[21]] %>% filter(Yr %in% c((2020-(t_asmt-1)):2020)&Seas==11.5)
  len21ns2$Yr = yearn
  len21ns2$Nsamp = rep(round(mean((len_old[[21]]%>% filter(Seas==11.5))$Nsamp)),t_asmt)
  len21n=rbind(len21ns1,len21ns2)
  
  len_new = len_old
  for (j in c(1:6,12,14,15,17,18,20,21)){
    len_new[[j]]=rbind(len_old[[j]],eval(str2lang(paste("len",j,"n",sep=""))))
  }
  
  om_dat_new$sizefreq_data_list = len_new
  
  #update the number of observations per method
  nm = 1:22
  for (j in 1:22){
    nm[j]=dim(len_new[[j]])[1]
  }
  
  om_dat_new$Nobs_per_method = nm
  
  #ensure that the bootstrap compression is not 0.01 for fleet 10 and fleet 11 
  #that is mirrored to it to remove bootstrap bias according to Lee et al. 2022
  om_dat_new$mincomp_per_method[10]=0.0001
  om_dat_new$mincomp_per_method[11]=0.0001
  
  #Write a stock synthesis data file from the mse_dat list object
  SS_writedat(om_dat_new, ss_file_out)
  
}
  