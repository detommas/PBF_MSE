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
#' @param nrep how many times the catch matrix has to be replicated (1 means it remains as is)
#' @param cdat_new set of new catch data, based on previous assessment time TAC
#' @param cdat_new2 set of new catch data, based on TAC from two previous assessments
#' @param tstep assessment time step
#' @return A modified dat file.
#' @author Desiree Tommasi

change_dat_cpue_boot_adj_s1 <- function(ss_file_in, ss_file_out, nrep, cdat_new, cdat_new2, tstep){
  
  #read in data file from previous assessment period
  om_dat = SS_readdat(ss_file_in)
  
  #create a new data file based on the old to be modified
  om_dat_new = om_dat
  
  #change end year to the end of next assessment period
  om_dat_new$endyr = om_dat$endyr + nrep
  
  #generate a vector of years for the catch data based on the end year of the previous data file and the assessment frequency
  ldat=length(cdat_new$catch)
  cyear=rep(1,ldat*nrep)
  for (j in 1:nrep){
    cyear[(j*ldat-ldat+1):(j*ldat)] = rep((om_dat$endyr+j),ldat)}
  
  #repeat catch data according to the assessment period
  #the first line repeats the TAC for the assessment cycle (tasmt)
  #however, season 1 and 2 are based actually on the old TAC (cdat_new2)
  #so catches for those season on the first year need to be replaced
  cdat_new3 = data.frame(year=cyear,seas=rep(cdat_new$Seas,nrep),fleet=rep(cdat_new$Fleet,nrep),catch=rep(cdat_new$catch,nrep),catch_se=rep(0.1,ldat*nrep))
  cdat_new3$catch[cdat_new3$seas%in%c(1,2)&cdat_new3$year==(om_dat$endyr + 1)]=cdat_new2$catch[cdat_new2$Seas%in%c(1,2)]
  
  #change the catch data
  catch_new = rbind(om_dat$catch, cdat_new3)
  om_dat_new$catch = catch_new
  
  #change the CPUE data
  #In this scenario we assume that in the future only data from the 
  #the TWLL one (31) will continue. This might be overly optimistic, can try different data scenarios.
  #these are all used in the likelihood already according to the ctl file
  
  cpue_old = om_dat$CPUE
  cpue31 = cpue_old %>% filter(index==31)
  #cpue29 = cpue_old %>% filter(index==29) #Japanese LL
  #cpue27 = cpue_old %>% filter(index==27) #Japanese troll
  
  #in some r4ss version cpue has a seas var but in the most recent it uses month instead
  #so need to change the second heading to seas
  names(cpue31)[2]="seas"
  names(cpue_old)[2]="seas"
  
  #Add CPUE dummy data for the additional future years of this time step
  #Note that the SE used here was the one in the 2020 assessment. 
  #This can be increased/decreased to change the observation error
  yearn = ((om_dat_new$endyr-nrep+1):om_dat_new$endyr)
  cpue31n = data.frame(year=yearn,seas=cpue31$seas[1:nrep],index=rep(31,nrep),obs=rep(1.5,nrep),se_log=rep(0.2,nrep))
  #cpue29n = data.frame(year=yearn,seas=cpue29$seas[1:t_asmt],index=rep(29,t_asmt),obs=rep(1.5,t_asmt),se_log=rep(0.2,t_asmt))
  #cpue27n = data.frame(year=yearn,seas=cpue27$seas[1:t_asmt],index=rep(27,t_asmt),obs=rep(1.5,t_asmt),se_log=rep(0.2,t_asmt))
  
  #i312927 = which(cpue_old$index==31|cpue_old$index==29|cpue_old$index==27)
  i31 = which(cpue_old$index==31)
  #cpue_new = rbind(cpue_old[-i312927,],cpue31, cpue31n, cpue29, cpue29n, cpue27, cpue27n)
  cpue_new = rbind(cpue_old[-i31,],cpue31, cpue31n)
  om_dat_new$CPUE = cpue_new
  
  #Create dummy length composition data
  #Note that the Nsamp taken was the mean of the Nsamp specified in the 2020 stock assessment dat file for that fleet
  #So in this case the comps can be more informative than for any average year
  #Nsamp can be changed to increase/decrease the observation error
  len_old = om_dat$sizefreq_data_list
  #To address bootstrapping bias all the effective sample sizes need to be multiplied by 10 based on the procedure 4  
  #in ISC21/PBFWG-1/07 written by H. Lee
  #Only do this for the first time step as they woudl have been already adjusted in the following time steps
  if (tstep == 1){
  for (j in 1:23){
    len_old[[j]]$Nsamp=len_old[[j]]$Nsamp*10
    names(len_old[[j]])[1:4]=c("Method","Yr","Seas","FltSvy")
  }
  }
  for (j in 1:23){
     names(len_old[[j]])[1:4]=c("Method","Yr","Seas","FltSvy")
  }
  #generate new dummy data data for fleet 1
  #select a random 2 years as a template - note only data from season 11.5 is currently being used in SAM
  len1n = len_old[[1]] %>% filter(Yr %in% c((2016-(nrep-1)):2016))
  len1n$Yr = yearn
  len1n$Nsamp = rep(round(mean(len_old[[1]]$Nsamp)),nrep)
  
  #Lcomps for fleet 2 have two seasons but were not sampled for three years, so use fleet 1 template
  len2ns1 = len1n
  len2ns1$Method = 2
  len2ns1$Seas = 8.5
  len2ns1$FltSvy = 2
  len2ns2 = len2ns1
  len2ns2$Method = 2
  len2ns2$Seas = 11.5
  len2ns1$Nsamp = rep(round(mean((len_old[[2]]%>% filter(Seas==8.5))$Nsamp)),nrep)
  len2ns2$Nsamp = rep(round(mean((len_old[[2]]%>% filter(Seas==11.5))$Nsamp)),nrep)
  len2n=rbind(len2ns1,len2ns2)
  
  #Lcomps for fleet 3 
  len3n = len_old[[3]] %>% filter(Yr %in% c((2016-(nrep-1)):2016))
  len3n$Yr = yearn
  len3n$Nsamp = rep(round(mean(len_old[[3]]$Nsamp)),nrep)
  
  #Lcomps for fleet 4 
  len4n = len_old[[4]] %>% filter(Yr %in% c((2016-(nrep-1)):2016))
  len4n$Yr = yearn
  len4n$Nsamp = rep(round(mean(len_old[[4]]$Nsamp)),nrep)
  
  #Lcomps for fleet 5 
  len5n = len_old[[5]] %>% filter(Yr %in% c((2016-(nrep-1)):2016))
  len5n$Yr = yearn
  len5n$Nsamp = rep(round(mean((len_old[[5]]%>% filter(Seas==11.5))$Nsamp)),nrep)
  
  #Lcomps for fleet 6 
  #here we assume they are
  len6n = len_old[[6]] %>% filter(Yr %in% c((2016-(nrep-1)):2016))
  len6n$Yr = yearn
  len6n$Nsamp = rep(round(mean(len_old[[6]]$Nsamp)),nrep)
  
  #Lcomps for fleet 7 - 
  #here we assume they are
  len7n = len_old[[7]] %>% filter(Yr %in% c((2020-(nrep-1)):2020))
  len7n$Yr = yearn
  len7n$Nsamp = rep(round(mean(len_old[[7]]$Nsamp)),nrep)
  
  #Lcomps for fleet 8 have two seasons 
  len8ns1 = len_old[[8]] %>% filter(Yr %in% c((2017-(nrep-1)):2017)&Seas==11.5)
  len8ns1$Yr = yearn
  len8ns2 = len_old[[8]] %>% filter(Yr %in% c((2017-(nrep-1)):2017)&Seas==2.5)
  len8ns2$Yr = yearn
  len8ns1$Nsamp = rep(round(mean((len_old[[8]]%>% filter(Seas==11.5))$Nsamp)),nrep)
  len8ns2$Nsamp = rep(round(mean((len_old[[8]]%>% filter(Seas==2.5))$Nsamp)),nrep)
  len8n=rbind(len8ns1,len8ns2)
  
  #Lcomps for fleet 9
  len9n = len_old[[9]] %>% filter(Yr %in% c((2020-(nrep-1)):2020))
  len9n$Yr = yearn
  len9n$Nsamp = rep(round(mean(len_old[[9]]$Nsamp)),nrep)
  
  #Lcomps for fleet 10
  len10n = len_old[[10]] %>% filter(Yr %in% c((2020-(nrep-1)):2020))
  len10n$Yr = yearn
  len10n$Nsamp = rep(round(mean(len_old[[10]]$Nsamp)),nrep)
  
  #Lcomps for fleet 11
  len11n = len_old[[11]] %>% filter(Yr %in% c((2020-(nrep-1)):2020))
  len11n$Yr = yearn
  len11n$Nsamp = rep(round(mean(len_old[[11]]$Nsamp)),nrep)
  
  #Lcomps for fleet 12 
  len12ns1 = len_old[[12]] %>% filter(Yr %in% c((2022-(nrep-1)):2022)&Seas==11.5)
  len12ns1$Yr = yearn
  len12ns2 = len_old[[12]] %>% filter(Yr %in% c((2020-(nrep-1)):2020)&Seas==8.5)
  len12ns2$Yr = yearn
  len12ns3 = len_old[[12]] %>% filter(Yr %in% c((2019-(nrep-1)):2019)&Seas==5.5)
  len12ns3$Yr = yearn
  len12ns1$Nsamp = rep(round(mean((len_old[[12]]%>% filter(Seas==11.5))$Nsamp)),nrep)
  len12ns2$Nsamp = rep(round(mean((len_old[[12]]%>% filter(Seas==8.5))$Nsamp)),nrep)
  len12ns3$Nsamp = rep(round(mean((len_old[[12]]%>% filter(Seas==5.5))$Nsamp)),nrep)
  len12n=rbind(len12ns1,len12ns2,len12ns3)
  
  #Lcomps for fleet 13
  len13n = len_old[[13]] %>% filter(Yr %in% c((2008-(nrep-1)):2008))
  len13n$Yr = yearn
  len13n$Nsamp = rep(round(mean(len_old[[13]]$Nsamp)),nrep)
  
  #Lcomps for fleet 18 
  len18n = len_old[[18]] %>% filter(Yr %in% c((2020-(nrep-1)):2020))
  len18n$Yr = yearn
  len18n$Nsamp = rep(round(mean(len_old[[18]]$Nsamp)),nrep)
  
  #Lcomps for fleet 21  
  len21n = len_old[[21]] %>% filter(Yr %in% c((2022-(nrep-1)):2022)&Seas==8.5)
  len21n$Yr = yearn
  len21n$Nsamp = rep(round(mean((len_old[[21]]%>% filter(Seas==8.5))$Nsamp)),nrep)
  
  #Lcomps for fleet 22  
  len22n = len_old[[22]] %>% filter(Yr %in% c((2022-(nrep-1)):2022))
  len22n$Yr = yearn
  len22n$Nsamp = rep(round(mean(len_old[[22]]$Nsamp)),nrep)
  
  len_new = len_old
  for (j in c(1:13,18,21,22)){
    len_new[[j]]=rbind(len_old[[j]],eval(str2lang(paste("len",j,"n",sep=""))))
  }
  
  om_dat_new$sizefreq_data_list = len_new
  
  #update the number of observations per method
  nm = 1:23
  for (j in 1:23){
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
  