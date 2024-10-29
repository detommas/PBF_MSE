#Wrapper code to extract impact performance metrics indicators saved for each
#hcr as a text file
# Author: D. Tommasi

library(foreach)
library(doParallel)
library(r4ss)
library(dplyr)
library(reshape2)

#Specify parent directories path 
pdir = "D:/Desiree/PBF_MSE/"

#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

for (j in 1:12) {
  #Calculate the numbers of cores 
  no_cores = detectCores() - 92
  
  #Initiate cluster
  cl= makeCluster(no_cores)
  registerDoParallel(cl)
  
  #specify the HS that is being run
  hsn=1
  hsnum = hsn
  hs = paste(hsnum, "/", sep = "")
  
  #specify the HCR that is being run
  hcrn=j
  hcrnum = hcrn
  hcr = paste(hcrnum, "/", sep = "")
  
  #Specify the scenario being run
  scnn=1
  snum = scnn
  scn = paste(snum, "/", sep ="")
  
  #Extract results from each iteration and add to the same matrix
  itrn =NA
  #only use the last time step
  tstep=8
  
  #specify directory with Ss-executable
  ssdir = paste(pdir,"SS_model/ss.exe",sep="")
  years=1983:2045
  
  idat=data.frame(Year=rep(years,100),
                  SB=rep(c(1:length(years)),100),
                  noEPO=rep(c(1:length(years)),100),
                  noWPO=rep(c(1:length(years)),100),
                  noF=rep(c(1:length(years)),100),
                  noWPOs=rep(c(1:length(years)),100),
                  noWPOl=rep(c(1:length(years)),100),
                  noWPOm=rep(c(1:length(years)),100),
                  noUS=rep(c(1:length(years)),100),
                  noMx=rep(c(1:length(years)),100),
                  noJp=rep(c(1:length(years)),100),
                  noTw=rep(c(1:length(years)),100),
                  noKr=rep(c(1:length(years)),100),
                  noEPOs=rep(c(1:length(years)),100),
                  noWPOs=rep(c(1:length(years)),100),
                  EPOi=rep(c(1:length(years)),100),
                  WPOi=rep(c(1:length(years)),100),
                  WPOsi=rep(c(1:length(years)),100),
                  WPOli=rep(c(1:length(years)),100),
                  WPOmi=rep(c(1:length(years)),100),
                  USi=rep(c(1:length(years)),100),
                  Mxi=rep(c(1:length(years)),100),
                  Jpi=rep(c(1:length(years)),100),
                  Twi=rep(c(1:length(years)),100),
                  Kri=rep(c(1:length(years)),100),
                  itr=rep(c(1:length(years)),100))
  
  indx=seq(1,6300,by=63)
  
  foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% {
    impact_pbfMSE_prllitr(pdir=pdir, hs=hs, hcr=hcr,scn=scn, itr=itr,tstep=tstep,ssdir=ssdir,idat=idat,indx=indx)
  }
  
  stopCluster(cl)
  
  for(itrn in 1:100){
    imat=read.table(paste(pdir,hs,hcr, scn,itrn, "/impact",hsnum,hcrnum,snum,".txt", sep =""))
    imat$itr=itrn
    idat[indx[itrn]:(indx[itrn]+62),]=imat
  }
  
  #save output to file
  write.table(idat, paste(pdir,hs,hcr, scn, "/impact",hsnum,hcrnum,snum,".txt", sep =""))
  
}
