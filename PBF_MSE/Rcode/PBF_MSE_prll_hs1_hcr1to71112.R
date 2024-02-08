#North Pacific Bluefin tuna MSE code

#this runs the PBF MSE framework for candidate HCRs 1 to 7, 11, and 12 for 24 years for the specified iterations
#iterations differ in their recruitment deviations and random implementation errors
#with an assessment every three years

#clean up the workspace
rm(list=ls())

#source all the packages needed
library(foreach)
library(doParallel)
library(r4ss)
library(Rcpp)
library(dplyr)
library(reshape2)

#Specify path of parent directory
pdir = "J:/Desiree/Bluefin/PBF_MSE/"
#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#Specify the path of conditioned initial OM
sdir = "J:/Desiree/Bluefin/PBF_MSE/Condition/"

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#specify the frequency of assessments in years
tasmt = 3

#we are projecting 24 years into the future
#we assume the assessment occurs every 3 years, so there are 10 assessment time steps 
#below we specify when those happen in the 24 years time series as first management action really occurs three years into the simulation
asmt_t = seq(1, 24, by=tasmt)

#import needed files
#catch ratios by fleet and season for each of the fleet groups obtained by running the bluefin_om_base.R code 
cr_wl= read.csv(paste(pdir,"crwl.csv", sep = ""))
cr_ws= read.csv(paste(pdir,"crws.csv", sep = ""))
cr_wm= read.csv(paste(pdir,"crwm.csv", sep = ""))
cr_epo= read.csv(paste(pdir,"crepo.csv", sep = ""))
cr_all = read.csv(paste(pdir,"crdat.csv", sep = ""))

#Set the harvest strategy (i.e. the hcr shape)
hsnum = 1
#Set the HCR
hcrnum=1
#Set the scenario
scnnum=1

#Calculate the numbers of cores 
no_cores = detectCores() - 1

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Run the MSE code for specified number of iterations
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point represting the fraction of unfished SSB
#sa specifies if a stock assessment shoudl be run (1) or not (0)
#fmin specific the fraction of the fishing intensity at the target reference point (TRP) that correpsonds to the minimum fishign intensity
#lag specifies the lag between data availbility and the assessment time step
#obse specifies if to use the expected values (2) or bootstrap (3) from the bootstrap data file that is fed into the EM
#aspm specifies if, when sa=1, to use the aspm EM (aspm ="ASPMR-f1f12") or the full EM (aspm=NULL). This aspm is ASPM-R with size & selectivities for F1 & F12
#Note that the F based TRP is specified already in the forecast file 
#the output is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file created for each iteration
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.15,sa=0,Fmin=0.10,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=2
#Set the scenario
scnnum=1

foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.15,sa=0,Fmin=0.10,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=3
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.15,sa=0,Fmin=0.10,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=4
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.25, Blim = 0.15,sa=0,Fmin=0.10,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=5
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.25, Blim = 0.20,sa=0,Fmin=0.10,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=6
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.10,sa=0,Fmin=0.43,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=7
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.10,sa=0,Fmin=0.67,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=11
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.15, Blim = 0.077,sa=0,Fmin=0.05,lag=0,obse=2,aspm=NULL)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=12
#Set the scenario
scnnum=1
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.077,sa=0,Fmin=0.05,lag=0,obse=2,aspm=NULL)}


stopCluster(cl)
