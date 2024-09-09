#North Pacific Bluefin tuna MSE code

#this runs the PBF MSE framework for 21 years for the specified iteration
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
library(tidyverse)

#Specify path of parent directory
pdir = "D:/Desiree/PBF_MSE/"
#pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/s1720/PBF_MSE/"
#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#Specify the path of conditioned initial OM
sdir = "D:/Desiree/PBF_MSE/Condition/"
#sdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/s1720/PBF_MSE/Condition/"

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#specify the frequency of assessments in years
tasmt = 3

#we are projecting 24 years into the future
#we assume the assessment occurs every 3 years, so there are 10 assessment time steps 
#below we specify when those happen in the 24 years time series as first management action really occurs three years into the simulation
asmt_t = seq(2, 23, by=tasmt)

#import needed files
#catch ratios by fleet and season for each of the fleet groups obtained by running the OMinit_catch.R code 
ciom= read.csv(paste(pdir,"ciom.csv", sep = ""))
ciom25= read.csv(paste(pdir,"ciom25.csv", sep = ""))

#read selectivity deviations
#sdev= read.csv(paste(pdir,"sdev1720.csv", sep = ""))
#sdev= read.csv(paste(pdir,"sdev1722.csv", sep = ""))
sdev= read.csv(paste(pdir,"sdev1522.csv", sep = ""))

#Set the harvest strategy (i.e. the hcr shape)
hsnum = 3
#Set the HCR
hcrnum=201
#Set the scenario
scnnum=3

#Calculate the numbers of cores 
no_cores = detectCores() - 92

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Run the MSE code for specified number of iterations
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point representing the fraction of unfished SSB
#sa specifies if a stock assessment shoudl be run (1) or not (0)
#Fmin specific the fraction of the fishing intensity at the target reference point that correpsonds to the minimum fishign intensity
#lag specifies the lag between data availbility and the assessment time step
#obse specifies if to use the expected values (2) or bootstrap (3) from the bootstrap data file that is fed into the EM
#aspm specifies if, when sa=1, to use the aspm EM (aspm ="ASPMR-f1f12") or the full EM (aspm=NULL). 
#yfor specifies the years the selectivity and relF are averaged over to find benchamark quantities and for the forecast
#tacl specifies if the 25% change on TAC change between assessment periods should be apploed (tacl=25) or not (tacl=0)
#Note that the F based TRP is specified already in the forecast file 
#the output is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.15,sa=1,Fmin=0.10,lag=1,obse=3,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=202
#Set the scenario
scnnum=3

foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.25, Blim = 0.15,sa=1,Fmin=0.10,lag=1,obse=3,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=3
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.15,sa=0,Fmin=0.10,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=4
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.25, Blim = 0.15,sa=0,Fmin=0.10,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=5
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.25, Blim = 0.20,sa=0,Fmin=0.10,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=6
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.10,sa=0,Fmin=0.43,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=7
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.10,sa=0,Fmin=0.67,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=11
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.15, Blim = 0.077,sa=0,Fmin=0.05,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

#terminate cluster
stopCluster(cl)

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Set the harvest strategy
hsnum = 3
#Set the HCR
hcrnum=12
#Set the scenario
scnnum=3
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20, Blim = 0.077,sa=0,Fmin=0.05,lag=1,obse=2,aspm=NULL,yfor=c(2015:2022),tacl=25)}

stopCluster(cl)
