#North Pacific Bluefin tuna MSE code to multiple iterations in parallel

#this runs the PBF MSE framework for 30 years for the specified iterations
#iterations differ in their recruitment deviations and random implementation errors

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
pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/"
#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#Specify the path of conditioned initial OM
sdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/Condition/"

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#specify the frequency of assessments
tasmt = 2

#we are projecting 30 years into the future
#we assume the assessment occurs every 3 years, so there are 10 assessment time steps 
#below we specify when those happen in the 30 years time series
asmt_t = seq(1, 30, by=tasmt)

#import needed files
#catch ratios by fleet and season for each of the fleet groups obtained by running the bluefin_om_base.R code 
cr_wl= read.csv(paste(pdir,"crwl.csv", sep = ""))
cr_ws= read.csv(paste(pdir,"crws.csv", sep = ""))
cr_wm= read.csv(paste(pdir,"crwm.csv", sep = ""))
cr_epo= read.csv(paste(pdir,"crepo.csv", sep = ""))
cr_all = read.csv(paste(pdir,"crdat.csv", sep = ""))

#Set the harvest strategy
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

#Run the MSE code for 100 iterations
#rdev1 is the last recruitment deviation from year 2015 in the 2017 assessment model
#it is needed to compute the first random deviation with autocorrelation
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished dynamic SSB
#Blim is the limit reference point represting the fraction of unfished dynamic SSB
#Note that the F based TRP is specified already in the forecast file 
#the output is alredy saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file created for each iteration
foreach(itr = 1:25, .packages = c('r4ss','dplyr','reshape2')) %dopar% { PBF_MSE_hs1(hsnum,hcrnum,scnnum,itr, Bthr = 0.2, Blim = 0.077,sa=0,Fmin=0.05)}