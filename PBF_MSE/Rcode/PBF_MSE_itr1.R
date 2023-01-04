#North Pacific Bluefin tuna MSE code to run one iteration

#this runs the PBF MSE framework for 30 years for the specified iteration
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

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=15

#Set the scenario
scnnum=1

#Set the iteration
itr=1

#Run the MSE code for 1 iteration
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point representing the fraction of unfished SSB
#Note that the F based TRP is specified already in the forecast file 
#the output is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file

PBF_MSE_hs1(hsnum,hcrnum,scnnum,itr, Bthr = 0.2, Blim = 0.077,sa=0,Fmin=0.05)