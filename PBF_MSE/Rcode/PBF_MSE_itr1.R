#code to run one iteration of North Pacific Bluefin tuna MSE code

#this runs the PBF MSE framework for 30 years for the specified iteration
#iterations differ in their random recruitment deviations 

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

#we are simulating 30 years into the future
#we assume the assessment occurs every 2 years, so there will be 15 assessment time steps.
#this can be changed to test the impact of having more/less frequent assessments
#below we specify when those happen in the 30 years time series
asmt_t = seq(1, 30, by=tasmt)

#import needed files
#catch ratios for 2017-2019 by fleet and season across all fleets or across each of the following fleet groups
# this are needed for this first simple HCR we are using, later will split by fisheries impact
#wl= Western Pacific large fish, ws= Western Pacific small fish, epo = eastern Pacific
#the fleets were assigned as follows to the different groups, please check
#wl = 1,4,10:12, 16,17,28
#ws = 2,3,5:9, 18:20, 26,27
#epo = 13:15,29,30
cr_wl= read.csv(paste(pdir,"crwl.csv", sep = ""))
cr_ws= read.csv(paste(pdir,"crws.csv", sep = ""))
cr_epo= read.csv(paste(pdir,"crepo.csv", sep = ""))
cr_all = read.csv(paste(pdir,"crdat.csv", sep = ""))

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum=1

#Set the scenario
scnnum=1

#Set the iteration
itr=1

#Run the MSE code for 1 iteration
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point representing the fraction of unfished SSB
#Note that the F based TRP is specified already in the forecast file, which is the hcr folder .../PBF_MSE/1/1
#the output of SS is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file

PBF_MSE_hs1(hsnum,hcrnum,scnnum,itr, Bthr = 0.2, Blim = 0.077)