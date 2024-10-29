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

#pdir = "D:/Desiree/PBF_MSE/"
pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/PBF_MSE-main2/PBF_MSE/"
#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#Specify the path of conditioned initial OM
#sdir = "D:/Desiree/PBF_MSE/Condition/"
sdir = "C:/Users/desiree.tommasi/Documents/Bluefin/Github/PBF_MSE-main2/PBF_MSE/Condition/"

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
#catch ratios by fleet and season for each of the fleet groups obtained by running the bluefin_om_base.R code 
ciom= read.csv(paste(pdir,"ciom.csv", sep = ""))
ciom25= read.csv(paste(pdir,"ciom25.csv", sep = ""))
#import minimum TAc, which for this hcr is set by the cmm
cmin8= read.csv(paste(pdir,"cmin8.csv", sep = ""))

#read selectivity deviations
#sdev= read.csv(paste(pdir,"sdev1720.csv", sep = ""))
#sdev= read.csv(paste(pdir,"sdev1722.csv", sep = ""))
sdev= read.csv(paste(pdir,"sdev1522.csv", sep = ""))

#Set the harvest strategy
hsnum = 1
#Set the HCR
hcrnum= 8

#Set the scenario
scnnum=1

#Calculate the numbers of cores 
no_cores = detectCores() - 92

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Run the MSE code for specified number of iterations
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point represting the fraction of unfished SSB
#Note that the F based TRP is specified already in the forecast file 
#the output is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file created for each iteration
foreach(itr = 1:100, .packages = c('r4ss','dplyr','reshape2', 'tidyverse')) %dopar% { PBF_MSE_hs1_hcr8_for_sam24_ncmm(hsnum,hcrnum,scnnum,itr, Bthr = 0.20,sa=0, lag=1, obse=2, aspm = NULL, yfor = c(2015:2022), tacl=25)}

stopCluster(cl)