#Wrapper code to extract impact performance metrics indicators saved for each
#hcr as a text file
# Author: D. Tommasi

library(foreach)
library(doParallel)
library(r4ss)
library(dplyr)
library(reshape2)

#Specify parent directories path 
pdir = "J:/Desiree/Bluefin/PBF_MSE/"

#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#Calculate the numbers of cores 
no_cores = detectCores() - 1

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#Run the MSE code for specified number of iterations
#Bthr is the SSB based biomass reference point. It represents the fraction of unfished SSB
#Blim is the limit reference point represting the fraction of unfished SSB
#Note that the F based TRP is specified already in the forecast file 
#the output is already saved as the code runs in the respective folders
#main output to then compute performance metrics is the outlist.text file created for each iteration
foreach(hcrn = 1:50, .packages = c('r4ss','dplyr','reshape2')) %dopar% { impact_pbfMSE(pdir=pdir, hsn=1, hcrn,scnn=1)}

stopCluster(cl)