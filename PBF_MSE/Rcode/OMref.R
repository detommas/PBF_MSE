#Script to find average selectivity for each of the reference OM set
#This is from a model that was already fit given the update M, H, or L parameters
#And another run with a block for 2015-2022
#The OMfit directory is the directory where the fitted OM is, with time varying 

#load packages
library(r4ss)
library(tidyverse)
library(reshape2)
library(ggplot2)

#Set the directory where to load and then save the updated .ctl file 
fdir = "C:/Users/desiree.tommasi/Documents/Bluefin/2024_SAM/Adoptedmodel/2024sam_bio/OM1rq/"
bdir = "C:/Users/desiree.tommasi/Documents/Bluefin/2024_SAM/Adoptedmodel/2024sam_bio/OM1rq1522/"

ctl_in = paste(fdir, "ctl_PBF_2024_0309_BC.ss", sep = "")
ctl_out = paste(bdir, "ctl_PBF_2024_0309_BC.ss", sep="")

#load the fitted ctl file with same blocks as SAM
ctl = readLines(ctl_in, warn = FALSE)

pattern = "# begin and end years of blocks"

#change block to 2015-2022
#Change block 1
which.line = grep(pattern=pattern, x=ctl)+1
blk1.old = ctl[which.line]
blk1.new = gsub("2015 2016 2016 2017 2018 2019 2019 2020 2020 2021 2022", "2014 2015 2022", blk1.old)  
ctl[which.line] = blk1.new
#Change block 2
which.line = grep(pattern=pattern, x=ctl)+2
blk2.old = ctl[which.line]
blk2.new = gsub("2018 2019 2019 2020 2022", "2022", blk2.old) # replace last year of block 2 
ctl[which.line] = blk2.new
#block 3 stays the same
#Change block 4
which.line = grep(pattern=pattern, x=ctl)+4
blk4.old = ctl[which.line]
blk4.new = gsub("2015 2016 2016 2017 2017 2018 2018 2019 2019 2020 2020 2021 2021 2022 2022", "2022", blk4.old) 
ctl[which.line] = blk4.new
#block 5 stays the same
#block 6 doesn't change
#Change block 7
which.line = grep(pattern=pattern, x=ctl)+7
blk7.old = ctl[which.line]
blk7.new = gsub("2017 2018 2018 2019 2019 2020 2022", "2022", blk7.old) 
ctl[which.line] = blk7.new
#block 8 doesn't change

#change the number of blocks per pattern
which.line = grep(pattern=pattern, x=ctl)-1
blkp.old = ctl[which.line]
blkp.new = gsub("13 5 1 8 2 1 6 1", "9 3 1 1 2 1 3 1", blkp.old) 
ctl[which.line] = blkp.new

#remove the time-varying block parameters that are not needed anymore
ctl.new=ctl[-c(665:668,678:681,691:694,696:702,704:710,712:718,720:726,728:734,740:741,745:747,751:752,756:757)]

#write the new ctl file into the OM block folder
writeLines(ctl.new, con = ctl_out)

#check that the file looks ok
#transfer the starter, forecast, dat file, and executable from the OMfit folder and run with estimation

#####Change the PAR file###############################
#read the original par file to be modified
#the value for the last selectivity block will be changed to the 2015-2022 average
pfile_in = paste(fdir,"ss3.PAR",sep="")
#read the par file of the run with the 2015-2022 blocks to get the param to put in
pblk_in = paste(bdir,"ss3.PAR",sep="")
#set a folder where the output file will be saved

#read the original file to be changed
par.old = readLines(pfile_in, warn = FALSE)
par.new =par.old
#read the file with the 2015-2022 average values
par.b = readLines(pblk_in, warn = FALSE)
#read vector of lines to change
vl=c(529,555,581,597,613,629,645,661,675,687,697,707)
#read vector of lines with parameter to read in from 2015-2022 blk
vlb=c(521,539,557,559,561,563,565,567,577,583,589,595)

#substitute last block with average values, these will be kept constant in the MSE simulation
#only the end of the block will be changed
for (j in 1:length(vl)){
  par.new[vl[j]]=par.b[vlb[j]]
}

#write new file in a final folder
ldir = "C:/Users/desiree.tommasi/Documents/Bluefin/2024_SAM/Adoptedmodel/2024sam_bio/OM1rqf/"
par_out = paste(ldir, "ss3.PAR", sep="")
writeLines(par.new, con = par_out)

#move starter, ctl, forecast, dat, and executable from fdir
#run one last time with estimation from par file (change starter to reflect this)
#but keeping these last block parameters fixed (change to - phase in ctl file)
#now you have the .ctl, and par files to add to the OM folder