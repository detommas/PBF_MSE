#Combine outputs from all iterations in one dataframe for each HCR

library(r4ss)

#specify the harvest strategy (HCR shape) that is being run
hsnum = 1
hs = paste(hsnum, "/", sep = "")

#specify the HCRs that were run
hcrnumv = c(1:8,11,12)

#Specify the scenario being run
snum = 1
scn = paste(snum, "/", sep ="")

#Specify parent directories path 
pdir= "J:/Desiree/Bluefin/PBF_MSE/"
#pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/"
#pdir = "C:/Users/FRDScientist/Documents/Desiree/MSE_2/MSE_ALB/"

for (j in 1:length(hcrnumv)){
  
  #specify the hcr to extract
  hcrnum = hcrnumv[j]
  hcr = paste(hcrnum, "/", sep = "")
  
  #Extract results from each iteration and add to the same matrix
  itrn =NA
  
  for (itr in 1:100){
    #check what is in the working directory
    itr_d= paste(pdir, hs, hcr, scn, itr, sep = "")
    
    itr_f = list.files(itr_d)
    
    #only extract data if EM completed
    
    if ("outlist.txt" %in% itr_f == TRUE){
      itrn = c(itrn,itr)
    }
  }
  
  itrn = itrn[-1]
  ind = seq(1,(length(itrn)*24), by = 24)
  smat = matrix(NaN, nrow = (length(itrn)*24), ncol = 48)
  smat = as.data.frame(smat)
  
  
  for (itr in 1:length(itrn)) {
    for (c in 1:47){
      #import simulation data fo reach iteration
      dat= read.table(paste(pdir,hs, hcr, scn, itrn[itr],"/outlist.txt", sep =""))
      smat[ind[itr]:(ind[itr]+23), ] = dat
    }
    #add a column to specify the iteration
    smat[ind[itr]:(ind[itr]+23), 48] = rep(itrn[itr], 24)
  }
  
  names(smat) = c(names(dat),"itr")
  
  #save output to file
  write.table(smat, paste(pdir,hs,hcr, scn, "/outmat",hsnum,hcrnum,snum,".txt", sep =""))
  
}
