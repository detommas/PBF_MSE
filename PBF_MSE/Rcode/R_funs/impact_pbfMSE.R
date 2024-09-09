#' Re-runs the last OM for each of the 100 itr of a specified hcr of the 
#' bluefin MSE to calculate the proportional impact 
#' and saves the output in a text file
#' @param pdir the parent directory path
#' @param hsn the harvest strategy being run
#' @param hcrn the hcr being run
#' @param scnn the om scenario being run
#' @return impact data frame with the the proportional impact for each grouping
#' @author Desiree Tommasi

impact_pbfMSE <- function(pdir, hsn, hcrn,scnn){

#specify the HS that is being run
hsnum = hsn
hs = paste(hsnum, "/", sep = "")

#specify the HCR that is being run
hcrnum = hcrn
hcr = paste(hcrnum, "/", sep = "")

#Specify the scenario being run
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

for (itr in 1:100){
  itr_d= paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OM",sep = "")
  imat = impact_calc_h1(Dir=itr_d, ssDir=ssdir)
  imat$itr=itr
  idat[indx[itr]:(indx[itr]+62),]=imat
}

#save output to file
write.table(idat, paste(pdir,hs,hcr, scn, "/impact",hsnum,hcrnum,snum,".txt", sep =""))

}