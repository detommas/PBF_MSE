#' Computes catch per fleet given the terminal year numbers at age, the F multiplier, biology and exploitation pattern in the forecast report file
#'
#' @param ssout SS-output file
#' @param yearb years over which to select biology for the catch advice calculation
#' @param yearf years over which to select relF and selectivity for the catch advice calculation
#' @param ben forecast report file from which to read in benchmark calculation information
#' @return catch by fleet and season dataframe
#' @author Desiree Tommasi

catch_calc <- function(ssout,yearsb,yearsf,ben,fmult){
  
  #Extract natural mortality
  Maa=ssout$Natural_Mortality_Bmark
  Maa_long = melt(Maa[,4:25], id.vars=c("Seas"),variable.name = "Age",value.name = "Ma")
  
  #extract weight at age
  Waamat=ssout$wtatage %>% filter(Yr %in% c(yearsb)) #per season and fleet
  Waa_long = melt(Waamat, id.vars=c("Yr", "Seas", "Sex", "Bio_Pattern", "BirthSeas","Fleet"),variable.name = "Age",value.name = "waa")
  Waa=Waa_long %>% filter(Fleet %in% c(1:20,26:30)) %>% group_by(Seas,Sex, Bio_Pattern, BirthSeas, Fleet, Age) %>% summarize(
    Yr=mean(Yr),
    waa=mean(waa)
  )
  
  #Extract Age selectivity at age
  Saa=ssout$ageselex %>% filter(Yr %in% c(yearsf)&Factor=="Asel2"&Fleet %in% c(1:20,26:30)) #also for surveys
  Saa_long = melt(Saa, id.vars=c("Factor", "Fleet", "Yr","Seas","Sex", "Morph", "Label"),variable.name = "Age",value.name = "Sa")
  Sdat_long = Saa_long %>% group_by(Factor, Fleet, Seas, Sex, Morph, Age) %>% summarize(
    Yr=mean(Yr),
    Sa=mean(Sa)
  )
  
  #extract the relative F for each season
  relfs1=ben[6]
  relfs1 = gsub("^\\s+|\\s+$", "", relfs1) # remove leading blank
  relfs1 = gsub("\\s+", " ", relfs1)       # remove >1 blanks
  relfs1 = as.numeric(unlist(strsplit(relfs1, split= " ")))
  
  relfs2=ben[7]
  relfs2 = gsub("^\\s+|\\s+$", "", relfs2) # remove leading blank
  relfs2 = gsub("\\s+", " ", relfs2)       # remove >1 blanks
  relfs2 = as.numeric(unlist(strsplit(relfs2, split= " ")))
  
  relfs3=ben[8]
  relfs3 = gsub("^\\s+|\\s+$", "", relfs3) # remove leading blank
  relfs3 = gsub("\\s+", " ", relfs3)       # remove >1 blanks
  relfs3 = as.numeric(unlist(strsplit(relfs3, split= " ")))
  
  relfs4=ben[9]
  relfs4 = gsub("^\\s+|\\s+$", "", relfs4) # remove leading blank
  relfs4 = gsub("\\s+", " ", relfs4)       # remove >1 blanks
  relfs4 = as.numeric(unlist(strsplit(relfs4, split= " ")))
  
  Afdat = data.frame(Fleet=rep(c(1:20,26:30),4), Seas=c(rep(1,25),rep(2,25),rep(3,25),rep(4,25)), Af=c(relfs1[c(1:20,26:30)],relfs2[c(1:20,26:30)],relfs3[c(1:20,26:30)],relfs4[c(1:20,26:30)]))
  Afdat$Afm=Afdat$Af*fmult
  Fdat=as.data.frame(Sdat_long)
  Fdat$Fa=0
  ages=0:20
  fleets = c(1:20,26:30)
  
  #Compute f at age (apical F x sel) for all fleets in PBF
  for (a in 1:21){
    for (fl in 1:length(fleets)){
      for (s in 1:4){
        Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==s&Fdat$Age==ages[a]]=Sdat_long$Sa[Sdat_long$Fleet==fleets[fl]&Sdat_long$Seas==s&Sdat_long$Age==ages[a]]*Afdat$Afm[Afdat$Seas==s&Afdat$Fleet==fleets[fl]]
      }
    }
  }
  
  #Extract terminal numbers at age
  Ndat=ssout$natage[,c(8:9,11,13:33)] 
  names(Ndat)[3]="Period"
  Ndat=Ndat%>% filter(Yr==ssout$endyr&Period=="B")
  Ndat_long = melt(Ndat, id.vars=c("Yr","Seas","Period"),variable.name = "Age",value.name = "Na")
  
  #calculate catch at age by fleet
  Cfage = Fdat
  Cfage$catch=0
  Cfage$yield=0
  
  #need to multiply by M and Z
  
  for (a in 1:21){
    for (fl in 1:length(fleets)){
      for (s in 1:4){
        Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==s&Cfage$Age==ages[a]]=(((Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==s&Fdat$Age==ages[a]]/
                                                                                   (Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==s&Fdat$Age==ages[a]]+
                                                                                      0.25*(Maa_long$Ma[Maa_long$Seas==s&Maa_long$Age==ages[a]])))*Ndat_long$Na[Ndat_long$Seas==s&Ndat_long$Age==ages[a]])*
                                                                                 (1-exp(-0.25*(Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==s&Fdat$Age==ages[a]]+0.25*(Maa_long$Ma[Maa_long$Seas==s&Maa_long$Age==ages[a]])))))
        Cfage$yield[Cfage$Fleet==fleets[fl]&Cfage$Seas==s&Cfage$Age==ages[a]]=Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==s&Cfage$Age==ages[a]]*Waa$waa[Waa$Fleet==fleets[fl]&Waa$Seas==s&Waa$Age==ages[a]]
        
      }
    }
  }
  
  #sum yield over ages for each fleet and season
  Cfdat = as.data.frame(Cfage %>% group_by(Fleet, Seas) %>% summarize(yield=sum(yield)))
  
  return(Cfdat)
}
