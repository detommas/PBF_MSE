#' Computes catch per fleet given the terminal year numbers at age, the F multiplier, biology and exploitation pattern in the forecast report file
#'
#' @param ssout SS-output file
#' @param yearb years over which to select biology for the catch advice calculation
#' @param yearf years over which to select relF and selectivity for the catch advice calculation
#' @param ben forecast report file from which to read in benchmark calculation information
#' @param fmult fmultiplier to meet specified FSPR target from forecast report
#' @param ffraction reduction in F from fmult as determined by hcr, if 1 no reduction
#' @return catch by fleet and season dataframe
#' @author Desiree Tommasi

catch_calc_f <- function(ssout,yearsb,yearsf,ben,fmult,ffraction){
  
  #Extract natural mortality
  Maa=ssout$Natural_Mortality_Bmark
  Maa_long = melt(Maa[,4:25], id.vars=c("Seas"),variable.name = "Age",value.name = "Ma")
  
  #extract weight at age
  Waamat=ssout$ageselex %>% filter(Yr %in% c(yearsb)&Factor=="bodywt"&Fleet %in% c(1:20,26:30)) #per season and fleet
  Waa_long = melt(Waamat, id.vars=c("Factor", "Fleet", "Yr","Seas","Sex", "Morph", "Label"),variable.name = "Age",value.name = "waa")
  Waa = Waa_long %>% group_by(Factor, Fleet, Seas, Sex, Morph, Age) %>% summarize(
    Yr=mean(Yr),
    waa=mean(waa)
  )
  
  #Extract selectivity at age
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
  Afdat$Afm=Afdat$Af*fmult*ffraction
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
  
  #compute total F by age and season
  Ftotmat = Fdat %>% group_by(Age, Seas)%>%summarize(
    Ftota = sum(Fa)
  )
  
  #compute Z at age
  Maa_long$Za = 1
  for (a in 1:21){
    for (s in 1:4){
      Maa_long$Za[Maa_long$Age==ages[a]&Maa_long$Seas==s]=
        Maa_long$Ma[Maa_long$Age==ages[a]&Maa_long$Seas==s]+
        Ftotmat$Ftota[Ftotmat$Age==ages[a]&Ftotmat$Seas==s]
    }
  }
  
  #Extract terminal year numbers at age to compute 
  #the starting number at age for the forecast period
  Ndatyt=ssout$natage[,c(8:9,11,13:33)] 
  names(Ndatyt)[3]="Period"
  Ndatyt=Ndatyt%>% filter(Yr==ssout$endyr&Period=="B"&Seas==1)
  Ndatyt_long = melt(Ndatyt, id.vars=c("Yr","Seas","Period"),variable.name = "Age",value.name = "Na")
  
  #Extract Z at age for the end year
  Zyt = ssout$Z_at_age %>% filter(Yr==ssout$endyr)
  Zyt_long = melt(Zyt, id.vars=c("Yr","Bio_Pattern","Sex"),variable.name = "Age",value.name = "Za")
  #Set the oldest age Z the same as the previous - not avialble in ss
  Zyt_long$Za[21]=Zyt_long$Za[20]
  
  #Calculate the numbers at age at the start of the forecast
  Ndatfs1_long = Ndatyt_long
  Ndatfs1_long$Yr = ssout$endyr+1
  Ndatfs1_long$Seas = 1
  
  for (a in 2:20){
    Ndatfs1_long$Na[Ndatfs1_long$Age==ages[a]]=
      Ndatyt_long$Na[Ndatyt_long$Age==ages[a-1]]*
      exp(-(Zyt_long$Za[Zyt_long$Age==ages[a-1]]))
  }
  
  #calculation for the plus group
  Ndatfs1_long$Na[Ndatfs1_long$Age==ages[21]]=
    Ndatyt_long$Na[Ndatyt_long$Age==ages[20]]*
    exp(-(Zyt_long$Za[Zyt_long$Age==ages[20]]))+
    Ndatyt_long$Na[Ndatyt_long$Age==ages[21]]*
    exp(-(Zyt_long$Za[Zyt_long$Age==ages[21]]))
  
  #age 0 will be the recruits spawned in season 4 of end year
  Ndatfs1_long$Na[Ndatfs1_long$Age==ages[1]]=
    ssout$sprseries$Recruits[ssout$sprseries$Yr==ssout$endyr]
  
  #calculate catch at age by fleet for forecast s1
  Cfage = Fdat
  Cfage$catch=0
  Cfage$yield=0
  Cfage$Yr=ssout$endyr+1
  
  #Calculate catch at age for s1 given Z, N, and F for each fleet
  for (a in 1:21){
    for (fl in 1:length(fleets)){
        Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==1&Cfage$Age==ages[a]]=
          ((Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==1&Fdat$Age==ages[a]]/
               Maa_long$Za[Maa_long$Seas==1&Maa_long$Age==ages[a]])*
          Ndatfs1_long$Na[Ndatfs1_long$Age==ages[a]])*
          (1-exp(-0.25*Maa_long$Za[Maa_long$Seas==1&Maa_long$Age==ages[a]]))
        Cfage$yield[Cfage$Fleet==fleets[fl]&Cfage$Seas==1&Cfage$Age==ages[a]]=
          Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==1&Cfage$Age==ages[a]]*
          Waa$waa[Waa$Fleet==fleets[fl]&Waa$Seas==1&Waa$Age==ages[a]]
    }
  }
  
  #Calculate the numbers at age in s2 of the forecast
  Ndatfs2_long = Ndatfs1_long
  Ndatfs2_long$Seas = 2
  
  for (a in 1:21){
    Ndatfs2_long$Na[Ndatfs2_long$Age==ages[a]]=
      Ndatfs1_long$Na[Ndatfs1_long$Age==ages[a]]*
      exp(-0.25*(Maa_long$Za[Maa_long$Age==ages[a]&Maa_long$Seas==1]))
  }
  
  #Calculate catch at age for s2 given Z, N, and F for each fleet
  for (a in 1:21){
    for (fl in 1:length(fleets)){
      Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==2&Cfage$Age==ages[a]]=
        ((Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==2&Fdat$Age==ages[a]]/
            Maa_long$Za[Maa_long$Seas==2&Maa_long$Age==ages[a]])*
           Ndatfs2_long$Na[Ndatfs2_long$Age==ages[a]])*
        (1-exp(-0.25*Maa_long$Za[Maa_long$Seas==2&Maa_long$Age==ages[a]]))
      Cfage$yield[Cfage$Fleet==fleets[fl]&Cfage$Seas==2&Cfage$Age==ages[a]]=
        Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==2&Cfage$Age==ages[a]]*
        Waa$waa[Waa$Fleet==fleets[fl]&Waa$Seas==2&Waa$Age==ages[a]]
    }
  }
  
  #Calculate the numbers at age in s3 of the forecast
  Ndatfs3_long = Ndatfs2_long
  Ndatfs3_long$Seas = 3
  
  for (a in 1:21){
    Ndatfs3_long$Na[Ndatfs3_long$Age==ages[a]]=
      Ndatfs2_long$Na[Ndatfs2_long$Age==ages[a]]*
      exp(-0.25*(Maa_long$Za[Maa_long$Age==ages[a]&Maa_long$Seas==2]))
  }
  
  #Calculate catch at age for s3 given Z, N, and F for each fleet
  for (a in 1:21){
    for (fl in 1:length(fleets)){
      Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==3&Cfage$Age==ages[a]]=
        ((Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==3&Fdat$Age==ages[a]]/
            Maa_long$Za[Maa_long$Seas==3&Maa_long$Age==ages[a]])*
           Ndatfs3_long$Na[Ndatfs3_long$Age==ages[a]])*
        (1-exp(-0.25*Maa_long$Za[Maa_long$Seas==3&Maa_long$Age==ages[a]]))
      Cfage$yield[Cfage$Fleet==fleets[fl]&Cfage$Seas==3&Cfage$Age==ages[a]]=
        Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==3&Cfage$Age==ages[a]]*
        Waa$waa[Waa$Fleet==fleets[fl]&Waa$Seas==3&Waa$Age==ages[a]]
    }
  }
  
  #Calculate the numbers at age in s4 of the forecast
  Ndatfs4_long = Ndatfs3_long
  Ndatfs4_long$Seas = 4
  
  for (a in 1:21){
    Ndatfs4_long$Na[Ndatfs4_long$Age==ages[a]]=
      Ndatfs3_long$Na[Ndatfs3_long$Age==ages[a]]*
      exp(-0.25*(Maa_long$Za[Maa_long$Age==ages[a]&Maa_long$Seas==3]))
  }
  
  #Calculate catch at age for s4 given Z, N, and F for each fleet
  for (a in 1:21){
    for (fl in 1:length(fleets)){
      Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==4&Cfage$Age==ages[a]]=
        ((Fdat$Fa[Fdat$Fleet==fleets[fl]&Fdat$Seas==4&Fdat$Age==ages[a]]/
            Maa_long$Za[Maa_long$Seas==4&Maa_long$Age==ages[a]])*
           Ndatfs4_long$Na[Ndatfs4_long$Age==ages[a]])*
        (1-exp(-0.25*Maa_long$Za[Maa_long$Seas==4&Maa_long$Age==ages[a]]))
      Cfage$yield[Cfage$Fleet==fleets[fl]&Cfage$Seas==4&Cfage$Age==ages[a]]=
        Cfage$catch[Cfage$Fleet==fleets[fl]&Cfage$Seas==4&Cfage$Age==ages[a]]*
        Waa$waa[Waa$Fleet==fleets[fl]&Waa$Seas==4&Waa$Age==ages[a]]
    }
  }
  
  #sum yield over ages for each fleet and season
  Cfdat = as.data.frame(Cfage %>% group_by(Fleet, Seas) %>% summarize(yield=sum(yield)))
  
  return(Cfdat)
}
