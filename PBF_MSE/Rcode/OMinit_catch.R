#Code to compute the initial catch for fishing years 2023 and 2024 and half of 2025
#for the OM before the first TAC is applied
#the first TAC is for Cal years 2026, 2027, 2028 (so applied starting FY2025 seas 3,4)
library(r4ss)
library(tidyverse)
library(reshape2)
library(ggplot2)

#base model with all input catch in biomass
bio24dir="C:/Users/desiree.tommasi/Documents/Bluefin/2024_SAM/Adoptedmodel/2024sam_bio"
bio24out=SS_output(bio24dir)

#Extract the catch in biomass from the catch section of the report file for the above fleets
exom = bio24out$catch

#Fishing year 2023 catch will equal the Cal year 2023 July-Dec catch submitted by each country,
#while we wait for data, here we set it to 2022 values
c22= exom%>%filter(Yr==2022)

#create a data frame for input into om data file
# num of rows is #of fleets * #of seas (26*4=104)
c23om = data.frame(year=rep(2023,104),seas=c22$Seas,fleet=c22$Fleet,catch=c22$sel_bio,catch_se=rep(0.1,104))

#the FY 2024 catches will be set equal to the CMM for 2024 limit for seas 1 and 2
#and to the CMM for 2025 limit for seas 3 and 4
#the CMM based catch limits are by member country for a calendar year
#and for WPO countries also by large and small size
#however, the OM needs input by fleet, and season, and is run by fishing year

#Use most recent catch ratios (FY 2022) to translate CMM to catch
#WCPFC CMM 2023-02 for cal year 2024
js24=4007 # Japanese small fish - OM fleets 8-10,12-16
jl24=5614 #Japanese large fish - OM fleets 1, 2, 5-7
ks24=718 #Korea small fish - OM fleet 11
kl24=30 #Korea large fish - OM fleet 11 (OM has only 1 Korean fleet)
ct24=1965 #Chinese Taipei large fish - OM fleets 3,4
#IATTC CMM C-21-05
epoc24=3925 #epo commercial - OM fleet 20,21

#summer 2024 cmm for cal year 2025
js25=4407 # Japanese small fish - OM fleets 8-10,12-16
jl25=8421 #Japanese large fish - OM fleets 1, 2, 5-7
ks25=718 #Korea small fish - OM fleet 11
kl25=501 #Korea large fish - OM fleet 11 (OM has only 1 Korean fleet)
ct25=2947 #Chinese Taipei large fish - OM fleets 3,4
epoc25=7581 #epo commercial - OM fleet 20,21

#fleets 17-19 cannot be directly assigned to the above as they catch 
# a mix of small and large fish
#check out how much of the total small/large fish catch was from these fleets

#select the catch at age from 2022 and remove column with duplicate names
catcha = bio24out$catage[,-c(4,5,10)] %>% filter(Yr==2022)
#extract body weight at age
bwmat=bio24out$ageselex%>%filter(Factor=="bodywt"&Yr==2022)

catchab=catcha
  for(f in 1:26){
    for (s in 1:4){
      for (a in 0:20){
        catchab[catchab$Fleet==f&catchab$Seas==s,9+a]=bwmat[bwmat$Fleet==f&bwmat$Seas==s,8+a]*catcha[catcha$Fleet==f&catcha$Seas==s,9+a]
      }
    }
  }

#compute total Japanese small fish catch (<3 years old) from small and and mixed fleets
csmdat=catchab %>% filter(Fleet %in% c(8:10,12:16,17:19))
csm=sum(csmdat$`0`,csmdat$`1`,csmdat$`2`)
#compute total catch of small fish from mixed as fraction of total small fish catch
cmdat=catchab %>% filter(Fleet %in% c(17:19))
cmp=sum(cmdat$`0`,cmdat$`1`,cmdat$`2`)/csm #about 20.1% of small fish catch

#compute total Japanese large fish catch (>=3 years old) from large and and mixed fleets
clmdat=catchab %>% filter(Fleet %in% c(1,2,5:7,17:19))
clm=sum(clmdat[,12:29])
#compute total catch of small fish from mixed as fraction of total small fish catch
cmpl=sum(cmdat[,12:29])/clm #about 22.9% of large fish catch

#compute limit for Japanese mixed fleet
jm24=js24*cmp+jl24*cmpl
jm25=js25*cmp+jl25*cmpl
#small and large catch limits accounting for mixed fleet catches
js242 = js24-js24*cmp
js252 = js25-js25*cmp
jl242 = jl24-jl24*cmpl
jl252 = jl25-jl25*cmpl

#Note the EPO rec fleet is not subject to a catch limit, rec catches 
#and associated discards, fleets 22,23, 26, will be set 
#equal to 2022 (2023 once data will be available)
#Discards for the WPO fleet will be set to
#5% of total jpn and Korean catch except penning fleets for fleet 24
#5% of catch for penning (fleets 7,14) plus catches of fleet 10 - CHECK

#compute average catch ratio for WPO small fish, WPO large, WPO Mix and EPOcomm

#add a column to specify fleet type
exomf = exom
exomf$Ftype = "Nocmm"

test = exomf %>%
  mutate(Ftype = case_when(
    Fleet %in% c(20:21) ~ "EPOc",
    Fleet %in% c(8:10,12:16) ~ "jpns",
    Fleet %in% c(11) ~ "kr",
    Fleet %in% c(1,2,5:7) ~ "jpnl",
    Fleet %in% c(3,4) ~ "cht",
    Fleet %in% c(17:19) ~ "jpnm",
    Fleet == 24 ~ "jpnd",
    Fleet == 25 ~ "jpndf",
    Fleet %in% c(22,23,26) ~ "EPOr"))

#generate table of total catches per fleet per season for each of the fleet groups
ctjl = as.data.frame(test %>% filter(Yr==2022&Ftype=="jpnl") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctjs = as.data.frame(test %>% filter(Yr==2022&Ftype=="jpns") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctjm = as.data.frame(test %>% filter(Yr==2022&Ftype=="jpnm") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctjd = as.data.frame(test %>% filter(Yr==2022&Ftype=="jpnd") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctjdf = as.data.frame(test %>% filter(Yr==2022&Ftype=="jpndf") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctcht = as.data.frame(test %>% filter(Yr==2022&Ftype=="cht") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctkr = as.data.frame(test %>% filter(Yr==2022&Ftype=="kr") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))
ctepoc = as.data.frame(test %>% filter(Yr==2022&Ftype=="EPOc") %>% group_by(Fleet,Seas) %>% summarize(catch=mean(sel_bio)))

#calculate catch total for each group
ctotjl=sum(ctjl$catch)
ctotjs=sum(ctjs$catch)
ctotjm=sum(ctjm$catch)
ctotjd=sum(ctjd$catch)
ctotjdf=sum(ctjdf$catch)
ctotcht=sum(ctcht$catch)
ctotkr=sum(ctkr$catch)
ctotepoc=sum(ctepoc$catch)

#calculate catch ratio
ctjl$cratio = ctjl$catch/ctotjl
ctjs$cratio = ctjs$catch/ctotjs
ctjm$cratio = ctjm$catch/ctotjm
ctjd$cratio = ctjd$catch/ctotjd
ctjdf$cratio = ctjdf$catch/ctotjdf
ctcht$cratio = ctcht$catch/ctotcht
ctkr$cratio = ctkr$catch/ctotkr
ctepoc$cratio = ctepoc$catch/ctotepoc

#calculate 2024 catch
ctjl$c24 = ctjl$cratio*jl242
ctjs$c24 = ctjs$cratio*js242
ctjm$c24 = ctjm$cratio*jm24
ctcht$c24 = ctcht$cratio*ct24
ctkr$c24 = ctkr$cratio*(ks24+kl24)
ctepoc$c24 = ctepoc$cratio*epoc24

#calculate 2025 catch
ctjl$c25 = ctjl$cratio*jl252
ctjs$c25 = ctjs$cratio*js252
ctjm$c25 = ctjm$cratio*jm25
ctcht$c25 = ctcht$cratio*ct25
ctkr$c25 = ctkr$cratio*(ks25+kl25)
ctepoc$c25 = ctepoc$cratio*epoc25

#Join in large data frame
ct2425=rbind(ctjl,ctjs,ctjm,ctcht,ctkr,ctepoc)

#calculate total jpn/kr discards
ctjd$c24=(sum((ct2425 %>% filter(Fleet %in% c(1,2,5:6,8:9,11:13,15:19)))$c24)*0.05)*ctjd$cratio
ctjdf$c24=(sum((ct2425 %>% filter(Fleet %in% c(7,14)))$c24)*0.05+sum((ct2425 %>% filter(Fleet ==10))$c24))*ctjdf$cratio
ctjd$c25=(sum((ct2425 %>% filter(Fleet %in% c(1,2,5:6,8:9,11:13,15:19)))$c25)*0.05)*ctjd$cratio
ctjdf$c25=(sum((ct2425 %>% filter(Fleet %in% c(7,14)))$c25)*0.05+sum((ct2425 %>% filter(Fleet ==10))$c25))*ctjdf$cratio

#create large data frame with discards
cmat = rbind(ctjl,ctjs,ctjm,ctcht,ctkr,ctepoc,ctjd,ctjdf)

#create a rec fleet data frame, same as FY2022
rec = data.frame(Fleet=c(rep(22,4),rep(23,4),rep(26,4)),Seas=rep(c(1:4),3),catch=rep(NA,12),cratio=rep(NA,12),c24=c23om$catch[c23om$fleet %in% c(22,23,26)],c25=c23om$catch[c23om$fleet %in% c(22,23,26)])
#add the rec fleets set same as om23
cmat2 = rbind(cmat,rec)
#order by fleet and season
cmat3 = cmat2[order(cmat2$Fleet, cmat2$Seas),]

#Parsing the catches out to the different FYs
#2023FY will have obs catches for S1,S2 and those based on CMM for 2024 for S3 and 4
#2024FY will have CMM for 2024 for s1,2 and CMM for 2025 for S3,4
#2025FY will have cMM for 2024 for s1,2 and then first TAC from within sim for S3,4

#add 2024 cmm catch for s 3,4 of FY2023 catches
c23om2=c23om
c23om2$catch[c23om2$seas%in%c(3,4)]=cmat3$c24[cmat3$Seas%in%c(3,4)]

c24om1 = data.frame(year=rep(2024,dim(cmat3)[1]),
                   seas=c((cmat3%>%filter(Seas%in%c(1,2)))$Seas,(cmat3%>%filter(Seas%in%c(3,4)))$Seas),
                   fleet=c((cmat3%>%filter(Seas%in%c(1,2)))$Fleet,(cmat3%>%filter(Seas%in%c(3,4)))$Fleet),
                   catch=c((cmat3%>%filter(Seas%in%c(1,2)))$c24,(cmat3%>%filter(Seas%in%c(3,4)))$c25),
                   catch_se=rep(0.1,dim(cmat3)[1]))

#order by fleet and season
c24om = c24om1[order(c24om1$fleet, c24om1$seas),]

ciom=rbind(c23om2,c24om)

#Calculate catch for initial FY 2025 (seas 1,2, cal year), which will not be the same as 2024, 
c25om1 = data.frame(year=rep(2025,(dim(cmat3)[1])/2),
                    Seas=(cmat3%>%filter(Seas%in%c(1,2)))$Seas,
                    fleet=(cmat3%>%filter(Seas%in%c(1,2)))$Fleet,
                    catch=(cmat3%>%filter(Seas%in%c(1,2)))$c25,
                    catch_se=rep(0.1,(dim(cmat3)[1])/2))

#order by fleet and season
c25om = c25om1[order(c25om1$fleet, c25om1$Seas),]

#save text file
write.csv(ciom,"C:/Users/desiree.tommasi/Documents/Bluefin/2024baseOM/ciom.csv")
write.csv(c25om,"C:/Users/desiree.tommasi/Documents/Bluefin/2024baseOM/ciom25.csv")

#find the total TAC (no discards) by calendar year and fleeet type
tac23 = sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(1:23)])
tac24 = sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(1:23)])+
  sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(1:23)])
tac25 = sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(1:23)])+
  sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(1:23)])

#small fish TAc for WPO, note prop small of mixed fleet same as FY2022 ~ 36%
tac23ws = sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(8:10,12:16)])+
  0.357521*sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(11,17:19)])
tac24ws = sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(8:10,12:16)])+
  sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(8:10,12:16)])+
  0.357521*sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(11,17:19)])+
  0.357521*sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(11,17:19)])
tac25ws = sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(8:10,12:16)])+
  sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(8:10,12:16)])+
  0.357521*sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(11,17:19)])+
  0.357521*sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(11,17:19)])

#small fish TAc for WPO, note prop large of mixed fleet same as FY2022 ~ 64%
tac23wl = sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(1:7)])+
  0.642479*sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(11,17:19)])
tac24wl = sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(1:7)])+
  sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(1:7)])+
  0.642479*sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(11,17:19)])+
  0.642479*sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(11,17:19)])
tac25wl = sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(1:7)])+
  sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(1:7)])+
  0.642479*sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(11,17:19)])+
  0.642479*sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(11,17:19)])

#find the EPO TAC (no discards) by calendar year 
tac23e = sum(ciom$catch[ciom$year==2023&ciom$fleet %in% c(20:23)])
tac24e = sum(ciom$catch[ciom$year==2023&ciom$seas%in%c(3,4)&ciom$fleet %in% c(20:23)])+
  sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(1,2)&ciom$fleet %in% c(20:23)])
tac25e = sum(ciom$catch[ciom$year==2024&ciom$seas%in%c(3,4)&ciom$fleet %in% c(20:23)])+
  sum(ciom25$catch[ciom25$year==2025&ciom25$Seas%in%c(1,2)&ciom25$fleet %in% c(20:23)])
