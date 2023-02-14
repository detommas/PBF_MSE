#Calculates impact performance metric for Yield Management objective
#Author: D. Tommasi
library(r4ss)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(fitdistrplus)

#Specify path of parent directory
pdir = "C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/"

#set working directory to where all functions needed are stored
setwd(paste(pdir,"Rcode/R_funs", sep = ""))

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#specific the harvest strategy, hcrs to compare, and scenario
hsnum=1
hcrnumv = c(1:50)
scnum = 1

hs = paste(hsnum, "/", sep = "")
scn = paste(scnum, "/", sep = "")

#potential threshold, limit, and target reference points
btv=c(rep(0.15,4),rep(0.2,3),0.25,0.25,rep(0.15,4),rep(0.2,3),rep(0.25,2),rep(0.2,3),rep(0.25,4),rep(0.15,4),rep(0.2,3),0.25,0.25,rep(0.15,4),rep(0.2,3),rep(0.25,2),rep(0.2,3),rep(0.25,4))
blv=c(rep(0.05,9),rep(0.077,9),rep(0.15,5),rep(0.2,2),rep(0.05,9),rep(0.077,9),rep(0.15,5),rep(0.2,2))
trpv = c(0.15,0.2,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.15,0.2,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.3,0.4,0.15,0.2,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.15,0.2,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.2,0.3,0.4,0.3,0.4,0.3,0.4)
fminv= c(rep(0.05,25), rep(0.10,25))

#Extract results of first hcr
hcrnum=hcrnumv[1]
hcr = paste(hcrnum, "/", sep = "")
bt = btv[1]
bl = blv[1]
trp = trpv[1]
fmin = fminv[1]

dat1=read.table(paste(pdir,hs,hcr, scn, "/impact",hsnum,hcrnum,scnum,".txt", sep =""))
dat1$hcr=hcrnum
dat1$scn=scnum
dat1$trp=trp
dat1$lrp=bl
dat1$thrp=bt
dat1$fmin=fmin

for (j in 2:length(hcrnumv)){
  hcrnum=hcrnumv[j]
  hcr = paste(hcrnum, "/", sep = "")
  bt = btv[j]
  bl = blv[j]
  trp = trpv[j]
  fmin = fminv[j]
  
  dat=read.table(paste(pdir,hs,hcr, scn, "/impact",hsnum,hcrnum,scnum,".txt", sep =""))
  dat$hcr=hcrnum
  dat$scn=scnum
  dat$trp=trp
  dat$lrp=bl
  dat$thrp=bt
  dat$fmin=fmin
  
  dat1 = rbind(dat1, dat)
}

##############IMPACT PERFORMANCE METRIC #####################################################
#The Yield management objective has the following fishery impact metrics;
#Median fishery impact (in %) on SSB in any given year of the evaluation 
#period by fishery and by WCPO fisheries and EPO fisheries
#The probability that the proportional EPO fishery impact is at least 
#the 1971-1994 average in any given year

#set directory to folder where to store results
setwd("C:/Users/desiree.tommasi/Documents/Bluefin/PBF_MSE/1/Output/PM4")

#Calculates median impact across 100 iterations for each year/hcr in the simulation
mean_yeari = as.data.frame(dat1 %>% group_by(hcr,scn,Year) %>% summarize(
  N = length(itr),
  EPOi = median (EPOi, na.rm=TRUE),
  WPOi = median (WPOi, na.rm=TRUE),
  WPOsi = median (WPOsi, na.rm=TRUE),
  WPOli = median (WPOli, na.rm=TRUE),
  WPOmi = median (WPOmi, na.rm=TRUE),
  USi = median (USi, na.rm=TRUE),
  Mxi = median (Mxi, na.rm=TRUE),
  Jpi = median (Jpi, na.rm=TRUE),
  Twi = median (Twi, na.rm=TRUE),
  Kri = median (Kri, na.rm=TRUE),
  Ftarget = mean(trp),
  LRP = mean(lrp),
  ThresholdRP = mean(thrp),
))

#plot of impact between EPO and WPO over time
iepowpo=ggplot(data = mean_yeari) + geom_ribbon(aes(x = Year, ymin = 0, ymax = EPOi), fill = "green") +
  geom_ribbon(aes(x = Year, ymin = EPOi, ymax = (WPOi+EPOi)), fill = "purple")+
  theme_bw() + ylab("Impact (%)")+facet_wrap(~hcr, ncol = 10)

dev.new(width=10, height=8)
png("iepowpo.png", width = 10, height = 8, units = 'in', res = 300)
iepowpo
dev.off()

#calculate the median and 5th and 95th quantiles of impact over the 
#last 10 years of the simulation for each hcr across all 100 iterations
meani = as.data.frame(dat1 %>% filter (Year>2034) %>% group_by(hcr,scn) %>% summarize(
  N = length(itr),
  EPOi = median (EPOi, na.rm=TRUE),
  WPOi = median (WPOi, na.rm=TRUE),
  WPOsi = median (WPOsi, na.rm=TRUE),
  WPOli = median (WPOli, na.rm=TRUE),
  WPOmi = median (WPOmi, na.rm=TRUE),
  USi = median (USi, na.rm=TRUE),
  Mxi = median (Mxi, na.rm=TRUE),
  Jpi = median (Jpi, na.rm=TRUE),
  Twi = median (Twi, na.rm=TRUE),
  Kri = median (Kri, na.rm=TRUE),
  Ftarget = mean(trp),
  LRP = mean(lrp),
  ThresholdRP = mean(thrp),
  EPOiq5 = quantile (EPOi, probs = 0.05,na.rm=TRUE),
  WPOiq5 = quantile (WPOi, probs = 0.05,na.rm=TRUE),
  WPOsiq5 = quantile (WPOsi, probs = 0.05,na.rm=TRUE),
  WPOliq5 = quantile (WPOli, probs = 0.05,na.rm=TRUE),
  WPOmiq5 = quantile (WPOmi, probs = 0.05,na.rm=TRUE),
  USiq5 = quantile (USi, probs = 0.05,na.rm=TRUE),
  Mxiq5 = quantile (Mxi, probs = 0.05,na.rm=TRUE),
  Jpiq5 = quantile (Jpi, probs = 0.05,na.rm=TRUE),
  Twiq5 = quantile (Twi, probs = 0.05,na.rm=TRUE),
  Kriq5 = quantile (Kri, probs = 0.05,na.rm=TRUE),
  EPOiq95 = quantile (EPOi, probs = 0.95,na.rm=TRUE),
  WPOiq95 = quantile (WPOi, probs = 0.95,na.rm=TRUE),
  WPOsiq95 = quantile (WPOsi, probs = 0.95,na.rm=TRUE),
  WPOliq95 = quantile (WPOli, probs = 0.95,na.rm=TRUE),
  WPOmiq95 = quantile (WPOmi, probs = 0.95,na.rm=TRUE),
  USiq95 = quantile (USi, probs = 0.95,na.rm=TRUE),
  Mxiq95 = quantile (Mxi, probs = 0.95,na.rm=TRUE),
  Jpiq95 = quantile (Jpi, probs = 0.95,na.rm=TRUE),
  Twiq95 = quantile (Twi, probs = 0.95,na.rm=TRUE),
  Kriq95 = quantile (Kri, probs = 0.95,na.rm=TRUE)
))

#plots the median and 5th/95th quantiles for the EPO
#with a line showing the 1971-1994 average impact (35%)
epoi_hcr = ggplot(meani, aes(x=as.factor(hcr), y=EPOi,color=as.factor(Ftarget),shape = as.factor(ThresholdRP))) + 
  geom_errorbar(aes(ymin=EPOiq5, ymax=EPOiq95), colour="black", width=.1) +
  geom_point(size=3)+ylab("EPO Fishery Impact Ratio") + xlab("Harvest Control Rule") +
  ggtitle("Harvest Strategy 1a") + theme_bw()+ylim(0,100)+
  scale_colour_discrete(name = "F Target\n Reference Point")+
  scale_shape_discrete(name = "Threshold\n Reference Point")+
  geom_hline(yintercept=35, linetype="dashed")

dev.new(width=10, height=8)
png("epoi_hs1.png", width = 10, height = 6, units = 'in', res = 300)
epoi_hcr
dev.off()

#turns data frame into long format for plotting both EPO and WPO impact
meanil <- melt(meani,
                 id.vars = "hcr",
                 measure.vars = c("EPOi","WPOi"),
                 variable.name = "fgroup")
i_hcr=ggplot(data=meanil, aes(x=as.factor(hcr), y=value, fill=fgroup)) +
  geom_bar(stat="identity")+ylab("Fishery Impact Ratio") + xlab("Harvest Control Rule") +
  ggtitle("Harvest Strategy 1a") + theme_bw()

dev.new(width=10, height=8)
png("impactepowpo_hs1.png", width = 10, height = 6, units = 'in', res = 300)
i_hcr
dev.off()

#turns data frame into long format for plotting both EPO vs WPO impact split for
#for fisheries catchign large, small, or mixed sized fish
meaniw <- melt(meani,
               id.vars = "hcr",
               measure.vars = c("EPOi","WPOsi","WPOli","WPOmi"),
               variable.name = "fgroup")
wi_hcr=ggplot(data=meaniw, aes(x=as.factor(hcr), y=value, fill=fgroup)) +
  geom_bar(stat="identity")+ylab("Fishery Impact Ratio") + xlab("Harvest Control Rule") +
  ggtitle("Harvest Strategy 1a") + theme_bw()

dev.new(width=10, height=8)
png("impactwpo_hs1.png", width = 10, height = 6, units = 'in', res = 300)
wi_hcr
dev.off()

#turns data frame into long format for plotting impact by country
meanic <- melt(meani,
               id.vars = "hcr",
               measure.vars = c("USi","Mxi","Jpi","Twi","Kri"),
               variable.name = "fgroup")
ci_hcr=ggplot(data=meanic, aes(x=as.factor(hcr), y=value, fill=fgroup)) +
  geom_bar(stat="identity")+ylab("Fishery Impact Ratio") + xlab("Harvest Control Rule") +
  ggtitle("Harvest Strategy 1a") + theme_bw()

dev.new(width=10, height=8)
png("impactcountry_hs1.png", width = 10, height = 6, units = 'in', res = 300)
ci_hcr
dev.off()

#calculates probability EPO relative impact is the same or greater 
#than the 1971-1994 average of 0.35
#extract only simulation years, not conditioning period
datsim=dat1 %>% filter(Year>2020)
datsim$p4 = 35/datsim$EPOi

p4p = as.data.frame(datsim %>% group_by(hcr,scn) %>% summarize(
  meanp4 = mean(p4, na.rm=TRUE),
  sdp4 = sd(p4, na.rm=TRUE),
  Ftarget = mean(trp),
  prob = pnorm(1, mean=meanp4, sd=sdp4,lower.tail = TRUE),
  probe=ecdf(p4)(1)
))

#Add the odds associated with the probability (as per table in ISC doc)
p4p<- p4p %>%
  mutate (hcr = as.factor(hcr),
          Odds = cut (prob, 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.6,0.7,0.8,0.9,1),
                      labels = rev(c( "Almost Certain","Highly Likely","Likely","Better than Even","Even","Less than Even","Unlikely","Highly Unlikely","Almost Never")),
                      right = TRUE)
  )

#We used the R brewer palette "RdYlGn" to find the colors to associate with the risk level
#and create a color palette
odds_pal = c(
  "Almost Certain"="#1A9850",
  "Highly Likely"="#66BD63",
  "Likely"= "#A6D96A",
  "Better than Even"="#D9EF8B",
  "Even"= "#FFFFBF",
  "Less than Even"= "#FEE08B",
  "Unlikely"= "#FDAE61",
  "Highly Unlikely"= "#F46D43",
  "Almost Never"= "#D73027"
)

p4prob = ggplot(p4p, aes(x=as.factor(hcr), y=prob,fill=as.factor(Ftarget))) + 
  geom_bar(stat="identity") + scale_x_discrete(limits = (levels(as.factor(p4p$hcr)))) +
  ylab("EPO Impact > 1971-1994 average") + xlab("Harvest Control Rule") +
  ggtitle("Performance Metric 4") + theme_bw() + ylim(0,1)+
  scale_fill_discrete(name = "F Target\n Reference Point")

#the probability is almost never if we keep the same impact ratio
dev.new(width=10, height=8)
png("hs1_p4_prob_epo.png", width = 10, height = 6, units = 'in', res = 300)
p4prob
dev.off()