#' Computes the impact time series for the specifed fishery groups for a specified PBF model 
#' Based on the Wang et al. 2009 fisheries impact paper
#'
#' @param Dir directory where the ss model output is stored
#' @param ssDir directory where the ss model executable is stored
#' @return SB_dif data frame with the the resulting SSB for each run, the impact in mt of SSB,
#' and the proportional impact 
#' @author Desiree Tommasi

impact_calc_h1 <- function(Dir,ssDir){
  
  step_name <- c("noEPO", "noWPO", "noF","noWPOs","noWPOl","noWPOm","noUS","noMex","noJpn","noTwn","noKr")
  n_step <- length(step_name)
  
  epof <- c(13:15,29,30) #"EPO"
  wpof <- c(1:12,16:20,26:28) #"WPO"
  nof <- c(1:20,26:30) #nofishing 
  wposf <- c(2,6:8,16,18,19,20,27) #"WPOs"
  wpolf <- c(1,4,5,12,17,28) #"WPOl"
  wpomf <- c(3,9:11,26) #"WPOl"
  usf <- c(13,15,29,30) #"US"
  mxf <- c(14) #"Mexico"
  jpf <- c(1:2,4:11,16,18:20,27:28) #"Jpn"
  twf <- c(12,17) #"Taiwan"
  krf <- c(3,26) #"Korea"
  
  # read Base case data
  CtrlDir <- paste0(Dir, "/OMdat.ss")
  CtrlFile <- SS_readdat(CtrlDir)
  Catch0=CtrlFile$catch
  
  # read par file and initial F estimates
  ParDir <- paste0(Dir, "/ss.PAR")
  ParFile <- readLines(ParDir, warn = F)
  Init_F <- as.numeric(ParFile[49])
  
  #Loop to run the model with the specified fleets removed
  for (step in 1:n_step) {
    print(paste0("step: ", step_name[step]))
    unlink(paste0(Dir, step_name[step]), recursive = TRUE) # remove the folder first
    dir.create(paste0(Dir, step_name[step])) # create a folder for the each step's run
    files <- c(paste0(Dir, "/ss.par"), paste0(Dir, "/ssnohess.bat"), 
              paste0(Dir,"/starter.ss"), paste0(Dir, "/forecast.ss"), 
              paste0(Dir, "/Boot.ctl"), paste0(Dir, "/OMdat.ss"))
    file.copy(from = files, to = paste0(Dir, step_name[step]))
    file2 <- ssDir
    file.copy(from = file2, to = paste0(Dir, step_name[step]))
    CtrlDir <- paste0(paste0(Dir, step_name[step]), "/OMdat.ss")
    CtrlFile <- SS_readdat(CtrlDir)
    
    # remove the catch of each step's fishery
    Catch <- CtrlFile$catch
    
    if (step == 1) 
      fishery <- epof
    if (step == 2) 
      fishery <- wpof
    if (step == 3) 
      fishery <- nof
    if (step == 4) 
      fishery <- wposf
    if (step == 5) 
      fishery <- wpolf
    if (step == 6) 
      fishery <- wpomf
    if (step == 7) 
      fishery <- usf
    if (step == 8) 
      fishery <- mxf
    if (step == 9) 
      fishery <- jpf
    if (step == 10) 
      fishery <- twf
    if (step == 11) 
      fishery <- krf
    
    #The initial F is set using fleet 8, from the wpo. When catches for that fleet
    #are set to 0, the par file needs to be modified to hve no initial F
    if (8 %in% c(fishery)){
      #create new dat file with 0 catch for selected fisheries
      Catch$catch[Catch$fleet %in% c(fishery)]<-0
      CtrlFile$catch <- Catch
      SS_writedat(CtrlFile,CtrlDir,overwrite=TRUE)
      #remove initial F parameter from parameter file as no F8
      ParDir <- paste0(paste0(Dir, step_name[step]), "/ss.par")
      ParFile <- readLines(ParDir, warn = F)
      ParFilen <- ParFile[-(48:49)]
      #Set steepness to 1
      ParFilen[33] <- 0.8
      writeLines(ParFilen, ParDir)
      #set ctl file to not use initial F
      CtlDir <- paste0(paste0(Dir, step_name[step]), "/Boot.ctl")
      CtlFile <- readLines(CtlDir, warn = F)
      initFl <- CtlFile[160]
      CtlFile[160] <- paste("#",initFl)
      writeLines(CtlFile, CtlDir)
    } else {
      #create new dat file with 0 catch for selected fisheries
      Catch$catch[Catch$fleet %in% c(fishery)] <- 0
      CtrlFile$catch <- Catch
      SS_writedat(CtrlFile,CtrlDir,overwrite=TRUE)
      # change initial F in par file based on initial 5 years catch
      ParDir <- paste0(paste0(Dir, step_name[step]), "/ss.par")
      ParFile <- readLines(ParDir, warn = F)
      #if the catches of fleet 8 change from the initial model one can scale th einit F by 
      ParFile[49] <- toString(Init_F * sum(Catch$catch[Catch$fleet==8&Catch$year<=1987])/sum(Catch0$catch[Catch0$fleet==8&Catch0$year<=1987]))
      #set steepness to 1
      ParFile[33] <- 0.8
      writeLines(ParFile, ParDir)
    }
    
    # run the model without estimation
    command <- paste("cd", paste0(Dir, step_name[step]), "& ssnohess.bat", sep = " ")
    # command <- paste0(Dir,step_name[step],'/go_nohess.bat')
    x <- shell(cmd = command, intern = T, wait = T)
  }
  
  #Extract the output from each run
  
  #Original run with all the catches
  Dir1 <- Dir
  myreplist1 <- r4ss::SS_output(dir = Dir1, ncols = 400, covar = F, printstats = F)
  
  #Run with no EPO catches
  Dir2 <- paste0(Dir, step_name[1])
  myreplist2 <- r4ss::SS_output(dir = Dir2, ncols = 400, covar = F, printstats = F)
  
  #Run with no WPO catches
  Dir3 <- paste0(Dir, step_name[2])
  myreplist3 <- r4ss::SS_output(dir = Dir3, ncols = 400, covar = F, printstats = F)
  
  #Run with no catches
  Dir4 <- paste0(Dir, step_name[3])
  myreplist4 <- r4ss::SS_output(dir = Dir4, ncols = 400, covar = F, printstats = F)
  
  #Run with no WPO small fish catches
  Dir5 <- paste0(Dir, step_name[4])
  myreplist5 <- r4ss::SS_output(dir = Dir5, ncols = 400, covar = F, printstats = F)
  
  #Run with no WPO large fish catches
  Dir6 <- paste0(Dir, step_name[5])
  myreplist6 <- r4ss::SS_output(dir = Dir6, ncols = 400, covar = F, printstats = F)
  
  #Run with no WPO mixed fish catches
  Dir7 <- paste0(Dir, step_name[6])
  myreplist7 <- r4ss::SS_output(dir = Dir7, ncols = 400, covar = F, printstats = F)
  
  #Run with no US catches
  Dir8 <- paste0(Dir, step_name[7])
  myreplist8 <- r4ss::SS_output(dir = Dir8, ncols = 400, covar = F, printstats = F)
  
  #Run with no Mexico catches
  Dir9 <- paste0(Dir, step_name[8])
  myreplist9 <- r4ss::SS_output(dir = Dir9, ncols = 400, covar = F, printstats = F)
  
  #Run with no Japan catches
  Dir10 <- paste0(Dir, step_name[9])
  myreplist10 <- r4ss::SS_output(dir = Dir10, ncols = 400, covar = F, printstats = F)
  
  #Run with no Taiwan catches
  Dir11 <- paste0(Dir, step_name[10])
  myreplist11 <- r4ss::SS_output(dir = Dir11, ncols = 400, covar = F, printstats = F)
  
  #Run with no Korea catches
  Dir12 <- paste0(Dir, step_name[11])
  myreplist12 <- r4ss::SS_output(dir = Dir12, ncols = 400, covar = F, printstats = F)
  
  #generates a table with what more SB would have been present if a fishery was not there
  #this is done by subtracting the SSB from the original model from the model with no catches for the specified fleet
  SB_dif <- data.frame(Year = myreplist1$sprseries$Yr[1:(dim(myreplist1$sprseries)[1]-1)], 
                       SB = myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)], 
                       noEPO = myreplist2$sprseries$SSB[1:(dim(myreplist2$sprseries)[1]-1)] - myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noWPO = myreplist3$sprseries$SSB[1:(dim(myreplist3$sprseries)[1]-1)] - myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noF = myreplist4$sprseries$SSB[1:(dim(myreplist4$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noWPOs = myreplist5$sprseries$SSB[1:(dim(myreplist5$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noWPOl = myreplist6$sprseries$SSB[1:(dim(myreplist6$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noWPOm = myreplist7$sprseries$SSB[1:(dim(myreplist7$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noUS = myreplist8$sprseries$SSB[1:(dim(myreplist8$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noMx = myreplist9$sprseries$SSB[1:(dim(myreplist9$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noJp = myreplist10$sprseries$SSB[1:(dim(myreplist10$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noTw = myreplist11$sprseries$SSB[1:(dim(myreplist11$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)],
                       noKr = myreplist12$sprseries$SSB[1:(dim(myreplist12$sprseries)[1]-1)]-myreplist1$sprseries$SSB[1:(dim(myreplist1$sprseries)[1]-1)])#No F is the impact of all fisheries combined
  #scaling of the difference with that of the no fishing run to calculate impact in biomass
  SB_dif$noEPO_dif <- (SB_dif$noEPO/(SB_dif$noEPO+SB_dif$noWPO)) * SB_dif$noF
  SB_dif$noWPO_dif <- (SB_dif$noWPO/(SB_dif$noEPO+SB_dif$noWPO)) * SB_dif$noF
  #Computing the impact ratio
  SB_dif$SB_EPOi<-100*(SB_dif$noEPO/(SB_dif$noEPO+SB_dif$noWPO))
  SB_dif$SB_WPOi<-100*(SB_dif$noWPO/(SB_dif$noEPO+SB_dif$noWPO))
  SB_dif$SB_WPOsi<-100*(SB_dif$noWPOs/(SB_dif$noEPO+SB_dif$noWPOs+SB_dif$noWPOl+SB_dif$noWPOm))
  SB_dif$SB_WPOli<-100*(SB_dif$noWPOl/(SB_dif$noEPO+SB_dif$noWPOs+SB_dif$noWPOl+SB_dif$noWPOm))
  SB_dif$SB_WPOmi<-100*(SB_dif$noWPOm/(SB_dif$noEPO+SB_dif$noWPOs+SB_dif$noWPOl+SB_dif$noWPOm))
  SB_dif$SB_USi<-100*(SB_dif$noUS/(SB_dif$noUS+SB_dif$noMx+SB_dif$noJp+SB_dif$noTw+SB_dif$noKr))
  SB_dif$SB_Mxi<-100*(SB_dif$noMx/(SB_dif$noUS+SB_dif$noMx+SB_dif$noJp+SB_dif$noTw+SB_dif$noKr))
  SB_dif$SB_Jpi<-100*(SB_dif$noJp/(SB_dif$noUS+SB_dif$noMx+SB_dif$noJp+SB_dif$noTw+SB_dif$noKr))
  SB_dif$SB_Twi<-100*(SB_dif$noTw/(SB_dif$noUS+SB_dif$noMx+SB_dif$noJp+SB_dif$noTw+SB_dif$noKr))
  SB_dif$SB_Kri<-100*(SB_dif$noKr/(SB_dif$noUS+SB_dif$noMx+SB_dif$noJp+SB_dif$noTw+SB_dif$noKr))
  
  return(SB_dif)
  
}
  