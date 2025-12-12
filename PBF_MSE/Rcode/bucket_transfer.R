#Transfer iterations to bucket after it's mounted

#Specify path of parent directory
pdir = "/home/user/PBF_MSE/PBF_MSE/"

#specify name of mounted folder linked to bucket
bname = "mse_bucket/"

#Specify project name (all will be saved under this name)
pname = "PBF_MSE/"

#specify scenario, harvest strategy and HCR to move
scnnum = 1
hsnum = 1
hcrnum = 1

hs = paste(hsnum, "/", sep = "")
hcr = paste(hcrnum, "/", sep = "")
scn = paste(scnnum, "/", sep = "")

#Create directories for storage in mounted mse_bucket
dir.create(paste0("/home/user/",bname, pname))
dir.create(paste0("/home/user/",bname, pname, hs))
dir.create(paste0("/home/user/",bname, pname, hs, hcr))
dir.create(paste0("/home/user/",bname, pname, hs, hcr, scn))

#if parallel cluster not initialized
#Calculate the numbers of cores 
no_cores = detectCores() - 2

#Initiate cluster
cl= makeCluster(no_cores)
registerDoParallel(cl)

#move each iteration
foreach(itr = 1:10) %dopar% { 
  #create destination directory for each iteration 
  dir.create(paste0("/home/user/",bname, pname, hs, hcr, scn, itr,"/"))
  #set source directory 
  sdir = (paste0(pdir, hs, hcr, scn, itr,"/"))
  #move files
  command_mv=paste0("mv ",sdir,"* ", "/home/user/",bname, pname, hs, hcr, scn, itr,"/")
  system(command=command_mv)}
#terminate cluster
stopCluster(cl)


