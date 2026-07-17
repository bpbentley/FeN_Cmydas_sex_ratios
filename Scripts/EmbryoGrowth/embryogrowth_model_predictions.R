########################################
### EmbryoGrowth for empirical nests ###
########################################

library(embryogrowth)
library(ggpubr)

### Building the embryogrowth model (this needs a large number of nests)

path <- getwd()

#### fit the growth rate reaction norm ####

load(file.path("Temperature_logger_data", "TrainNests.RData"))

# 1. format nest temperatures using FormatNests()

## first nest
nest1 <- data.frame(Time=TrainSet[[1]][,"time"], Temperature=TrainSet[[1]][,"temperature"])
colnames(nest1)[2] <- names(TrainSet)[1] # makes sure each nest has a unique name

formatted <- FormatNests(nest1, previous=NULL)

## other nests
for(k in 2:length(TrainSet)){
  
  nestX <- data.frame(Time=TrainSet[[k]][,"time"], Temperature=TrainSet[[k]][,"temperature"])
  colnames(nestX)[2] <- names(TrainSet)[k] # makes sure each nest has a unique name
  
  formatted <- FormatNests(nestX, previous=formatted)
  
}

# 2. now we can use the formatted data to fit the Growth Rate Thermal Reaction Norm (GRTRN) using searchR()

## parameter initialization
SCLmean <- 50.02757 # mm (take sizes from you dataset)
SCLsd <- 1.973651 # mm (take sizes from you dataset)

testSCL <- c(Mean=SCLmean, SD=SCLsd)
parinit <- c("DHA"=170, "DHH"=200, "T12H"=300, "Rho25"=385) # parameters to be fitted (these initial values seem to work in most cases)
pfixed <- c(rK=1.208968) # fixed parameters for the Gompertz growth function (FYI, default in the function searchR)
X0 = 0.3470893 # initial embryo size (FYI, default in the function searchR)

## fit the curve using Maximum Likelihood (use HPC)
GRTRN <- searchR(parameters=parinit, 
                 temperatures=formatted, 
                 hatchling.metric=testSCL) # Takes ~30 mins w/ 96GB mem & 16 cores

save(GRTRN, file=file.path(path, "dataOut", "GRTRN.RData"))

load(file.path(path, "dataOut", "GRTRN.RData"))

plotR(GRTRN)


# 3. estimate confidence intervals with Bayesian MCMC (definitely run on HPC)
pMCMC <- TRN_MHmcmc_p(GRTRN, accept=TRUE)

GRTRN_mcmc <- GRTRN_MHmcmc(result=GRTRN,
                           parametersMCMC=pMCMC, 
                           n.iter=10000, 
                           adaptive=TRUE, 
                           trace=TRUE) # Takes ~2 days for 100 nests w/ 96GB mem & 16 cores

save(GRTRN, GRTRN_mcmc, file=file.path(goto, "dataOut", "GRTRN.RData"))

############################################################################################
# Use this code once you have generated the growth model:

load(file.path("Temperature_logger_data", "NestTemps.RData"))

# 1. format nest temperatures using FormatNests()

## first nest
nest1 <- data.frame(Time=NestTemps[[1]][,"time"], Temperature=NestTemps[[1]][,"temperature"])
colnames(nest1)[2] <- names(NestTemps)[1] # makes sure each nest has a unique name

formatted <- FormatNests(nest1, previous=NULL)

## other nests
for(k in 2:length(NestTemps)){
  
  nestX <- data.frame(Time=NestTemps[[k]][,"time"], Temperature=NestTemps[[k]][,"temperature"])
  colnames(nestX)[2] <- names(NestTemps)[k] # makes sure each nest has a unique name
  
  formatted <- FormatNests(nestX, previous=formatted)
}

load(file.path("dataOut", "GRTRN.RData"))
#load(file.path("dataOut", "Final_Nests", "TestNests.RData"))

CM<-subset(DatabaseTSD, Species=="Chelonia mydas" & (!is.na(Sexed) & Sexed!=0))
CM_TSD <- subset(DatabaseTSD, RMU.2023 == "North Atlantic, South Atlantic" & Species=="Chelonia mydas" & (!is.na(Sexed) & Sexed!=0))
tsdL <- with (CM_TSD, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature.set, 
                          equation="logistic", replicate.CI=NULL))

nestinfo <- info.nests(x=GRTRN,
                       resultmcmc=GRTRN_mcmc,
                       temperatures = formatted,
                       out="summary",
                       embryo.stages="Chelonia mydas.SCL",
                       replicate.CI=100,
                       GTRN.CI="MCMC",
                       stop.at.hatchling.metric=TRUE,
                       metric.end.incubation = "hatchling.metric",
                       progressbar=TRUE, tsd=tsdL, tsd.CI="Hessian")
test<-nestinfo$summary

#dates <- seq(as.POSIXct(paste0(test_years[q],"-01-01 0:00")),
#             as.POSIXct(paste0(test_years[q],"-12-31 23:00")),by='days')
#dates<-subset(dates, !grepl("02-29",dates))

write.csv(file=paste0("embryoGrowth_out/Updated_empirical_sex_ratios.csv"),
          test, quote = FALSE)
