######################################################
### Clean microclimate model based on I/O tutorial ###
######################################################

# ==== Load packages ====
library(NicheMapR)

# ==== Upstream variables to define ====
ystart=1978
yend=2025
nyears=yend-ystart

# ==== Model Mode Settings ====
writecsv <- 1 # make Fortran program write output as csv files
microdaily <- 1 # run microclimate where each day is iterated 3 times starting with the initial condition of uniform soil temp at mean monthly temperature (0) or with initial conditions of each day based on final conditions of previous day (1)
runshade <- 0 # run the model twice, once for each shade level (1) or just for the first shade level (0)?
IR <- 0 # Clear-sky longwave radiation computed using Campbell and Norman (1998) eq. 10.10 (includes humidity) (0) or Swinbank formula (1)
runmoist <- 0 # run soil moisture model (0=no, 1=yes)?
snowmodel <- 0 # run the snow model (0=no, 1=yes)? - note that this runs slower
hourly <- 1 # use hourly input data?
solonly <- 0 # only run SOLRAD to get solar radiation? 1=yes, 0=no
lamb <- 0 # return wavelength-specific solar radiation output?
IUV <- 0 # use gamma function for scattered solar radiation? 
ndmax <- 3 # iterations of first day to get a steady periodic
message <- 0 # allow the Fortran integrator to output warnings? (1) or not (0)
fail <- nyears*24*365 # how many restarts of the integrator before the Fortran program quits (avoids endless loops when solutions can't be found)
spinup <- 1 # repeat each day three times to get steady period solution? 0 is no, 1 is yes. Set to 1 for daily runs where the initial soil temps are the previous day's final temps.
maxsurf <- 85 # deg C, maximum allowable surface temperature for stability purposes
dewrain <- 0 # feed dew into soil as rain? (0 is no, 1 is yes)
moiststep <- 360 # how many seconds is each time step when simulating soil moisture (3600 would result in the integrator being hourly)

# ==== Time and location parameters ====
doy <- as.matrix(read.csv(file="micro csv input/doy.csv")[2])
ndays <- nrow(doy) # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
idayst <- 1 # start day (legacy parameter)
ida <- ndays # end day (legacy parameter)
ALAT <- -3 # degrees latitude
AMINUT <- 86 # minutes latitude
ALONG <- -32 # degrees longitude
ALMINT <- 43 # minutes latitude
ALREF <- 89 # reference longitude for time zone
HEMIS <- ifelse(ALAT > 0, 1, 2) # chose hemisphere
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)

# ==== Air and wind vertical profile parameters ====
RUF <- 0.005 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
Refhyt <- 2 # Reference height (m), reference height at which air temperature, wind speed and relative humidity input data are measured
Usrhyt <- 0.01# local height (m) at which air temperature, relative humidity and wind speed calculatinos will be made
ZH <- 0 # heat transfer roughness height (m) for Campbell and Norman air temperature/wind speed profile (invoked if greater than 1, 0.02 * canopy height in m if unknown)
D0 <- 0 # zero plane displacement correction factor (m) for Campbell and Norman air temperature/wind speed profile (0.6 * canopy height in m if unknown)
# Next four parameters are segmented velocity profiles due to bushes, rocks etc. on the surface
#IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO! (then roughness height is based on the parameter RUF)
Z01 <- 0 # Top (1st) segment roughness height(m)
Z02 <- 0 # 2nd segment roughness height(m)
ZH1 <- 0 # Top of (1st) segment, height above surface(m)
ZH2 <- 0 # 2nd segment, height above surface(m)

# ==== Radition related parameters ====
SLE <- 0.95 # substrate longwave IR emissivity (decimal %), typically close to 1
REFL <- 0.5 # substrate solar reflectivity (decimal %)
CMH2O <- 2 # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)
# Aerosol extinction coefficient profile
# the original profile from Elterman, L. 1970. Vertical-attenuation model with eight surface meteorological ranges 2 to 13 kilometers. U. S. Airforce Cambridge Research Laboratory, Bedford, Mass.
TAI <- as.matrix(read.csv(file="micro csv input/TAI.csv")[2])

# ==== Terrain and shading parameters ====
ALTT <- 1 # elevation (m)
slope <- 0 # slope (degrees, range 0-90)
azmuth <- 320 # aspect (degrees, 0 = North, range 0-360)
hori <- rep(0, 24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
VIEWF <- 1-sum(sin(hori*pi/180))/length(hori) # convert horizon angles to radians and calc view factor(s)
minshade <- 0 # minimum available shade (%)
maxshade <- 90 # maximum available shade (%)
PCTWET <- 0 # percentage of surface area acting as a free water surface (%)

# ==== Soil profile settings ====
DEP <- c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
ERR <- 1.5 # Integrator error for soil temperature calculations

# ==== Time varying environmental data ====
TIMINS <- c(0, 0, 1, 1)   # time of minima for air temp, wind, humidity and cloud cover (h), air & wind mins relative to sunrise, humidity and cloud cover mins relative to solar noon
TIMAXS <- c(1, 1, 0, 0)   # time of maxima for air temp, wind, humidity and cloud cover (h), air temp & wind maxs relative to solar noon, humidity and cloud cover maxs relative to sunrise
TMINN <- as.matrix(read.csv(file="micro csv input/TMINN.csv")[2]) # minimum air temperatures (deg C)
TMAXX <- as.matrix(read.csv(file="micro csv input/TMAXX.csv")[2]) # maximum air temperatures (deg C)
RHMINN <- as.matrix(read.csv(file="micro csv input/RHMINN.csv")[2]) # min relative humidity (%)
RHMAXX <- as.matrix(read.csv(file="micro csv input/RHMAXX.csv")[2]) # max relative humidity (%)
WNMINN <- as.matrix(read.csv(file="micro csv input/WNMINN.csv")[2]) # min wind speed (m/s)
WNMAXX <- as.matrix(read.csv(file="micro csv input/WNMAXX.csv")[2]) # max wind speed (m/s)
CCMINN <- as.matrix(read.csv(file="micro csv input/CCMINN.csv")[2]) # min cloud cover (%)
CCMAXX <- as.matrix(read.csv(file="micro csv input/CCMAXX.csv")[2]) # max cloud cover (%)
RAINFALL <- as.matrix(read.csv(file="micro csv input/rain.csv")[2]) # monthly mean rainfall (mm)
TAIRhr <- as.matrix(read.csv(file="micro csv input/TAIRhr.csv")[2]) # hourly air temperature input (degrees C) (not used if hourly = 0)
RHhr <- as.matrix(read.csv(file="micro csv input/RHhr.csv")[2]) # hourly relative humidity input (%) (not used if hourly = 0)
WNhr <- as.matrix(read.csv(file="micro csv input/WNhr.csv")[2]) # hourly wind speed input (m/s) (not used if hourly = 0)
CLDhr <- as.matrix(read.csv(file="micro csv input/CLDhr.csv")[2]) # hourly cloud cover input (%) (not used if hourly = 0)
SOLRhr <- as.matrix(read.csv(file="micro csv input/SOLRhr.csv")[2]) # hourly solar radiation input (W/m2) (not used if hourly = 0)
RAINhr <- as.matrix(read.csv(file="micro csv input/RAINhr.csv")[2]) # hourly rainfall input (mm) (not used if hourly = 0)
ZENhr <- as.matrix(read.csv(file="micro csv input/ZENhr.csv")[2]) # hourly zenith angle input (degrees) (not used if hourly = 0 or if negative)
IRDhr <- as.matrix(read.csv(file="micro csv input/IRDhr.csv")[2]) # hourly longwave radiation input (W/m2) (not used if hourly = 0 or if negative)
tannul <- mean(c(TMAXX,TMINN)) # annual mean temperature for getting monthly deep soil temperature (deg C)
tannulrun <- rep(tannul,ndays) # monthly deep soil temperature (2m) (deg C)
SoilMoist <- c(0.42,0.42,0.42,0.43,0.44,0.44,0.43,0.42,0.41,0.42,0.42,0.43) # soil moisture (decimal %, 1 means saturated)
# creating the arrays of environmental variables that are assumed not to change with month for this simulation
MAXSHADES <- rep(maxshade,ndays) # daily max shade (%)
MINSHADES <- rep(minshade,ndays) # daily min shade (%)
SLES <- rep(SLE,ndays) # set up vector of ground emissivities for each day
REFLS <- rep(REFL,ndays) # set up vector of soil reflectances for each day
PCTWET <- rep(PCTWET,ndays) # set up vector of soil wetness for each day

# ==== Soil properties ====
# set up a profile of soil properites with depth for each day to be run
Numtyps <- 1 # number of soil types
Nodes <- matrix(data = 0, nrow = 10, ncol = ndays) # array of all possible soil nodes
Nodes[1,1:ndays] <- 3 # deepest node for first substrate type
Nodes[2,1:ndays] <- 9 # deepest node for second substrate type

# soil thermal parameters 
Thcond <- 2.25 # soil minerals thermal conductivity (W/mC)
Density <- 2560 # soil minerals density (kg/m3)
SpecHeat <- 830 # soil minerals specific heat (J/kg-K)
BulkDensity <- 1282 # soil bulk density (kg/m3)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- 1 # clay content for matric potential calculations (%)
Density <- Density/1000 # density of minerals - convert to Mg/m3
BulkDensity <- BulkDensity/1000 # density of minerals - convert to Mg/m3

# now make the depth-specific soil properties matrix
# columns are:
#1) bulk density (Mg/m3)
#2) volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
#3) thermal conductivity (W/mK)
#4) specific heat capacity (J/kg-K)
#5) mineral density (Mg/m3)
soilprops <- matrix(data = 0, nrow = 10, ncol = 6) # create an empty soil properties matrix
soilprops[1,1] <- BulkDensity # insert soil bulk density to profile 1
soilprops[2,1] <- BulkDensity # insert soil bulk density to profile 2
soilprops[1,2] <- SatWater # insert saturated water content to profile 1
soilprops[2,2] <- SatWater # insert saturated water content to profile 2
soilprops[1,3] <- Thcond # insert thermal conductivity to profile 1
soilprops[2,3] <- Thcond # insert thermal conductivity to profile 2
soilprops[1,4] <- SpecHeat # insert specific heat to profile 1
soilprops[2,4] <- SpecHeat # insert specific heat to profile 2
soilprops[1,5] <- Density # insert mineral density to profile 1
soilprops[2,5] <- Density # insert mineral density to profile 2
soilinit <- rep(tannul,20) # make inital soil temps equal to mean annual

# ==== Soil moisture parameters ====
# note that these are set for sand (Table 9.1 in Campbell and Norman, 1995)
PE <- rep(0.7,19) #air entry potential J/kg
KS <- rep(0.0058,19) #saturated conductivity, kg s/m3
BB <- rep(1.7,19) #soil 'b' parameter
BD <- rep(1.3,19) # soil bulk density, Mg/m3
DD <- rep(2.56,19) # soil density (Mg/m3)  (19 values descending through soil for specified soil nodes in parameter DEP and points half way between)
L <- c(0,0,8.18990859,7.991299442,7.796891252,7.420411664,7.059944542,6.385001059,5.768074989,4.816673431,4.0121088,1.833554792,0.946862989,0.635260544,0.804575,0.43525621,0.366052856,0,0)*10000 # root density at each node, mm/m3 (from Campell 1985 Soil Physics with Basic, p. 131)
R1 <- 0.001 # root radius, m
RW <- 2.5e+10 # resistance per unit length of root, m3 kg-1 s-1
RL <- 2e+6 # resistance per unit length of leaf, m3 kg-1 s-1
PC <-  -1500 # critical leaf water potential for stomatal closure, J kg-1
SP <- 10 # stability parameter for stomatal closure equation, -
IM <- 1e-06 # maximum allowable mass balance error, kg
MAXCOUNT <- 500 # maximum iterations for mass balance, -
LAI <- rep(0.1, ndays) # leaf area index, used to partition traspiration/evaporation from PET
rainmult <- 1 # rainfall multiplier to impose catchment
maxpool <- 10 # max depth for water pooling on the surface, mm (to account for runoff)
rainhourly <- 1 # Is hourly rain input being supplied (1 = yes, 0 = no)?
evenrain <- 2 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (2)
SoilMoist_Init <- rep(0.2,10) # initial soil water content for each node, m3/m3
moists <- matrix(nrow=10, ncol = ndays, data=0) # set up an empty vector for soil moisture values through time
moists[1:10,] <- SoilMoist_Init # insert inital soil moisture

# ==== Snow model inputs ====
snowtemp <- 1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens <- 0.375 # snow density (mg/m3)
densfun <- c(0.5979, 0.2178, 0.001, 0.0038) # slope and intercept of model of snow density as a linear function of snowpack age if first two values are nonzero, and following the exponential function of Sturm et al. 2010 J. of Hydromet. 11:1380-1394 if all values are non-zero; if it is c(0,0,0,0) then fixed density used
snowmelt <- 0.9 # proportion of calculated snowmelt that doesn't refreeze
undercatch <- 1. # undercatch multipier for converting rainfall to snow
rainmelt <- 0.0125 # parameter in equation from Anderson's SNOW-17 model that melts snow with rainfall as a function of air temp
snowcond <- 0 # effective snow thermal conductivity W/mC (if zero, uses inbuilt function of density)
intercept <- maxshade / 100 * 0.3 # snow interception fraction for when there's shade (0-1)
grasshade <- 0 # if 1, means shade is removed when snow is present, because shade is cast by grass/low shrubs

# ==== Intertidal parameters ====
# intertidal simulation input vector (col 1 = tide in(1)/out(0), col 2 = sea water temperature in deg C, col 3 = % wet from wave splash)
tides <- matrix(data = 0, nrow = 24*ndays, ncol = 3) # matrix for tides

# ==== Collate parameters ====
# input parameter vector
microinput <- c(ndays, RUF, ERR, Usrhyt, Refhyt, Numtyps, Z01, Z02, ZH1, ZH2,
                idayst, ida, HEMIS, ALAT, AMINUT, ALONG, ALMINT, ALREF, slope,
                azmuth, ALTT, CMH2O, microdaily, tannul, EC, VIEWF, snowtemp,
                snowdens, snowmelt, undercatch, rainmult, runshade, runmoist,
                maxpool, evenrain, snowmodel, rainmelt, writecsv, densfun,
                hourly, rainhourly, lamb, IUV, RW, PC, RL, SP, R1, IM, MAXCOUNT,
                IR, message, fail, snowcond, intercept, grasshade, solonly, ZH,
                D0, TIMAXS, TIMINS, spinup, dewrain, moiststep, maxsurf, ndmax)


# Final input list - all these variables are expected by the input argument of the Fortran microclimate subroutine
micro <- list(microinput = microinput, tides = tides, doy = doy, SLES = SLES,
              DEP = DEP, Nodes = Nodes, MAXSHADES = MAXSHADES, MINSHADES = MINSHADES,
              TMAXX = TMAXX, TMINN = TMINN, RHMAXX = RHMAXX, RHMINN = RHMINN,
              CCMAXX = CCMAXX, CCMINN = CCMINN, WNMAXX = WNMAXX, WNMINN = WNMINN,
              TAIRhr = TAIRhr, RHhr = RHhr, WNhr = WNhr, CLDhr = CLDhr,
              SOLRhr = SOLRhr, RAINhr = RAINhr, ZENhr = ZENhr, IRDhr = IRDhr,
              REFLS = REFLS, PCTWET = PCTWET, soilinit = soilinit, hori = hori,
              TAI = TAI, soilprops = soilprops, moists = moists, RAINFALL = RAINFALL,
              tannulrun = tannulrun, PE = PE, KS = KS, BB = BB, BD = BD, DD = DD, L = L, LAI = LAI)

# ==== Run the microclimate model ====
microut <- microclimate(micro) # run the model in Fortran

# ==== Extract the soil temperatures ====
soil <- as.data.frame(microut$soil)
soil$DateTime <- seq(as.POSIXct("1978-01-01 00:00", tz = "UTC"),
                     as.POSIXct("2025-12-31 23:00", tz = "UTC"), by = "hours")
write.csv(file = "NicheMapR_out/Microclimate_ERA5_FdN_1978_2025.csv", soil, quote = F)

library(ggpubr)
ggline(soil, x = "DateTime", y = "D50cm", plot_type = "l")

test <- read.csv(file="NicheMapR_out/FdN_ERA5_1978_2025.csv")
test$DateTime <- seq(as.POSIXct("1978-01-01 00:00", tz = "UTC"),
                     as.POSIXct("2025-12-31 23:00", tz = "UTC"), by = "hours")
test$model <- "era5"
soil$model <- "microclimate"

test2<-as.data.frame(rbind(test, soil))
test2$DateTime <- as.POSIXct(test2$DateTime, tz = "UTC")
ggline(test2, x = "DateTime", y = "D50cm", col = "model", plot_type = "l")
