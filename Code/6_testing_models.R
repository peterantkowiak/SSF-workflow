# only for Henni ;)
 setwd("C:/Users/Henia/Documents")
getwd()
# Installing and loading the necessary packages ---------------------------


# installing the necessary packages

install.packages("adehabitat") 
install.packages("adehabitatHR")
install.packages("adehabitatHS")
install.packages("adehabitatLT")
install.packages("adehabitatMA")
install.packages("tkrplot")
install.packages("hab", repos = "http://ase-research.org/R/") # regular
install.packages("hab", repos = "http://ase-research.org/R/", type = "source") # for self-compiling

install.packages("move")
install.packages("raster")
install.packages("rgdal")
#install.packages("")

# loading the packages
# require(adehabitat) # keep fingers off this package. It is outdated.
require(hab)
require(adehabitatMA)
require(adehabitatHR)
require(adehabitatHS)
require(adehabitatLT)

#require(move)
require(raster)
require(rgdal)
#require(tkrplot)
#require(raster)
#require(sp)

#demo(rastermaps)


#data(puechabonsp)
#data(bauges) # chamois dataset from france



# Loading the waypoint data -------------------------------------------------


cougars <- read.csv("C:/Users/Test/SSF-workflow/Code/UTMsREDUCED.csv", head=T)

head(cougars)


# create SPDF -----------------------------------------------

require(sp)

cougarsSPDF = SpatialPointsDataFrame(coords = cougars[,c("easting","northing")], data = cougars)

names(cougarsSPDF)



# to export our spatial data frame - later
#write.OGR()


# create ltraj object ------------------------------------------------------

XY <- coordinates(cougarsSPDF)  # coordinates are stored in my SPDF
cougarsDF <- base:::as.data.frame(cougarsSPDF)
# catID <- as.character(cougarsDF[,3])
# cougars2[,1] <- as.factor(cougars2[,1]) # does not really help but now its a Factor just as the name in puechabonsp

# it is VERY important to get date and time in the same column! If not the burst cannot be assigned with a unique value:
date = as.POSIXct(strptime(paste(cougarsDF$LMT_DATE, cougarsDF$LMT_TIME), "%d/%m/%Y  %H:%M:%S"))

summary(cougarsDF)
unique(cougarsDF$cat) # 
# [1] 10286 10287 10288 10289 10290 10291 10293


cougarsLTR <- hab:::as.ltraj(XY, date, id = cougarsDF[,1]) 
# cougarsLTR <- hab:::as.ltraj(XY, date, id = catID) 

 # safe the table we need for the ltraj
couLTR <- ld(cougarsLTR)

write.table(couLTR, file = "C:/Users/Test/SSF-workflow/Code/couLTR.csv", sep = " ", dec = ".") 
 
plot(cougarsLTR)
str(cougarsLTR)


# only for one individual:
# cougarsONE <- adehabitatLT:::as.ltraj(XY[catID=="10286",], date = date[catID=="10286"], id="10286")

# plot(cougarsLTR, id=10289)

#all.equal(cougarsLTR, cougarsONE)

 


#
# option 1 to solve it: ignore date
cougarsLTR <- adehabitatLT:::as.ltraj(XY, date = dat, id=catID, burst=catID, typeII=F)

# option 2 (to be finished): include hours in the data. Maybe choose other column as id
cougarsLTR2 <- adehabitatLT:::as.ltraj(XY, date = dat, id=cougars2[,3])



##
## ltraj objects 

## as.ltraj handles datasets consisting of two components:

# $map = SpatialPixelsDataFrame
# $relocs = SpatialPointsDataFrame

# These object classes are defined in package "sp"


# plot ltraj --------------------------------------------------------------

plot(cougarsONE, main="individual # 10286")
plot(cougarsLTR)


adehabitatLT:::plotNAltraj(cougarsONE)  # does that mean there are no NAs ?? nice :)
adehabitatLT:::plotNAltraj(cougarsLTR)


# create bursts -------------------------------------------------

plotltr(cougarsLTR, "dt/7200") # 2 hour intervals
plotltr(cougarsLTR, "dt/900") # 15 minutes intervals
plotltr(cougarsLTR, "dt/1800") # 1/2 hour intervals
plotltr(cougarsLTR, "dt/3600/3") # 3 hour intervals
# converting the time lag between relocations into days and plotting

foo = function(dt) {return(dt> (3800*3))} # intervals of 3:10 h
# set a function that time lag between relocs is <100 days

cougars.LTR.cut <- cutltraj(cougarsLTR, "foo(dt)", nextr = TRUE)
# look if time lag between relocs is > 3:10 h

cougars.LTR.cut 
# very many bursts now

## we now created new bursts if there were gaps in the 3 hour scheme.
## However, we did not exclude relocations recorded at an even higher frequency (e.g. 15 min.)


# create random steps ------------------------------------------------------

#cougars.steps <- rdSteps(cougarsLTR)
# cougars.steps.c <- rdSteps(cougars.LTR.cut) 

# head(cougars.steps)
# str(cougars.steps)
# View(cougars.steps)

## check for correlation between step length and angle
couCUT <- ld(cougars.LTR.cut)  # make a data frame
head(couCUT)
with(couCUT, plot(dist, rel.angle)) # i need a data frame to do that

# the plot shows a correlation of step length and turning angle and therefore the random steps should be taken as pairs (simult=T):
cougars.steps.c <- rdSteps(cougars.LTR.cut, simult=T)
head(cougars.steps.c)

## calculate new coordinates

#cougars.steps$new_x <- cougars.steps$x + cougars.steps$dx
#cougars.steps$new_y <- cougars.steps$y + cougars.steps$dy

cougars.steps.c$new_x <- cougars.steps.c$x + cougars.steps.c$dx
cougars.steps.c$new_y <- cougars.steps.c$y + cougars.steps.c$dy

head(cougars.steps.c)
 names(cougars.steps.c)
 
 write.table(cougars.steps.c, file = "C:/Users/Test/SSF-workflow/Code/cougars.steps.c.csv", sep = " ", dec = ".") 

 


# Preparing the raster data -----------------------------------------------


install.packages("RArcInfo")
require(RArcInfo)
require(raster)
require(rgdal)

require(sp)


#?raster
getwd()
#setwd("/home/Peter/")
 
rugged <- raster("C:/Users/Test/Documents/Uni/Sem_09/01 Best Practice/NEW GIS LAYERS/tri1/w001001.adf") 

# plot(ruggedness) # outcomment this if you just quickly want to run the script. Takes a minute to process.

landco <- raster("C:/Users/Test/Documents/Uni/Sem_09/01 Best Practice/NEW GIS LAYERS/lc_30/w001001.adf")

# plot(landcover) # outcomment this if you just quickly want to run the script. Takes a minute to process.
 
canopyco <- raster("C:/Users/Test/Documents/Uni/Sem_09/01 Best Practice/NEW GIS LAYERS/cc_abmt/w001001.adf") 

# plot(canopycover) # outcomment this if you just quickly want to run the script. Takes a minute to process.

disthiway <- raster("C:/Users/Test/Documents/Uni/Sem_09/01 Best Practice/NEW GIS LAYERS/disthwy/w001001.adf") 

# plot(disthighway) # outcomment this if you just quickly want to run the script. Takes a minute to process.


# due to correlation of the last two parameters i leave one out
# distroad <- raster("/home/Test/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/distsmrd/w001001.adf") 
# plot(distroad) # outcomment this if you just quickly want to run the script. Takes a minute to process.


# Raster extraction ---------------------------------

#sp <- SpatialPoints(xy)
# cougarsRugged <- extract(ruggedness, cougarsSPDF, method='simple', sp=T, df=T) 
# method = 'bilinear' interpolates values from four nearest cells. sp returns Spatial object, df a data frame.


# first convert the cougars.steps into a SpatialPointsDataFrame

#cougars.steps.SPDF = SpatialPointsDataFrame(coords = cougars.steps[,c("new_x","new_y")], data = cougars.steps)
cougars.steps.c.SPDF = SpatialPointsDataFrame(coords = cougars.steps.c[,c("new_x","new_y")], data = cougars.steps.c)

#cougars.steps.Rugged <- extract(ruggedness, cougars.steps.SPDF, method='simple', sp=T, df=T) 
#cougars.steps.c.Rugged <- extract(ruggedness, cougars.steps.c.SPDF, method='simple', sp=T, df=T) 

# method = 'simple' extracts value from nearest cell. method = 'bilinear' interpolates from the four nearest cells.

ruggedness.extr <- extract(rugged, cougars.steps.c.SPDF, method='simple', sp=F, df=T) 
landcover.extr <- extract(landco, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
canopycover.extr <- extract(canopyco, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
disthighway.extr <- extract(disthiway, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
# distroad.extr <- extract(distroad, cougars.steps.c.SPDF, method='simple', sp=F, df=T)


cougars.steps.c.SPDF$rugged <- ruggedness.extr[,2]
cougars.steps.c.SPDF$landco <- landcover.extr[,2]
cougars.steps.c.SPDF$canopyco <- canopycover.extr[,2]
cougars.steps.c.SPDF$disthiway <- disthighway.extr[,2]
# cougars.steps.c.SPDF$distroad <- distroad.extr[,2]

  
head(cougars.steps.c.SPDF)


#head(cougars.steps.Rugged)
#View(cougars.steps.Rugged)

head(cougars.steps.c.Rugged)
View(cougars.steps.c.Rugged)


#names(ruggedness)
#head(cougars)
#head(cougarsRugged)


# Model glmer --------------------------------------------


library(lme4)

# model 1
# csR <- as.data.frame(cougars.steps.Rugged)
# cscR <- as.data.frame(cougars.steps.c.Rugged)

cscR.all <- as.data.frame(cougars.steps.c.SPDF)

# model1 = glmer(case ~ w001001 + (1|id/strata), family = binomial, data=csR)
# model1c = glmer(case ~ w001001 + (1|id/strata), family = binomial, data=cscR)

m_all = glmer(case ~ rugged + landco + canopyco + disthiway + (1|id/strata), family = binomial, data=cscR.all)

test <- stepAIC(m_all)

summary(model1)
summary(model1c)
summary(model1c.all)

## model1c less significant than model1. why?
# - loss of data points
# - linear instead of quadratic term?
# - average step length too small?


library(effects)

plot(allEffects(model1))
plot(allEffects(model1c))
plot(allEffects(model1c.all))


# Rescaling function - the better models ------------------------------------------

cs. <- function(x) scale(x,scale=TRUE,center=TRUE) #to rescale your variable



resc_all = glmer(case ~ cs.(rugged) + landco + cs.(canopyco) + cs.(disthiway) + (1|id/strata), family = binomial, data=cscR.all)

summary(resc_all)
plot(allEffects(resc_all))
# test <- stepAIC(resc_all) # not working because of the wrong data class ( in this case c4 class) - no real solution on the web

# this takes ages and might not be the best way to test for a good model...
resc_all_quad = glmer(case ~ (cs.(rugged) + landco + cs.(canopyco) + cs.(disthiway))^2 + I(cs.(rugged)^2) + I(landco^2) + I(cs.(canopyco)^2) + I(cs.(disthiway)^2) + (1|id/strata), family = binomial, data=cscR.all)

summary(resc_all_quad)

# trying to sort out by significance
resc_all_sig = glmer(case ~ (cs.(rugged) + cs.(disthiway))^2 + I(cs.(rugged)^2) + landco  + I(cs.(disthiway)^2) + (1|id/strata), family = binomial, data=cscR.all)

plot(allEffects(resc_all_sig))
summary(resc_all_sig)
 
# add rugged^2

 

 

# Model mclogit -----------------------------------------------------------

install.packages("mclogit")
require(mclogit)

mc <- mclogit(cbind(case, strata) ~ rugged + landco + disthiway, data = cscR.all)
 
summary(mc) 
 
mc_quad <-  mclogit(cbind(case, strata) ~ landco + I(rugged^2)  + disthiway, data = cscR.all)
summary(mc_quad) 

# models give same results as the glmer:
 # ruggedness is significant when quared, landcover is always significant
 
# plot(allEffects(mc)) # how to plot??
 
 
 ## example
 data(Transport)
 
 summary(mclogit(
   cbind(resp,suburb)~distance+cost,
   data=Transport
 ))
 
 
