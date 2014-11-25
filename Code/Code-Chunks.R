

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
#require(raster)
#require(rgdal)
#require(tkrplot)
#require(raster)
#require(sp)

#demo(rastermaps)


#data(puechabonsp)
#data(bauges) # chamois dataset from france



# Loading the waypoint data -------------------------------------------------

require(sp)

cougars = read.csv("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/UTMsREDUCED.csv", head=T)
head(cougars)


# create SPDF -----------------------------------------------

cougars <- read.csv("P:/SSF PROJECT/UTMsREDUCED.csv", head=T)
cougars <- read.csv("C:/Users/Henia/Desktop/SSF-workflow/Code/UTMsREDUCED.csv", head=T)

head(cougars)

require(sp)

cougarsSPDF = SpatialPointsDataFrame(coords = cougars[,c("easting","northing")], data = cougars)

names(cougarsSPDF)


##############################

# to export our spatial data frame - later
#write.OGR()


## SpatialPointsDataFrame

#data(meuse)
#head(meuse)
#head(meuse.grid)
#str(meuse) # is a data frame with x and y coordinates and other arbitrary variables, e.g. date

#SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0), proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)

#o = SpatialPointsDataFrame(coords = meuse[,c("x","y")], data = meuse)

#names(o)
#o
#str(o)



# create ltraj object ------------------------------------------------------

XY <- coordinates(cougarsSPDF)  # coordinates are stored in my SPDF

# This is not necessary
cougarsDF <- base:::as.data.frame(cougarsSPDF)
# catID <- as.character(cougarsDF[,3])
# cougars2[,1] <- as.factor(cougars2[,1]) # does not really help but now its a Factor just as the name in puechabonsp

# it is VERY important to get date and time in the same column! If not the burst cannot be assigned with a unique value:
date = as.POSIXct(strptime(paste(cougarsDF$LMT_DATE, cougarsDF$LMT_TIME), "%d/%m/%Y  %H:%M:%S"))

summary(cougarsDF)
unique(cougarsDF$cat) # 
# [1] 10286 10287 10288 10289 10290 10291 10293


cougarsLTR <- hab:::as.ltraj(coordinates(cougarsSPDF), date, id = cougarsSPDF$cat) 
cougarsLTR <- hab:::as.ltraj(XY, date, id = cougarsDF[,1]) 
# cougarsLTR <- hab:::as.ltraj(XY, date, id = catID) 


plot(cougarsLTR)


# only for one individual:
# cougarsONE <- adehabitatLT:::as.ltraj(XY[catID=="10286",], date = date[catID=="10286"], id="10286")

plot(cougarsLTR, id=10289)

#all.equal(cougarsLTR, cougarsONE)


#########################
# option 1 to solve it: ignore date
cougarsLTR <- adehabitatLT:::as.ltraj(XY, date = dat, id=catID, burst=catID, typeII=F)

# option 2 (to be finished): include hours in the data. Maybe choose other column as id
cougarsLTR2 <- adehabitatLT:::as.ltraj(XY, date = dat, id=cougars2[,3])



########################
## ltraj objects 

## as.ltraj handles datasets consisting of two components:

# $map = SpatialPixelsDataFrame
# $relocs = SpatialPointsDataFrame

# These object classes are defined in package "sp"

# usage examples
data(puechabonsp)
locs <- puechabonsp$relocs
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))
ltr1 <- adehabitatLT:::as.ltraj(xy, da, id = id) 
ltr2 <- as.ltraj(xy, da, id = id)
all.equal(ltr1, ltr2)



# plot ltraj --------------------------------------------------------------

plot(cougarsONE, main="individual # 10286")
plot(cougarsLTR)


adehabitatLT:::plotNAltraj(cougarsONE)  # does that mean there are no NAs ?? nice :)
adehabitatLT:::plotNAltraj(cougarsLTR)


# create bursts -------------------------------------------------
plotltr(cougarsLTR)
plotltr(cougarsLTR, "dt/3600") # 1 hour intervals 
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
cougars.steps.c <- rdSteps(cougars.LTR.cut) 

head(cougars.steps)
str(cougars.steps)
View(cougars.steps)

## check for correlation between steplength and angle
with(cougars.steps.c, plot(dist, rel.angle))

# If they correlate: For drawing angle and length at the same time use simult=T
cougars.steps.c <- rdSteps(cougars.LTR.cut, simult=T)


## calculate new coordinates

#cougars.steps$new_x <- cougars.steps$x + cougars.steps$dx
#cougars.steps$new_y <- cougars.steps$y + cougars.steps$dy

cougars.steps.c$new_x <- cougars.steps.c$x + cougars.steps.c$dx
cougars.steps.c$new_y <- cougars.steps.c$y + cougars.steps.c$dy


### examples rdSteps ######

data(puechcirc)
#head(puechcirc)


## Simple example to check the distributions of step lengths and turning
## angles
bla <- rdSteps(puechcirc)
boxplot(bla$rel.angle ~ bla$case)
boxplot(bla$dist ~ bla$case)

## Reproducibility and alternative random distributions
## 1) Default: using the same ltraj for the random distributions:
bla <- rdSteps(puechcirc, reproducible = TRUE)

## 2) Explicitly use the same ltraj for the random distributions:
bli <- rdSteps(puechcirc, rand.dis = puechcirc, reproducible = TRUE)

## Check that 2) is the same as 1)
all.equal(bla, bli)

## 3) Explicitly uses random distributions in a data.frame:
rand <- subset(ld(puechcirc), !(is.na(x) | is.na(dx) | is.na(rel.angle)) &
                 dist <= Inf, select = c("dist", "rel.angle", "id"))
blo <- rdSteps(puechcirc, rand.dis = rand, reproducible = TRUE)

## Check that 3) is the same as 1)
all.equal(bla, blo)



# Preparing the raster data -----------------------------------------------


install.packages("RArcInfo")
require(RArcInfo)
require(raster)
require(rgdal)

require(sp)


#?raster
#getwd()
#setwd("/home/Peter/")

ruggedness <- raster("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/tri1/w001001.adf") 
# plot(ruggedness) # outcomment this if you just quickly want to run the script. Takes a minute to process.

landcover <- raster("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/lc_30/w001001.adf") 
# plot(landcover) # outcomment this if you just quickly want to run the script. Takes a minute to process.

canopycover <- raster("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/cc_abmt/w001001.adf") 
# plot(canopycover) # outcomment this if you just quickly want to run the script. Takes a minute to process.

disthighway <- raster("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/disthwy/w001001.adf") 
# plot(disthighway) # outcomment this if you just quickly want to run the script. Takes a minute to process.

distroad <- raster("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/NEW GIS LAYERS/distsmrd/w001001.adf") 
plot(distroad) # outcomment this if you just quickly want to run the script. Takes a minute to process.



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

ruggedness.extr <- extract(ruggedness, cougars.steps.c.SPDF, method='simple', sp=F, df=T) 
landcover.extr <- extract(landcover, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
canopycover.extr <- extract(canopycover, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
disthighway.extr <- extract(disthighway, cougars.steps.c.SPDF, method='simple', sp=F, df=T)
distroad.extr <- extract(distroad, cougars.steps.c.SPDF, method='simple', sp=F, df=T)





cougars.steps.c.SPDF$ruggedness <- ruggedness.extr[,2]
#cougars.steps.c.SPDF$landcover <- landcover.extr[,2]
cougars.steps.c.SPDF$landcover1 <- as.factor(
    ifelse(landcover.extr[,2] == 0,"NA",
    ifelse(landcover.extr[,2] < 5, "forest", 
    ifelse(landcover.extr[,2] < 8, "open","NA"))))         
cougars.steps.c.SPDF$canopycover <- canopycover.extr[,2]
cougars.steps.c.SPDF$disthighway <- disthighway.extr[,2]
cougars.steps.c.SPDF$distroad <- distroad.extr[,2]

  
head(cougars.steps.c.SPDF)

#head(cougars.steps.Rugged)
#View(cougars.steps.Rugged)

#head(cougars.steps.c.Rugged)
#View(cougars.steps.c.Rugged)


#names(ruggedness)
#head(cougars)
#head(cougarsRugged)


# rDF <- as.data.frame(r) # memory overflow


#getValues()
#as.matrix()
#extract() # can we extract data by just giving coordinates?
#coordinates()
#as.data.frame()

###################
get.arcdata(datadir, coverage, filename="arc.adf")

##################
library(rgdal)
dpath<-"/home/vrubio/tmp/GLiPHAmaps/global/poultry/totcor/glbpod1t0503m/hdr.adf"
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))

library(RColorBrewer)
xx<-asSGDF_GROD(x, output.dim=c(200, 200))
spplot(xx, "band1", 
       at=c(0, 10, 50, 100, 200, 500, 1000, 2500, max(xx$band1,
                                                      na.rm=TRUE)),
       col.regions=brewer.pal(8,"Oranges") )

If you require a higher resolution try to increase the values of
output.dim=c(200, 200) but be careful because it can take a lot of
memory.

##
#tiffs


##
## SpatialPixelsDataFrame
# defines a spatial grid with attribute data

data(meuse.grid)
# meuse.grid is a data frame with 3103 obs of 7 variables. 

head(meuse.grid)
?meuse.grid
str(meuse.grid)
# the columns are: x, y, part.a, part.b, dist, soil, ffreq
# x and y are the coordinates given as numeric vectors
# all other columns are arbitrary data that can be given as numeric vectors or factors

## How to create a SpatialPixelsDataFrame:
m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)
m

# checking whether Transformation worked
class(m)
summary(m)

## Alternative for loading data:

csv = read.csv(file =, header = T, sep =, dec =) # to be filled in
SPDF.grid = as.data.frame(csv, stringsAsFactors = T) # to be filled in

n = SpatialPixelsDataFrame(points = SPDF.grid[c("x", "y")], data = SPDF.grid)
n

# options for SpatialPixelsDataFrame function ???
# use other file formats than .csv as input





# Putting it together -----------------------------------------------------



meuseSDF <- list(m,o)
# this creates the full dataset consisting of a map (SpatialPixelsDataFrame) and the waypoints (SpatialPointsDataFrame).

str(meuseSDF)


head(puechabonsp$map)
head(puechabonsp$relocs)



data(meuse.grid)

str(meuse.grid)


str(puechabonsp)
head(puechabonsp$relocs)

puechabonsp

locs
str(locs)

xy
str(xy)
df$Date
da
id
head(df)

df[,1]
ltr1
ltr2
head(ltr2)
ltr2 <- as.ltraj(xy, da, id = id)



?puechabonsp




# Model --------------------------------------------


##lmer


library(lme4)

# model 1
csR <- as.data.frame(cougars.steps.Rugged)
cscR <- as.data.frame(cougars.steps.c.Rugged)

cscR.all <- as.data.frame(cougars.steps.c.SPDF)

model1 = glmer(case ~ w001001 + (1|id/strata), family = binomial, data=csR)
model1c = glmer(case ~ w001001 + (1|id/strata), family = binomial, data=cscR)
model1c.all = glmer(case ~ ruggedness + landcover + canopycover + disthighway + distroad + (1|id/strata), family = binomial, data=cscR.all)


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


# Rescaling function ------------------------------------------

cs. <- function(x) scale(x,scale=TRUE,center=TRUE) #to rescale your variable
# use just scale instead

model1rc = glmer(case ~ cs.(w001001) + (1|id/strata), data = cscR, family = binomial)
model1rc.q = glmer(case ~ cs.(w001001) + I(cs.(w001001)^2)  + (1|id/strata), data = cscR, family = binomial)
model1c.all.c = glmer(case ~ cs.(ruggedness) + landcover + cs.(canopycover) + cs.(disthighway) + cs.(distroad) + (1|id/strata), family = binomial, data=cscR.all)

summary(model1rc)
plot(allEffects(model1rc))

summary(model1rc.q)
plot(allEffects(model1rc.q))

summary(model1c.all.c)
plot(allEffects(model1c.all.c))



# quadratic
# more parameters


# model1 = glmer(response ~ ruggedness + canopycover + (1|ID/strata), cougars.final.DF,family = binomial)


# Model mclogit -----------------------------------------------------------

install.packages("mclogit")
require(mclogit)

mc <- mclogit(cbind(case, strata) ~ ruggedness + landcover + disthighway, data = cscR.all)

summary(mc) 

mc_quad <-  mclogit(cbind(case, strata) ~ landcover + ruggedness + I(ruggedness^2)  + disthighway + I(disthighway^2) , data = cscR.all)
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


landcover
ruggedness
disthgw


summary(mc_quad)
# cscR.all$wrugg = exp(mc_quad$coefficients[2] * cscR.all$ruggedness  + 
#                        mc_quad$coefficients[3] * cscR.all$ruggedness^2 + 
#                        mc_quad$coefficients[4] * median(cscR.all$disthighway)
#                      + mc_quad$coefficients[5] * (median(cscR.all$disthighway))^2)
# plot(cscR.all$ruggedness,cscR.all$wrugg)

#predictions for ruggedness (highw kept to median)
mydata = data.frame(ruggedness=seq(0,200,1))
mydata$wrugg = exp(mc_quad$coefficients[2] * mydata$ruggedness  + 
                       mc_quad$coefficients[3] * mydata$ruggedness^2 + 
                       mc_quad$coefficients[4] * median(cscR.all$disthighway)
                     + mc_quad$coefficients[5] * (median(cscR.all$disthighway))^2)
plot(mydata$ruggedness,mydata$wrugg,type="l")
mydata$wrugg = exp(mc_quad$coefficients[2] * mydata$ruggedness  + 
                     mc_quad$coefficients[3] * mydata$ruggedness^2 + 
                     mc_quad$coefficients[4] * median(cscR.all$disthighway)
                   + mc_quad$coefficients[5] * (median(cscR.all$disthighway))^2
                   -3.838e-01)
lines(mydata$ruggedness,mydata$wrugg,type="l",col="green")
abline(h=1,lty=2,col="wheat4")




