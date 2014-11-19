

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



# running the examples from hab.pdf ---------------------------------------


# function as.ltraj

data(puechabonsp)
locs <- puechabonsp$relocs
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))
ltr1 <- adehabitatLT:::as.ltraj(xy, da, id = id) 
ltr2 <- as.ltraj(xy, da, id = id)
all.equal(ltr1, ltr2)

df
ltr1


# ltraj objects ------------------------------------------------------------

## as.ltraj handles datasets consisting of two components:

# $map = SpatialPixelsDataFrame
# $relocs = SpatialPointsDataFrame

# These object classes are defined in package "sp"

## How to combine the two objects: ???


# Preparing the raster data -----------------------------------------------



install.packages("RArcInfo")
require(RArcInfo)
require(raster)
require(rgdal)

require(sp)

#############

?raster
getwd()
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



# rDF <- as.data.frame(r) # memory overflow


getValues()
as.matrix()
extract() # can we extract data by just giving coordinates?
coordinates()
as.data.frame()

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

#################
#tiffs


####################################################
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





# Preparing the waypoint data -------------------------------------------------

require(sp)

cougars = read.csv("/home/Peter/Dokumente/uni/WS_14_15/Best Practice R/Dataset/UTMsREDUCED.csv", head=T)
head(cougars)

## SpatialPointsDataFrame

data(meuse)
head(meuse)
head(meuse.grid)
str(meuse) # is a data frame with x and y coordinates and other arbitrary variables, e.g. date

SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0), proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)

o = SpatialPointsDataFrame(coords = meuse[,c("x","y")], data = meuse)

names(o)
o
str(o)



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





# Function rdSteps --------------------------------------------------------


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





