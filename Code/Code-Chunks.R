

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



# loading data ------------------------------------------------------------

## as.ltraj handles datasets consisting of two components:

# $map = SpatialPixelsDataFrame
# $relocs = SpatialPointsDataFrame

# These object classes are defined in package "sp"

## How to combine the two objects: ???


# Preparing the raster data -----------------------------------------------

require(sp)

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





