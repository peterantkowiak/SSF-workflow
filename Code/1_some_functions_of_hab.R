
# not sure whether all runs probably ;) -----------------------------------


# installing packages -----------------------------------------------------

install.packages("adehabitat")
install.packages("tkrplot")
install.packages("hab", repos = "http://ase-research.org/R/", type = "source")
install.packages("hab", repos = "http://ase-research.org/R/")

install.packages("adehabitatMA")
install.packages("adehabitatHR")
install.packages("adehabitatHS")
install.packages("adehabitatLT") # will be installed when insatlling adehabitatHR

require(adehabitatMA)
require(adehabitatLT)  # includes "ade4"
require(adehabitatHS)
require(adehabitat)

## only if the require does not work
library("adehabitat")
library("adehabitat", lib.loc="C:/Program Files/R/R-3.1.1/library")
library("adehabitatHR", lib.loc="C:/Program Files/R/R-3.1.1/library")
library("adehabitatMA")



# as.ltraj ----------------------------------------------------------------


locs <- puechabonsp$relocs
head(puechabonsp)
fred <- data(puechabonsp)
puechabonsp$map
map <- puechabonsp$map
locs <- puechabonsp$relocs
locs <- puechabonsp$relocs
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))

test <- df[, 2]
test2 <- df[, 3]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))
View(df)
ltr1 <- adehabitatLT:::as.ltraj(xy, da, id = id)
ltr2 <- as.ltraj(xy, da, id = id)
all.equal(ltr1, ltr2)




# ld ----------------------------------------------------------------------

df1 <- adehabitatLT:::ld(puechcirc)
data(puechcirc)
puechcirc$ltraj
df1 <- adehabitatLT:::ld(puechcirc)
View(df1)
df2 <- ld(puechcirc, strict = FALSE)
test.ld <- ld(puechcirc)
View(test.ld)
all.equal(df1, df2)
df2 <- ld(puechcirc, strict = FALSE)
df2 <- ld(puechcirc, strict = F)
df2 <- ld(puechcirc)
all.equal(df1, df2)
attr(df1, "row.names")
attr(df2, "row.names")
test.ld <- ld(puechcirc, strict = FALSE)


# dl ----------------------------------------------------------------------

all.equal(dl(df2), adehabitatLT:::dl(df2))
dl(df2, strict = FALSE)


library(help="hab")
install.packages("hab", repos = "http://ase-research.org/R/")
install.packages("hab", repos = "http://ase-research.org/R/", type = "source")


# Comparison regarding 'strict' 

all.equal(dl(df2), dl(df2, strict = FALSE))

## Differences in row.names (numeric in regular 'dl', characters using 
## 'strict = FALSE') + NAs in R2n (for a reason,'puechcirc[[2]]'
## starts by a sequence of missing values, but has several 'R2n'
## values. As a result, 'strict = FALSE' keeps the 'R2n' values)


# infoloc -----------------------------------------------------------------

library("tkrplot", lib.loc="C:/Program Files/R/R-3.1.1/library")
library(help="hab")

## Load puechcirc data, and add some random infolocs:
data(puechcirc)
rnorm(80)
plot(rnorm(80))
summary(rnorm(80))
runif(80)
summary(runif(80))
info <- list(data.frame(A = rnorm(80), B = runif(80), C = rpois(80, 1)), data.frame(A = rnorm(69, 10), B = runif(69, 10, 11), C = rpois(69, 10)), data.frame(A = rnorm(66, -10), B = runif(66, -10, -9), C = rpois(66, 100)))

## Watch the row names:
runif(69, 10, 11)
summary(runif(69, 10, 11))
info <- mapply(function(x, y) {
  row.names(x) <- row.names(y)
  return(x)
}, info, puechcirc, SIMPLIFY = FALSE)
infolocs(puechcirc) <- info

## Try to retrieve the column `toto`:
infolocs(puechcirc, "toto")



# plot.ltraj --------------------------------------------------------------

## Point and line parameters
data(puechcirc)
plot(puechcirc)
plot(puechcirc, ppar = list(pch = 2, cex = .5), lpar = list(lty = 2, col = grey(.5)))

##
## id/perani and mfrow
plot(puechcirc, perani = FALSE)

## Not run:
plot(puechcirc, perani = FALSE, mfrow = c(1, 2))

## End(Not run)
plot(puechcirc, id = "JE93", perani = FALSE)

##
## Using parameters for single steps
info <- list(data.frame(col = sample(c("red", "grey"), 80, rep = TRUE), stringsAsFactors = FALSE), data.frame(col = sample(c("blue", "darkred"), 69, rep = TRUE), stringsAsFactors = FALSE), data.frame(col = sample(c("darkgreen", "purple"), 66, rep = TRUE), stringsAsFactors = FALSE))

info <- mapply(function(x, y) {
  row.names(x) <- row.names(y)
  return(x)
}, info, puechcirc, SIMPLIFY = FALSE)

infolocs(puechcirc) <- info


##
## Per burst (default)
plot(puechcirc, ppar = list(pch = 19, col = infolocs(puechcirc,
"col", simplify = TRUE)), lpar = list(col = infolocs(puechcirc,
"col", simplify = TRUE)), na.rm = FALSE)

##
## Per animal
plot(puechcirc, ppar = list(pch = 19, col = infolocs(puechcirc, "col", simplify = TRUE, perani = TRUE)), lpar = list(col = infolocs(puechcirc, "col", simplify = TRUE, perani = TRUE)), na.rm = FALSE, perani = TRUE)

##'
## Using a SpatialPixelsDataFrame
data(puechabonsp)

##'
plot(puechcirc, perani = FALSE, spixdf = puechabonsp$map[,1])
plot(puechcirc, perani = FALSE, spixdf = puechabonsp$map[,1],
     ppar = list(pch = 2, cex = .5), lpar = list(lty = 2, col = "white"),spixdfpar = list(col = gray((1:240)/256)))

##
## Using a SpatialPolygonsDataFrame
cont <- getcontour(puechabonsp$map[,1])
plot(puechcirc, spoldf = cont)
plot(puechcirc, spoldf = cont, ppar = list(pch = 2, cex = .5),
lpar = list(lty = 2, col = grey(.5)), spoldfpar = list(col = "cornsilk",
border = grey(.5)))

     
     

# home range --------------------------------------------------------------



data(puechabonsp)
loc <- puechabonsp$relocs
elev <- puechabonsp$map
ker1 <- kernelUD(puechabonsp$relocs[,1], grid = elev, same4all = TRUE)
  summary(kerneloverlap(ker1, conditional = TRUE))

image(ker1)
plotLSCV(ker1)




# Demo of the Wild Boar data ----------------------------------------------

demo(rastermaps)
# stops when it becomes 3D
