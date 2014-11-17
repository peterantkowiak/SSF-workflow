# installing packages -----------------------------------------------------

# install.packages("adehabitat")

# install.packages("hab")
install.packages("hab", repos = "http://ase-research.org/R/", type = "source")
# install.packages("hab", repos = "http://ase-research.org/R/")

install.packages("adehabitatMA")
install.packages("adehabitatHR")
install.packages("adehabitatHS")
install.packages("adehabitatLT") # will be installed when insatlling adehabitatHR
install.packages("tkrplot")


require(hab)
require(adehabitatMA)
require(adehabitatLT)  # includes "ade4"
require(adehabitatHS)
require(tkrplot)

# require(adehabitat)  # not necessary to load



# good to know! -----------------------------------------------------------

# adehabitat::as.ltraj  	Working with Trajectories in 2D Space: the Class ltraj
# adehabitatLT::as.ltraj		Working with Trajectories in 2D Space: the Class ltraj
# hab::as.ltraj		Working with Trajectories in 2D Space: the Class ltraj


# as.ltraj with "hab"/"adehabitatLT" ----------------------------------------------------------------

data(puechabonsp)

locs <- puechabonsp$relocs

# head(puechabonsp)
# puechabonsp$map

xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))

View(df)

ltr1 <- adehabitatLT:::as.ltraj(xy, da, id = id)
ltr2 <- hab:::as.ltraj(xy, da, id = id)
ltr3 <- adehabitat:::as.ltraj(xy, da, id = id)  # different use than hab and LT !!!

# keep on trying to do your own burst
# test.ltr <- adehabitatLT:::as.ltraj(xy, da, id = id, burst = c(id,da))

all.equal(ltr1, ltr2)
all.equal(ltr1, ltr3)   # see the difference - what is the difference

head(ltr2[[1]])  ## finally the way, how to access the data stored in a list !!

# as.ltraj from "adehabitat" , need to install the package! ------------------------------------------------


data(puechabon)
locsP <- puechabon$locs
locsP[1:4,]
lixy <- split(locsP[,c("X","Y")],locsP$Name)

### Conversion of the date to the format POSIX
da <- as.character(locsP$Date)
da <- as.POSIXct(strptime(as.character(locsP$Date),"%y%m%d"))
lidat <- split(da,locsP$Name)

### Creation of an object of class "ltraj", with for
### example the first animal
(tr1 <- as.ltraj(lixy[[1]], date = lidat[[1]], id="Brock"))

## The components of the object of class "ltraj"
head(tr1[[1]])

## With all animals
litr <- lapply(1:4, function(i) as.ltraj(lixy[[i]], date = lidat[[i]], id = names(lidat)[i]))
(tr <- do.call("c.ltraj", litr))


data(puechcirc)
  

# infolocs ----------------------------------------------------------------



# na.omit.ltraj -----------------------------------------------------------


# plot.ltraj --------------------------------------------------------------

# the data we modified with as.ltraj
plot(ltr2)
plot(ltr2, id = "Chou")
plot(ltr2, perani=TRUE)  # dont see any changes


# the data prepared from the author, its basically the same data set but reduced to only 3 burst including 2 indiviuals 
data(puechcirc)
puechcirc

# this is the way how to access your data from this weird "list" 
head(puechcirc[[1]])

plot(puechcirc, perani=TRUE) # one plot per id
plot(puechcirc)   # one plot per burst


# plotNAltraj -------------------------------------------------------------

# produces plots showing the NAs ??
data(puechcirc)
adehabitatLT:::plotNAltraj(puechcirc)
plotNAltraj(puechcirc, perani = TRUE, addlines = FALSE, mfrow = c(1,2), ppar = list(pch = 15, cex = 0.5))

adehabitatLT:::plotNAltraj(ltr2)
plotNAltraj(ltr2)

# random steps ------------------------------------------------------------

# with the prepared data puechcirc

## Load the data
data(puechcirc)

##
## Simple example to check the distributions of step lengths and turning angles
bla <- rdSteps(puechcirc)  # imediately gives me a table with all the information I need!
head(bla)

boxplot(bla$rel.angle ~ bla$case)  # case=1 is the true data based on the measured coordinates 
boxplot(bla$dist ~ bla$case)

## Reproducibility and alternative random distributions 
## 1) Default: using the same ltraj for the random distributions:
bla <- rdSteps(puechcirc, reproducible = TRUE)

## 2) Explicitly use the same ltraj for the random distributions:
bli <- rdSteps(puechcirc, rand.dis = puechcirc, reproducible = TRUE)

boxplot(bli$rel.angle ~ bli$case)
boxplot(bli$dist ~ bli$case)


## Check that 2) is the same as 1)
all.equal(bla, bli)


## 3) Explicitly uses random distributions in a data.frame:
rand <- subset(ld(puechcirc), !(is.na(x) | is.na(dx) | is.na(rel.angle)) & dist <= Inf, select = c("dist", "rel.angle", "id"))
blo <- rdSteps(puechcirc, rand.dis = rand, reproducible = TRUE)

## Check that 3) is the same as 1)
all.equal(bla, blo)



# compare random steps and true data --------------------------------------








