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
ltr3 <- adehabitat:::as.ltraj(xy, da, id = id)  # diffrent use than hab and LT !!!

all.equal(ltr1, ltr2)
all.equal(ltr1, ltr3)   # see the difference - what is the difference


# as.ltraj from "adehabitat" ------------------------------------------------


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

