

### installing the necessary packages

install.packages("adehabitat")
install.packages("tkrplot")
install.packages("hab", repos = "http://ase-research.org/R/", type = "source")
install.packages("hab", repos = "http://ase-research.org/R/")


# loading the packages
require(adehabitat, hab, adehabitatMA)
data(puechabonsp)

# running the example from hab.pdf
data(puechabonsp)
locs <- puechabonsp$relocs
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
da <- as.POSIXct(strptime(as.character(df$Date), "%y%m%d"))
ltr1 <- adehabitatLT:::as.ltraj(xy, da, id = id)
ltr2 <- as.ltraj(xy, da, id = id)
all.equal(ltr1, ltr2)



data(puechcirc)
head(puechcirc)



