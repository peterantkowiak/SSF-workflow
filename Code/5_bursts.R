
# Settings ----------------------------------------------------------------

# installing packages
install.packages("adehabitat")
install.packages("adehabitatMA")
install.packages("adehabitatHR")
install.packages("adehabitatHS")
install.packages("adehabitatLT")
install.packages("tkrplot")
install.packages("hab", repos = "http://ase-research.org/R/", type = "source")
install.packages("hab", repos = "http://ase-research.org/R/")


# loading the packages
require(adehabitat)
require(hab)
require(adehabitatMA)
require(adehabitatHR)
require(adehabitatHS)
require(adehabitatLT)

data(puechabonsp)

# class ltraj  ---------------------------------------


?as.ltraj

data(puechabonsp)
puechabonsp

splocs = puechabonsp$relocs
splocs = as.data.frame(splocs)
head(splocs)
da <- as.character(splocs$Date)
head(da)
da <- as.POSIXct(strptime(as.character(splocs$Date),"%y%m%d"))
head(da)
puech <- as.ltraj(xy = splocs[,c("x","y")], date = da, id = splocs$Name)
puech
# created class ltraj -> type II and irregular (time is known but no constant time lag)
# list containing 4 dataframes (4 bursts -> 4 different animals)

head(puech[[1]])

plot (puech)


# Converting ltraj to data.frame and vice versa ---------------------------


puech2 = ld(puech) # converts ltraj object into data.frame
puech2
puech3= dl(puech2) # converts data.frame into ltraj object
puech3


# Setting bursts ----------------------------------------------------------


plotltr(puech, "dt/3600/24") 
# converting the time lag between relocations into days and plotting

foo = function(dt) {return(dt> (100*3600*24))} 
# set a function that time lag between relocs is <100 days

puech2 <- cutltraj(puech, "foo(dt)", nextr = TRUE)
# look if time lag between relocs is >100 days

puech2 
# Chou is devided into 2 bursts

burst(puech2)[3:4] <- c("Chou.1992", "Chou.1993") 
# renaming the burst

puech2

puech[[1]]

# Playing around with bursts ----------------------------------------------



# 

