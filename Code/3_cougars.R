
# Starting with the cougars -----------------------------------------------

install.packages("adehabitatLT")
require(adehabitatLT)
# also hab...

# load data and create SPDF -----------------------------------------------

cougars <- read.csv("P:/SSF PROJECT/UTMsREDUCED.csv", head=T)
cougars <- read.csv("C:/Users/Henia/Desktop/SSF-workflow/Code/UTMsREDUCED.csv", head=T)

head(cougars)

require(sp)

cougarsSPDF = SpatialPointsDataFrame(coords = cougars[,c("easting","northing")], data = cougars)

names(cougarsSPDF)

# to export our spatial data frame - later
write.OGR()

# create ltraj object ------------------------------------------------------

XY <- coordinates(cougarsSPDF)  # coordinates are stored in my SPDF
cougars2 <- as.data.frame(cougarsSPDF)
catID <- as.character(cougars2[,1])
# cougars2[,1] <- as.factor(cougars2[,1]) # does not really help but now its a Factor just as the name in puechabonsp

# it is VERY important to get date and time in the same column! If not the burst cannot be assigned with a unique value:
date = as.POSIXct(strptime(paste(cougars2$LMT_DATE, cougars2$LMT_TIME), "%d/%m/%Y  %H:%M:%S"))

summary(cougars2)
unique(cougars2$cat) # 
# [1] 10286 10287 10288 10289 10290 10291 10293


cougarsLTR <- adehabitatLT:::as.ltraj(XY, date, id = catID)
# only for one individual:
cougarsONE <- adehabitatLT:::as.ltraj(XY[catID=="10286",], date = date[catID=="10286"], id="10286")

all.equal(cougarsLTR, cougarsONE)

# plot ltraj --------------------------------------------------------------

plot(cougarsONE, main="individual # 10286")
plot(cougarsLTR)


adehabitatLT:::plotNAltraj(cougarsONE)  # does that mean there are no NAs ?? nice :)
adehabitatLT:::plotNAltraj(cougarsLTR)


# create random steps ------------------------------------------------------

couONE.step <- rdSteps(cougarsONE)  


########################
##############################
#####################################


data(puechabonsp)
locs <- puechabonsp$relocs
head(locs)
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]
######################################################
##
## Example of a trajectory of type I (time not recorded)
(litrI <- as.ltraj(xy, id = id, typeII=FALSE))
plot(litrI)
## The components of the object of class "ltraj"
head(litrI[[1]])
######################################################
##
## Example of a trajectory of type II (time recorded)
### Conversion of the date to the format POSIX
da <- as.character(df$Date)
da <- as.POSIXct(strptime(as.character(df$Date),"%y%m%d"))
### Creation of an object of class "ltraj", with for
### example the first animal
(tr1 <- as.ltraj(xy[id=="Brock",],
                 date = da[id=="Brock"],
                 id="Brock"))

plot(tr1)


