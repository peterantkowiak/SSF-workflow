## for implementing SSF

install.packages("adehabitatHR")
install.packages("adehabitatHS")
install.packages("adehabitatLT")
install.packages("adehabitatMA")
# Keep fingers off the "adehabitat" package! It is outdated.

install.packages("tkrplot")
install.packages("hab", repos = "http://ase-research.org/R/") # regular
install.packages("hab", repos = "http://ase-research.org/R/", type = "source") # for self-compiling

# for handling raster data
install.packages("move")
install.packages("raster")
install.packages("rgdal")

setwd("//csrv05/public$/Caro/SSF")
xmpl = read.csv("xmpl.csv", head=T)

xmpl.spdf = SpatialPointsDataFrame(coords = xmpl[,c("easting","northing")],
                                   data = xmpl)

names(xmpl)

date <- as.POSIXct(strptime(paste(xmpl.spdf$LMT_DATE, xmpl.spdf$LMT_TIME),
                            "%d/%m/%Y  %H:%M:%S"))

xmpl.ltr <- hab:::as.ltraj(xy = xmpl.spdf@coords, date = date,
                           id = xmpl.spdf$cat) 

str(xmpl.ltr)

plot(xmpl.ltr)
unique(xmpl.spdf$cat)

plot(xmpl.ltr, id=10289)

plotltr(xmpl.ltr, "dt/3600/3")

foo = function(dt) {return(dt> (3800*3))} 

xmpl.cut <- cutltraj(xmpl.ltr, "foo(dt)", nextr = TRUE)

xmpl.cut

head(xmpl.cut[[5]])

xmpl.cut.df <- ld(xmpl.cut)

with(xmpl.cut.df, plot(dist, rel.angle))

xmpl.steps <- rdSteps(x = xmpl.cut, nrs = 10, simult = T, rand.dis = NULL,  
                      distMax = Inf, reproducible = TRUE, only.others = FALSE)

head(xmpl.steps)

xmpl.steps$new_x <- xmpl.steps$x + xmpl.steps$dx
xmpl.steps$new_y <- xmpl.steps$y + xmpl.steps$dy

head(xmpl.steps)

xmpl.steps

wawotest.ltraj(xmpl.ltr) 
?wawotest


