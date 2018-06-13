## Libraries and functions
library(sp)
library(raster)
library(ape)
library(distrEx)
library(rgdal)
library(ncf)
library(Hmisc)
library(gplots)
library(distr)


#########################################################################################
## Variables in this section will need to be changed
#########################################################################################

source("HellsDistFunction.R") ##set location of the HellsDistFunction file

#### Load study sites dataset
sites <- read.csv("data_foodwebsclimate.csv") ##set location of data file with study site locations
lat <- sites$y
long <- sites$x

#### Load Directory for Global Layers
GLdir <- "C:\\Users\\hellinger\\" ##climate data layers from https://gisclimatechange.ucar.edu/gis-data-ar5 (should be .tif)


#### Load global layers, for site level values only
precip60 <- raster(file.path(GLdir, "ppt_rcp60.tif")) ##ppt_rcp60.tif is the climate data layer that sites are compared to
sites$precip60 <- extract(precip60, data.frame(lat, long))
summary(sites$precip60)

temp60 <- raster(file.path(GLdir, "tas_rcp60.tif")) ##tas_rcp60.tif is a second climate data layer that sites are compared to
sites$temp60 <- extract(temp60, data.frame(lat, long))
summary(sites$temp60)


p60 <- sites$precip60
t60 <- sites$temp60


#####################################################################################
## End of section - should run with no changes
#####################################################################################

###
## Method used in Gonzales et al. 2016. Ecology 97: 1949-1960.
quants <- c(1:4)
UD<- DiscreteDistribution(quants, c(0.25,0.25,0.25,0.25))

precip60_HD <- list()
precip60_HD[[1]] <- HellsDist(site_data_col = p60, "ppt_rcp60.tif", inDir=GLdir, land.use = FALSE, n.reps = 1000, UD = UD)
precip60_HD[[2]] <- HellsDistData(site_data_col = p60,  "ppt_rcp60.tif", inDir = GLdir, land.use = FALSE, UD = UD)

temp60_HD <- list()
temp60_HD[[1]] <- HellsDist(site_data_col = t60, "tas_rcp60.tif", inDir=GLdir, land.use = FALSE, n.reps = 1000, UD = UD)
temp60_HD[[2]] <- HellsDistData(site_data_col = t60,  "tas_rcp60.tif", inDir = GLdir, land.use = FALSE, UD = UD)

precip60_HD_meansSD <- c(mean(precip60_HD[[1]]), sd(precip60_HD[[1]]))
precip60dat_HD_meansSD <- c(precip60_HD[[2]])

temp60_HD_meansSD <- c(mean(temp60_HD[[1]]), sd(temp60_HD[[1]]))
temp60dat_HD_meansSD <- c(temp60_HD[[2]])


#####################################################################################
## End of section
#####################################################################################
## Plot
layer_labels <- c("Temp. change (RCP 6)", "Precip. change (RCP 6)")
plotCI(x=1, y=0, type="n", xlim=c(0,0.5), ylim=c(1,2.5), xlab = "Hellinger distance, d", yaxt = "n", ylab = "", bty = "n")

rect(-0.5,5, 1, 8,col="#A0A0A033",border=NA)

plotCI(x =  precip60_HD_meansSD[1], y =1.25, uiw = precip60_HD_meansSD[2],err='x', cex = pt.cex, gap =0, pch = pt.pch, add =TRUE)
plotCI(x =  precip60dat_HD_meansSD[1], y =1.25,err='x',cex = pt.cex2, lwd = 2, gap =0, pch = pt.pch2, col = "red", add = TRUE)

plotCI(x =  temp60_HD_meansSD[1], y =2.25, uiw = temp60_HD_meansSD[2],err='x', cex = pt.cex, gap =0, pch = pt.pch, add =TRUE)
plotCI(x =  temp60dat_HD_meansSD[1], y =2.25,err='x',cex = pt.cex2, gap =0, lwd = 2, col = "red", pch = pt.pch2, add = TRUE)

axis(2, at = 2.25:1.25, labels = layer_labels, las =2)


