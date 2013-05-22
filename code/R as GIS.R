########################################
### code to install packages needed ###
#######################################

# get names of all packages already installed
packs <- installed.packages()
exc <- names(packs[,'Package'])

# list of  packages that this code use
av <- c("repmis","ggplot2","ggmap","sp","maptools","RColorBrewer","classInt","rgdal",
        "raster","rasterVis","adehabitat","SDMTools")

# save to ins the packages that are not installed, next install ins
ins <- av[!av %in% exc]
install.packages(ins)
######################<--- end of package installation


### set the working directory,  to the file you use to unzip the data
setwd("D:/Documents and Settings/dlizcano/Documents/Rcode/R_as_GIS")


##################################
## some basic data manipulation  ##
##################################

#### Points #####

## read TEAM DATA from Volcan Barva, Costa Rica
data<-read.csv("VB_Mammals.csv" ,header = TRUE)
head(data) # see what we have

# calculate midle point
meanLat<-mean(data$Latitude) - 0.05
meanLong<-mean(data$Longitude)
location<-c(meanLong, meanLat)

#Plot all
plot(x=data$Longitude,y=data$Latitude,asp=1) #Boring !

## more elaborated map using ggmap
require(ggmap) # same as ggplot2 but for maps
wmap1 = qmap(location, zoom = 11) #Get a base map from google with location in the center

wmap1 

wmap2<- wmap1 + geom_point(data = data, aes(Longitude, Latitude), size = I(2), 
                          color = "red", na.rm = TRUE)
wmap2

# add the kernel of number of register arround the points
wmap3<- wmap1 + stat_density2d( aes(x = Longitude, y = Latitude, fill = ..level.., alpha =..level..), 
                                bins = 20, data = data, geom = 'polygon', alpha =    I(0.5),
                                show_guide = F, na.rm = TRUE) 
wmap3

# add the point on top
wmap4 <- wmap3 + geom_point(data = data, aes(Longitude, Latitude), size = I(2), 
                            color = "red", na.rm = TRUE) 

# separate by order, can be any other factor...species, sampling.period...
wmap4a<-wmap4 + facet_wrap (~Order) 
wmap4a

# add titles
wmap5 <- wmap4 + theme_bw() + labs(x = "Longitude", y = "Latitude") + ggtitle("Volcan Barva")

print(wmap5)


####################
#### Polygons ######
####################

## Load required packages
require (sp) # import and export shp
require (maptools)
library(RColorBrewer)
library(classInt)

## set the working directory,  to the file you use to unzip the data
# setwd("C:\Users\Diego\Documents\GitHub\R_as_GIS\NEW\data")

## load the shapefile. Use your directory 
forest<- readShapePoly("forest_types.shp")

plot(forest)
##have a look at the attribute table headings
names(forest)
length(forest$GROUP) # number of forest types
(labs<-forest$GROUP) # see the labels of forest types

## select a colour palette and the number of colours you wish to display.
colours <- brewer.pal(8, "Greens") 

# plot the map again
plot(forest, col=colours, axes=T)

## we can add:
## a title:
title(paste ("Forest Volcan Barva"))
## a legend
legend(x=815000, y=1130000, legend=leglabs(labs), fill=colours, bty="n", cex= 1,ncol=1)
## a north arrow: 
SpatialPolygonsRescale(layout.north.arrow(2), offset= c(812000,1150000), scale = 6000, 
                       plot.grid=F)

## a scale bar:
SpatialPolygonsRescale(layout.scale.bar(), offset= c(832000, 1121000), scale= 10000, fill= 
                         c("transparent", "black"), plot.grid= F)

## and some annotations:
text(834000, 1122300, "10KM", cex= 1)
text(835000, 1120000, "Data by TEAM 2013", cex= 1)


#########################
## Changing projection ##
#########################
require(rgdal)

summary(forest) # notice no projection, but see forest.prj
# lets define projection
utm16n<-CRS("+proj=utm +zone=16 +datum=WGS84")
proj4string(forest)<- utm16n

### take a look to http://spatialreference.org
latlon<- CRS("+proj=longlat +datum=WGS84")
forest_g<-spTransform(forest, latlon)

##############
##   overlay   ###
##############

campoints<-as.data.frame(cbind(data$Longitude,data$Latitude)) #extract the points from data
colnames(campoints)<-c("lon","lat")
# change from data frame to Object of class SpatialPoints
coordinates(campoints)<-c("lon","lat") # 
proj4string(campoints)<- latlon

ov<-over(forest_g, campoints, fn = sum)
photoxforest<-cbind(as.data.frame(labs),ov)
photoxforest 


#################
##  R A S T E R   ##
#################

##########################
require (raster) # an important Package !!!
library (rasterVis)
require (adehabitat)
require (SDMTools)
##########################

### import ASCii to Raster object in UTM 
## adjust the path to the data directory
aspect <- raster("aspect100m_cl.txt")    # notice the // and \              
slope <- raster("slope100m_cl.txt")               
canopy <- raster ("canopy100m_cl.txt")                
elev <- raster ("elev100m_cl.txt")                
edge <- raster ("edge100_cl.txt")

## put the proper projection             
projection(aspect)<-utm16n
projection(slope)<-utm16n  # 
projection(canopy)<-utm16n
projection(elev)<-utm16n
projection(edge)<-utm16n

### chk
plot(aspect)
plot(slope)
plot(canopy)
plot(elev)
plot(edge)

##### descriptive statistics
summary(canopy)

all_layers<-stack(elev,slope,aspect,edge,canopy) ## stack all
plot(all_layers) # chk 
spplot (all_layers) ## another visualization
##### more  statistics
histogram(all_layers) 
pairs(all_layers)

##### some more  visualizations 
persp(elev)
contourplot(elev)
levelplot(elev)
densityplot(elev)

#### spatial autocorrelation
Moran(elev)
elev.moran<-MoranLocal(elev)
plot(elev.moran)


###########################
### Changing Projection ###
###########################

all_projected # are in UTM
all_projected<-projectRaster(all_layers, crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
elev_projected<-projectRaster(elev, crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )


######################################################
###  Extract values from raster using polygon, Slow!!!
######################################################
canopy
forest_g #### notice the different projection
## calculate the mean canopy height per forest type
forest_g$canopy<-extract(canopy,forest_g, fun=mean, na.rm=TRUE) # change the function
head(forest_g) # mean canopy by forest type

###########################
### Rasterize
###########################
forest_rast<-rasterize(forest_g,elev_projected,"GROUP")
plot(forest_rast)

point_rast<-rasterize(campoints, elev_projected)
plot(point_rast)

### agregate, change resolution
point_rast_lowres<-aggregate(point_rast, fact = 5, fun=mean)
plot(point_rast_lowres)

####  visualization in 3D using transparent layers
slopeT<-terrain(elev,opt="slope")
aspectT<-terrain(elev,opt="aspect")
hill<-hillShade(slopeT,aspectT,25,270)
plot(hill, col=grey(0:100/100),legend=FALSE,main="Volcan Barva")
plot(elev, col=rainbow(25,alpha=0.35),add=TRUE)

####################
###    Exporting     ####
####################

## adjust the path to the data directory
writeraster(hill,filename="hillshade.grd") #### !!!!!!!!!!!!!!!!
all_layers_table<-as.data.frame(all_layers)### convert to data frame
write.csv(all_layers_table,file="all_layers.csv")

########### Header Ascii grid
#     
# ncols         169
# nrows         382
# xllcorner     813515.0021541
# yllcorner     1119250.21741
# cellsize      100
# NODATA_value  NA
##############################

## Read result from Jorge's analysis. Data coming as data frame or vector
result<-read.csv("mapdata_result.csv",header = TRUE)


map<-elev # copy elev format on map
map[]<-result$occ_Tapir # drape occ_Tapir on map
map@data@names<-"Tapirus" # change name
Tapir<-map

map[]<-result$occ_Leopar # drape occ_Leopar on map
map@data@names<-"Leopardus" # change name
Leopar<-map

map[]<-result$occ_Maz # drape occ_Mazama on map
map@data@names<-"Mazama" # change name
Mazama<-map

## simple visualization 1 map as image
image(Tapir,col=cm.colors(length(unique(Tapir))))
plot(Tapir,) #plot as map

all_sp<-stack(Tapir,Leopar,Mazama) #stack all
plot(all_sp, nr=1) #view on 1 row

# view in lattice
spplot(as(all_sp, "SpatialGridDataFrame"),
       col.regions=rev(terrain.colors(255)))



#### The part Jorge was waiting for !!!

# To view in ggplot (Not efficient for large rasters, but nice!):
# To make it compatible with cameras trap points, transform latlong
Mazama # notice do not have cor. ref
proj4string(Mazama)<- utm16n
Mazama_ll<-projectRaster(Mazama, crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

# you need to convert your RasterLayer to a data.frame, but 1st you need to convert it to
# a SpatialPixelsDataFrame 
r.spdf <- as(Mazama_ll, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)

# then you can use ggplot2 to plot that object
library(ggplot2)
g <- ggplot(r.df, aes(x=x, y=y)) + geom_tile(aes(fill = Mazama)) + coord_equal()
print(g)


### add camera traps
g1<-  g + geom_point(data = data, aes(Longitude, Latitude), size = I(2), 
                     color = "red", na.rm = TRUE) 
g1

### Export to ArcGis for better editing 
##  first convert matrix in asc, then export. Use your data directory
Tapir.m<-asc.from.raster(Tapir)
export.asc(x=Tapir.m, file="D:\\TEAM\\Data\\VB\\covariatesLayers\\ToJorge\\NewRasters\\Tapir_pred.asc")

Leopar.m<-asc.from.raster(Leopar)
export.asc(x=Leopar.m, file="D:\\TEAM\\Data\\VB\\covariatesLayers\\ToJorge\\NewRasters\\Leopardus_pred.asc")

Mazama.m<-asc.from.raster(Mazama)
export.asc(x=Mazama.m, file="D:\\TEAM\\Data\\VB\\covariatesLayers\\ToJorge\\NewRasters\\Mazama_pred.asc")

##################################
# some fragment stats
######## fragstats from SDMTools
tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
                   0,0,1,0,1,0,0,0,0,0,
                   0,1,NA,1,0,1,0,0,0,1,
                   1,0,1,1,1,0,1,0,0,1,
                   0,1,0,1,0,1,0,0,0,1,
                   0,0,1,0,1,0,0,1,1,0,
                   1,0,0,1,0,0,1,0,0,1,
                   0,1,0,0,0,1,0,0,0,1,
                   0,0,1,1,1,0,0,0,0,1,
                   1,1,1,0,0,0,0,0,0,1),nr=10,byrow=TRUE) }
#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat
image(t(ccl.mat[10:1,]),col=c("grey",rainbow(length(unique(ccl.mat))-1)))
#calculate the patch statistics
ps.data = PatchStat(ccl.mat)
ps.data

# lets apply the fragstats to the forest map. 1st convert matrix.
forest_mat<-as.matrix(forest_rast)
forest_fragstat<-PatchStat(forest_mat)


