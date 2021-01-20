# WBM Open Source: Primary Component tracking analysis
# make figures based on the results of validation_660.pl
# Danielle S Grogan

### R Libraries

library(ggplot2)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#######################################################################################################################################
# maps and other global data

# Basin IDs
basin.id = raster("/net/nfs/zero/data3/WBM_TrANS/data/flowdirection602_IDs.asc")
# countries
countries = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/FAOCountryBoundaries2012/", "Adm012p")

# coastline
coastline = readOGR("/net/home/eos/dgrogan/git_repos/Glacier_ag/data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(countries))

#######################################################################################################################################
#  validation data
val.data = read.delim("/net/nfs/merrimack/web/earthatlas/htdocs/wbm_validation/Primary_components_v3_discharge_m/summary.csv")

#######################################################################################################################################
# sumary plots

# cutoff histogram at = -10 (set values < -10 to be -10)
val.nash.cut = unlist(lapply(val.data$NashSutcliffe, FUN = function(x){max(x, -10)}))

png("figures/Q_validation_NS_histogram.png", height=4, width=6, units='in', res=300)
hist(val.nash.cut, breaks=seq(-10, 1, 0.1), xlim=c(-10, 1.6), xlab = "Nash Sutcliffe Coefficient", main="", col=rainbow(170))
dev.off()



# MAP
val.data.pts = as.data.frame(cbind(val.data$Latitude, val.data$Longitude, val.nash.cut))
colnames(val.data.pts) = c("Latitude", "Longitude", "NSC")

all.col = rainbow(170)[1:111]
val.data.pts$col = all.col[as.numeric(cut(val.data.pts$NSC, breaks = bins))]
val.data.pts$col[is.na(val.data.pts$col)] = all.col[1]

# make spatial point file                                  
coordinates(val.data.pts) <- ~ Longitude + Latitude      

png("figures/Q_validation_NS_map.png", height=4, width=8, units='in', res=300)
plot(coastline, lwd=0.6)
plot(val.data.pts, add=T, col = val.data.pts$col, pch=19, cex=0.2)
dev.off()


