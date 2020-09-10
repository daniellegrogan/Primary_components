# WBM Open Source: Primary Component tracking analysis
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

### Source functions from within this project:
# file.sources = list.files("src/functions", full.names = T)
# sapply(file.sources, source)

#######################################################################################################################################
# maps and other global data

# Basin IDs
basin.id = raster("/net/nfs/zero/data3/WBM_TrANS/data/flowdirection602_IDs.asc")
# countries
countries = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/FAOCountryBoundaries2012/", "Adm012p")

# Dams & reservoirs
reservoirs = as.data.frame(read.delim("/net/nfs/zero/home/WBM_TrANS/spreadsheets/GRanD_dams_v1_1.csv", header=T, sep="\t"))
coordinates(reservoirs) = ~ LONG_DD + LAT_DD

# Inter-basin transfers
ibt = as.data.frame(read.delim("/net/nfs/zero/home/WBM_TrANS/spreadsheets/InterBasinWaterTransferDatabase.csv", header=T, skip = 7, sep="\t"))
ibt = subset(ibt, is.na(ibt$STN6.From.Latitude)  == F)
ibt = subset(ibt, is.na(ibt$STN6.From.Longitude) == F)
coordinates(ibt) = ~ STN6.From.Longitude + STN.From.Latitude

# coastline
coastline = readOGR("/net/home/eos/dgrogan/git_repos/Glacier_ag/data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(countries))

# cell area
cell.area = raster::area(raster("/net/nfs/zero/data3/WBM_TrANS/data/flowdirection602.ascii"))
#######################################################################################################################################

# Analyses:

p = "/net/nfs/squam/raid/data/WBM_TrANS/WBM_OpenSource/Primary_components_v3"

# 1. % global Q NOT rain
Q.yc = raster(file.path(p, "climatology/wbm_discharge_yc.nc"))
Q.pr.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pr_yc.nc"))
Q.ps.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_ps_yc.nc"))
Q.pg.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pg_yc.nc"))
Q.pu.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pu_yc.nc"))

Q.yc[Q.yc < 1] = NA
Q.pr.frac = Q.pr.m3s/Q.yc
Q.ps.frac = Q.ps.m3s/Q.yc
Q.pg.frac = Q.pg.m3s/Q.yc
Q.pu.frac = Q.pu.m3s/Q.yc

plot(Q.pr.frac)
plot(Q.ps.frac)
plot(Q.pg.frac)
plot(Q.pu.frac)

plot(Q.pr.m3s)
plot(Q.ps.m3s)
plot(Q.pg.m3s)
plot(Q.pu.m3s)

blues <-colorRampPalette(c(brewer.pal(n=9, name='Blues')))(100)
Q.ps.frac[Q.ps.frac == 0] = NA
png("figures/Q_ps_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(Q.ps.frac, col=blues, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

bupu <-colorRampPalette(c(brewer.pal(n=9, name='BuPu')))(100)
Q.pg.frac[Q.pg.frac == 0] = NA
png("figures/Q_pg_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(Q.pg.frac, col=bupu, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

greens <-colorRampPalette(c(brewer.pal(n=9, name='Greens')))(100)
Q.pu.frac[Q.pu.frac == 0] = NA
png("figures/Q_pu_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(Q.pu.frac, col=greens, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

ylorbr <-colorRampPalette(c(brewer.pal(n=9, name='YlOrBr')))(100)
Q.pr.frac[Q.pr.frac == 0] = NA
png("figures/Q_pr_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(Q.pr.frac, col=ylorbr, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()



Q.not_rain.frac = 1 - Q.pr.frac

png("figures/Q_not_rain.png")
plot(Q.not_rain.frac)
dev.off()

# 2. % global Q not rain, how it would look without tracking
# components in the river
Q.pg.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pg_yc.nc"))
Q.ps.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_ps_yc.nc"))
Q.pu.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pu_yc.nc")) # Assume all UGW would be missed without tracking

# components entering river via baseflow (this part would not be seen without tracking)
BF.pg.mm = raster(file.path(p, "climatology/wbm_baseflow_mm_pg_yc.nc"))
BF.ps.mm = raster(file.path(p, "climatology/wbm_baseflow_mm_ps_yc.nc"))
BF.pr.mm = raster(file.path(p, "climatology/wbm_baseflow_mm_pr_yc.nc"))
BF.mm    = raster(file.path(p, "climatology/wbm_baseflow_yc.nc"))
BF.pr.frac = BF.pr.mm/BF.mm
BF.not_rain.frac = 1 - BF.pr.frac

png("figures/Baseflow_not_rain.png")
plot(BF.not_rain.frac)
dev.off()

# convert baseflow from mm to m3/s
# mm x (m/mm) x (m2/km2) x (km2 cell area) x (day/sec) = m3/sec
m_per_mm = 0.001
m2_per_km2 = 1e6
day_per_sec = 1/86400

BF.m3s = BF.mm * m_per_mm * m2_per_km2 * cell.area * day_per_sec

plot(BF.m3s/Q.yc)

BF.not_rain.m3s = BF.m3s * BF.not_rain.frac
Q.BF_not_rain.frac = BF.not_rain.m3s/Q.yc
plot(Q.BF_not_rain.frac)

png("figures/Q_fraction_not_rain_NoTracking.png")
plot(Q.BF_not_rain.frac)
plot(coastline, add=T, lwd=0.5)
dev.off()

greens <-colorRampPalette(c(brewer.pal(n=9, name='Greens')))(100)
png("figures/Baseflow_not_rain_NoTracking.png",
    height=8, width=12, units = 'in', res=300)
plot(BF.not_rain.frac, col=greens, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

Q.BF_not_rain.frac[Q.BF_not_rain.frac==0]=NA
png("figures/Q_fraction_not_rain_NoTracking.png",
    height=8, width=12, units = 'in', res=300)
plot(Q.BF_not_rain.frac, col=greens, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

# 3. Largest contributor to Q, recharge, and human water use (a) with tracking, (b) without tracking
# 4. Bar chart: global water resources by source (a) with tracking, (b) without tracking
# 5. Basins: % runoff from return flows is composed mainly of non-rain components
# 6. Point extraction: show source of water in large reservoirs
# 7. Point extraction: show source of water at inter-basin transfer donor locations

#######################################################################################################################################
# Largest contributor to Q

Q_fracs = stack(Q.pr.frac, Q.ps.frac, Q.pg.frac, Q.pu.frac)
Q_max_layers = whiches.max(Q_fracs)

terrain.colors(4, alpha=1)
brewer.pal(5, "Set1")

png("figures/Q_main_source.png",
    height=8, width=12, units = 'in', res=300)
plot(Q_max_layers, col = c("darkorange", "dodgerblue", "darkolivegreen", "purple"), box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()


# Largest contributor to irrigation
irr.yc = raster(file.path(p, "climatology/wbm_irrigationGross_yc.nc"))
irr.pr.mm = raster(file.path(p, "climatology/wbm_GrossIrr_mm_pr_yc.nc"))
irr.ps.mm = raster(file.path(p, "climatology/wbm_GrossIrr_mm_ps_yc.nc"))
irr.pg.mm = raster(file.path(p, "climatology/wbm_GrossIrr_mm_pg_yc.nc"))
irr.pu.mm = raster(file.path(p, "climatology/wbm_GrossIrr_mm_pu_yc.nc"))

irr.yc[irr.yc < 1] = NA
irr.pr.frac = irr.pr.mm/irr.yc
irr.ps.frac = irr.ps.mm/irr.yc
irr.pg.frac = irr.pg.mm/irr.yc
irr.pu.frac = irr.pu.mm/irr.yc

blues <-colorRampPalette(c(brewer.pal(n=9, name='Blues')))(100)
irr.ps.frac[irr.ps.frac == 0] = NA
png("figures/Irr_ps_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(irr.ps.frac, col=blues, box=F, axes=T, las=1, ylim=c(-51,90), zlim=c(0,1))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

bupu <-colorRampPalette(c(brewer.pal(n=9, name='BuPu')))(100)
irr.pg.frac[irr.pg.frac == 0] = NA
png("figures/Irr_pg_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(irr.pg.frac, col=bupu, box=F, axes=T, las=1, ylim=c(-51,90), zlim=c(0,1))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

greens <-colorRampPalette(c(brewer.pal(n=9, name='Greens')))(100)
irr.pu.frac[irr.pu.frac == 0] = NA
png("figures/Irr_pu_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(irr.pu.frac, col=greens, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

ylorbr <-colorRampPalette(c(brewer.pal(n=9, name='YlOrBr')))(100)
irr.pr.frac[irr.pr.frac == 0] = NA
png("figures/Irr_pr_fraction.png",
    height=8, width=12, units = 'in', res=300)
plot(irr.pr.frac, col=ylorbr, box=F, axes=T, las=1, ylim=c(-51,90))
plot(coastline, border='grey50', lwd=1, add=T)
dev.off()

#######################################################################################################################################
# global figures for N-SLCT
Q.yc = raster(file.path(p, "climatology/wbm_discharge_yc.nc"))
Q.pg.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pg_yc.nc"))
irr.pg = raster(file.path(p, "climatology/wbm_GrossIrr_mm_pg_yc.nc"))*365
irr.pu  = raster(file.path(p, "climatology/wbm_GrossIrr_mm_pu_yc.nc"))*365
ugw = raster(file.path(p, "climatology/wbm_irrigationExtra_yc.nc"))*365
Q.pu.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pu_yc.nc"))
glMelt = raster(file.path(p, "climatology/wbm_glMelt_yc.nc"))
plot(glMelt > 0)
plot(Q.pg.m3s, add=T)
plot(irr.pg, add=T)

coastline = spTransform(coastline, crs(Q.yc))
coastline = crop(coastline, extent(Q.yc))

glacier.icemelt   = glMelt
glacier.icemelt[glacier.icemelt == 0] = NA
GrossIrr.pgi      = irr.pg
discharge.pgi     = Q.pg.m3s

discharge.pgi[discharge.pgi < 1] = c(NA) # lower limit on discharge; else it covers all areas and other display items
GrossIrr.pgi[GrossIrr.pgi < 0.01] = c(NA) # lower limit on discharge; else it covers all areas and other display items

# color scales 
purples<-colorRampPalette(c(brewer.pal(n=9, name='Purples')))(100)
p1 = purples[30:100]

greens<-colorRampPalette(c(brewer.pal(n=9,name='Greens')))(100)
g1 = greens[20:100]

blues2<-colorRampPalette(c("cadetblue2", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))(100)


png("figures/Global_glacier_tracking.png",
    height=8, width=12, units = 'in', res=300)
par(mar=c(7, 4, 0.5, 0.5))
#plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline, border='grey70', col='white',  lwd=1)
plot(GrossIrr.pgi,      add=T, col = g1,     legend=F, bty='n', box=F, axes=F)
plot(discharge.pgi,     add=T, col = blues2, legend=F, bty='n', box=F, axes=F)
plot(glacier.icemelt>0,   add=T, col = p1,     legend=F,          box=F, axes=T, las=1)
#plot(basins, xlim = xl, ylim = yl, add=T,  lwd=0.8, border='grey15')

plot(discharge.pgi, add=T, col = blues2, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, 
     smallplot=c(0.32, 0.62, 0.07, 0.09),
     axis.args=list(cex.axis=0.8),
     legend.args=list(text='Glacier ice melt in discharge (m3/s)', side=3, font=1, line=0.05, cex=0.8))

plot(GrossIrr.pgi, add=T, col = g1, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, 
     smallplot=c(0.67, 0.97, 0.07, 0.09),
     axis.args=list(cex.axis=0.8),
     legend.args=list(text='Glacier ice melt in irrigation (mm/year)', side=3, font=1, line=0.05, cex=0.8))

dev.off()




#######

ugw[ugw == 0] = NA
GrossIrr.pgu      = irr.pu
discharge.pgu     = Q.pu.m3s

discharge.pgu[discharge.pgu < 5] = c(NA) # lower limit on discharge; else it covers all areas and other display items
GrossIrr.pgu[GrossIrr.pgu < 0.01] = c(NA) # lower limit on discharge; else it covers all areas and other display items


png("figures/Global_UGW_tracking.png",
    height=8, width=12, units = 'in', res=300)
par(mar=c(7, 4, 0.5, 0.5))
#plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline, border='grey70', col='white',  lwd=1)
plot(ugw,   add=T, col = p1,     legend=F,          box=F, axes=T, las=1)
plot(GrossIrr.pgu,      add=T, col = g1,     legend=F, bty='n', box=F, axes=F)
plot(discharge.pgu,     add=T, col = blues2, legend=F, bty='n', box=F, axes=F)
#plot(basins, xlim = xl, ylim = yl, add=T,  lwd=0.8, border='grey15')

# plot(ugw,    add=T, col = p1, box=F, axes=T, las=1,
#      legend.only=T, legend.width=0.4, horizontal=T, 
#      smallplot=c(0.02, 0.28, 0.07, 0.09),
#      axis.args=list(cex.axis=0.8),
#      legend.args=list(text='Unsust. Groundwater (mm/year)', side=3, font=1, line=0.05, cex=0.8))

plot(discharge.pgu, add=T, col = blues2, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, 
     smallplot=c(0.32, 0.62, 0.07, 0.09),
     axis.args=list(cex.axis=0.8),
     legend.args=list(text='Unsust. Groundwater in discharge (m3/s)', side=3, font=1, line=0.05, cex=0.8))

plot(GrossIrr.pgu, add=T, col = g1, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, 
     smallplot=c(0.67, 0.97, 0.07, 0.09),
     axis.args=list(cex.axis=0.8),
     legend.args=list(text='Unsust. Groundwater in irrigation (mm/year)', side=3, font=1, line=0.05, cex=0.8))

dev.off()
#################################################

# transects of largest rivers
basin.id = raster("/net/nfs/zero/data3/WBM_TrANS/data/flowdirection602_IDs.asc")

