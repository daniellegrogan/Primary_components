# WBM Open Source: Primary Component tracking analysis
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

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

# cell area
cell.area = raster::area(raster("/net/nfs/zero/data3/WBM_TrANS/data/flowdirection602.ascii"))
#######################################################################################################################################

# Analyses:

p = "/net/nfs/squam/raid/data/WBM_TrANS/WBM_OpenSource/Primary_components"

# 1. % global Q NOT rain
Q.yc = raster(file.path(p, "climatology/wbm_discharge_yc.nc"))
Q.pr.m3s = raster(file.path(p, "climatology/wbm_discharge_m3s_pr_yc.nc"))
Q.pr.frac = Q.pr.m3s/Q.yc
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

coastline = readOGR("/net/home/eos/dgrogan/git_repos/Glacier_ag/data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(Q.yc))

png("figures/Q_fraction_not_rain_NoTracking.png")
plot(Q.BF_not_rain.frac)
plot(coastline, add=T, lwd=0.5)
dev.off()

# 3. Largest contributor to Q, recharge, and human water use (a) with tracking, (b) without tracking
# 4. Bar chart: global water resources by source (a) with tracking, (b) without tracking
# 5. Basins: % runoff from return flows is composed mainly of non-rain components
# 6. Point extraction: show source of water in large reservoirs
# 7. Point extraction: show source of water at inter-basin transfer donor locations

#######################################################################################################################################
# 1. % global Q NOT rain




