#############################################
# PLOT ERA5 HWMI TMAX,TMIN,TNIGHT,ATEMPNIGHT
# Europe ERA5 original resolution
# 1993-2022 (ref period 1993-2016)
# Veronica Torralba (11/2022)
#############################################
# cleaning:
rm(list=ls())

# Load libraries
library(ncdf4)
library(RColorBrewer)
library(ClimProjDiags)
library(s2dv)
library(multiApply)

# Path to save the outputs
path_out <- '/work/csp/vt17420/figures_HW_WN_indices/'

# Ocean mask
mask <- nc_open('/data/csp/vt17420/CLINT_proj/ERA5/ERA5_masks/ERA5_lsm.nc')
lsm <- ncvar_get(mask,'var172')
lon_mask <- mask$dim$lon$vals
lat_mask <- mask$dim$lat$vals
lsm[which(lsm<=0.1)]=NA
lsm[which(lsm>0.1)]=1
mask1 <- SelBox(lsm,lat=as.vector(lat_mask),
                lon=as.vector(lon_mask),
                region=c(-15,60,25,70))
nc_close(mask)
mask1$lon[which(mask1$lon>180)]<-  mask1$lon[which(mask1$lon>180)]-360
names(dim(mask1$data)) <- c('lon','lat')
#
# Load the seasonal indices: NDAYSQ90, HWMI
# for the four variables considered

ref <- '1993_2016' # this is the reference period (1993-2016/1981-2010)
path <- '/data/csp/vt17420/CLINT_proj/CLINT_PRODUCTS/'
var_list <- c('tmax','tmin','t2m_night','atemp2m_night')
years <- 1993:2022
v <- 4
#for (v in c(1,4)){
#for (v in 1:length(var_list)){
#for (y in 1:length(years)){
data_var1 <- data_var2 <- array(dim=c(length(years),301,181))
for (y in 1:length(years)){
  v1 <- paste0('nbdaygtpercentpct_',var_list[v])
  v2 <- paste0('HWMI_',var_list[v])
  file <- paste0('ERA5_HWMI_',var_list[v],'/ref_',ref,'/HWMI_',var_list[v],'_Europe_15MJJA_CV_percent90_daymin3_ref_',ref,'_year_',years[y],'.nc')
  ncold <- nc_open(paste0(path,file))
  lats <- ncold$dim$lat$vals
  lons <- ncold$dim$lon$vals
  #print(file)
  data_var1[y,,] <- ncvar_get(ncold,v1)
  data_var2[y,,] <- ncvar_get(ncold,v2)
  
  if (lats[1] != mask1$lat[1]){
    data_var1[y,,] <- data_var1[y,,length(lats):1]
    data_var2[y,,] <- data_var2[y,,length(lats):1]
  }
  nc_close(ncold) 
}
names(dim(data_var1)) <- names(dim(data_var2)) <- c('time','lon','lat')
data <- list(data_var1,data_var2)
names(data) <- c(v1,v2)
data$lat <- lats
data$lon <- lons
  
data_reg <- SelBox(data[[1]],lat=as.vector(rev(data$lat)),lon=as.vector(data$lon),
                   region=c(-15,60,25,53))
data_reg$HWMI <- SelBox(data[[2]],lat=as.vector(rev(data$lat)),lon=as.vector(data$lon),
                   region=c(-15,60,25,53))$data
years <- 1993:2022
for (yr in 1:length(years)){
  cairo_ps(paste0('/work/csp/vt17420/TNMI/TNMI_Med_',years[yr],'.ps'),width = 12,height = 8)
  col1 <- colorRampPalette(brewer.pal(9,'YlOrRd'))(20)
  PlotEquiMap(var=data_reg$HWMI[yr,,],
              lat=data_reg$lat,
              lon=data_reg$lon,
              brks=seq(0,5,0.25),
              cols=col1,
              colNA='white',
              col_sup='#5F031E',
              intylat = 10,
              intxlon = 10,
              toptitle = paste0(years[yr],'- 15MJJ'),
              bar_label_scale = 1.3,draw_separators = T,
              subsampleg = 2,
              filled.continents=F)
  dev.off()
}
