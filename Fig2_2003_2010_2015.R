#############################################
# PLOT EXAMPLES+ DIFFERENCES ERA5 
# HWMI TMAX,TMIN,TNIGHT,ATEMPNIGHT
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
library(maps)

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
vars <- list()
for (v in 1:length(var_list)){
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
  vars[[v]] <- data
}

yr_list <- c(2003,2010,2015)
diff_atemp_tmin <-
  (vars[[4]][[2]] - vars[[2]][[2]])[which(years %in% yr_list), , ]
diff_atemp_tnight <-
  (vars[[4]][[2]] - vars[[3]][[2]])[which(years %in% yr_list), , ]

maps <- (vars[[4]][[2]])[which(years %in% yr_list), , ]


cairo_ps('/work/csp/vt17420/figures_HW_WN_indices/Figure2_hres.ps',width=12,height = 10)
par(oma=c(2,4,4,2))
layout(matrix(c(1:11,11),ncol=4,nrow=3,byrow=F),
       widths = c(4,4,4,1.5))
Cols <- colorRampPalette(brewer.pal(9,'YlOrRd'))(20)
col1 <- brewer.pal(11,'RdBu')
col1[6]<-'white'
for (y in 1:3){
  PlotEquiMap(var=maps[y,,],
              lat=mask1$lat,lon=mask1$lon,
              brks=seq(0,5,0.25),
              cols=Cols,
              col_inf = 'white',
              colNA='white',
              filled.oceans = 'lightgrey',
              col_sup='#5F031E', 
              #country.borders=T,
              drawleg = F,
              filled.continents=F)
  map('world',lwd=0.5,add = T)
  if (y==1){mtext('WNMI_atemp2m', side=2,font=2,line=3)}
  mtext(yr_list[y], side=3,line=1,font=2)
  PlotEquiMap(var=diff_atemp_tmin[y,,],
              lat=mask1$lat,lon=mask1$lon,
              brks=seq(-2.25,2.25,0.5),
              cols=col1[2:(length(col1)-1)],
              col_inf = col1[1],
              colNA='lightgrey',
              col_sup=col1[length(col1)], 
              #country.borders=T,
              filled.oceans = 'lightgrey',
              drawleg = F,
              filled.continents=F)
  map('world',lwd=0.5,add = T)
  if (y==1){mtext('WNMI_atemp2m - WNMI_tmin', side=2,font=2,line=3)}
  PlotEquiMap(var=diff_atemp_tnight[y,,],
              lat=mask1$lat,lon=mask1$lon,
              brks=seq(-2.25,2.25,0.5),
              cols=col1[2:(length(col1)-1)],
              col_inf = col1[1],
              colNA='lightgrey',
              col_sup=col1[length(col1)], 
              filled.oceans = 'lightgrey',
              #country.borders=T,
              drawleg = F,
              filled.continents=F)
   map('world',lwd=0.5,add = T)
   if (y==1){mtext('WNMI_atemp2m - WNMI_tnight', side=2,font=2,line=3)}

}
ColorBar(brks=seq(0,5,0.25),
         cols=Cols,
         extra_margin = c(1,0,0.5,0),
         subsampleg = 2,
         col_inf = 'white',
         col_sup='#5F031E',vertical = T,draw_separators = T)

ColorBar(brks=seq(-2.25,2.25,0.5),
         cols=col1[2:(length(col1)-1)],
         extra_margin = c(1,0,0.5,0),
         col_inf = col1[1],
         subsampleg = 1,
         col_sup=col1[length(col1)],
         vertical = T,
         draw_separators = T)
dev.off()
