library(s2dverification)
library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)
library(CSTools)

mask <- nc_open('~/mask_utc.nc')
lsm <- ncvar_get(mask,'mask')
nc_close(mask)
lsm[which(!is.na(lsm))]=1

# cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
#               '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
#               '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
#               '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')
# 
# path <-'/data/csp/vt17420/CLINT_proj/C3S/'
sys <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
vars <- c('atemp2m_night','tmin','t2m_night','tmax')
vars_names <- c('Tdisconfort','Tmin','Tnight','Tmax')
systems <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
# vars <- c('t2m','tmin','tmax')  
# vars_names <- c('T2m','Tmin','Tmax')
cairo_ps('~/summary_map_correlation_temperatures.ps',width=12,height = 12)
layout(matrix(1:4,ncol=2,byrow=T))
par(oma=c(4,4,2,2))
for (var in 1:length(vars)){
  sfile <- readRDS(paste0('~/C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
  corr_maps <- list((dim=c(45,75)))
  for (n in 1:5){
    m1 <- drop(sfile[[1]][[n]])[2,,]
    corr_maps[[n]] <- m1 * t(lsm)
  }
  
  PlotCombinedMap(maps=corr_maps,
                  lat=as.vector(sfile$lats),
                  lon=as.vector(sfile$lons),
                  drawleg = F,
                  # brks=seq(-0.45,0.45,by=0.05),
                  map_select_fun=max,
                  display_range=c(0,1),
                  brks=6,
                  bar_titles=systems,
                  #toptitle=vars_names[var],
                  #cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
                  #col_sup=cbar_cmcc[length(cbar_cmcc)],
                  #col_inf=cbar_cmcc[1],
                  #dots = flags,dot_symbol = 15,dot_size = 0.8,
                  colNA = 'white',
                  axelab = T)
  map('world',add=T,col='#525252')
  mtext(vars_names[var],side=3,cex=1.5)
}
dev.off()


cairo_ps('~/summary_map_rpss_temperatures.ps',width=12,height = 12)
#x11(width=12,height = 12)
layout(matrix(1:4,ncol=2,byrow=T))
par(oma=c(4,4,2,2))
for (var in 1:length(vars)){
  sfile <- readRDS(paste0('~/C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
  maps <- list((dim=c(45,75)))
  for (n in 1:5){
    m1 <- drop(sfile[[2]][[n]])[[1]]
    maps[[n]] <- m1 * lsm
  }
  
  PlotCombinedMap(maps=maps,
                  lat=as.vector(sfile$lats),
                  lon=as.vector(sfile$lons),
                  drawleg = F,
                  # brks=seq(-0.45,0.45,by=0.05),
                  map_select_fun=max,
                  display_range=c(0,1),
                  brks=6,
                  bar_titles=systems,
                  #toptitle=vars_names[var],
                  #cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
                  #col_sup=cbar_cmcc[length(cbar_cmcc)],
                  #col_inf=cbar_cmcc[1],
                  #dots = flags,dot_symbol = 15,dot_size = 0.8,
                  colNA = 'white',
                  axelab = T)
  map('world',add=T,col='#525252')
  mtext(vars_names[var],side=3,cex=1.5)
}
dev.off()
cairo_ps('~/summary_map_colorbar.ps',width=16,height = 10)

  PlotCombinedMap(maps=maps,
                  lat=as.vector(sfile$lats),
                  lon=as.vector(sfile$lons),
                  drawleg = T,
                  # brks=seq(-0.45,0.45,by=0.05),
                  map_select_fun=max,
                  display_range=c(0,1),
                  brks=6,
                  bar_titles=systems,
                  #toptitle=vars_names[var],
                  #cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
                  #col_sup=cbar_cmcc[length(cbar_cmcc)],
                  #col_inf=cbar_cmcc[1],
                  #dots = flags,dot_symbol = 15,dot_size = 0.8,
                  colNA = 'white',
                  axelab = T)
dev.off()
