library(s2dv)
library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)
library(maps)

mask <- nc_open('/data/csp/vt17420/CLINT_proj/ERA5/ERA5_masks/mask_utc.nc')
lsm <- ncvar_get(mask,'mask')
nc_close(mask)
lsm[which(!is.na(lsm))]=1

# cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
#               '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
#               '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
#               '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')


cbar_cmcc <- c("#BC01FF", "#6600CC", "#6666FF", "#4366D5" ,"#2166AC", "#3885BC" ,
               "#62A6CD", "#98C8DF" ,"#C4DEEC", "#DAF2FE" ,"#FBD0B9","#F4AA88",
               "#E27B62" ,"#CB4A42", "#B2182B", "#993404" ,"#cc6600" ,"#FFCA4D",
               "#FFE426", "#FFFF00")

vars <- list('atemp2m_night','tmin','t2m_night','tmax')
var_labs <- c('ATn','Tmin','Tn','Tmax')
systems <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7','mm')

path1 <- '/data/csp/vt17420/CLINT_proj/C3S/'
indices <- c('HWMI','NDQ90')
for (n in 1:length(indices)){
  cairo_ps(file=paste0('/work/csp/vt17420/Corrleation_C3S_mm_all_vars_',indices[n],'.ps'),width=12,height=14)
  layout(matrix(c(1:5,21,6:10,21,11:15,21,16:21),ncol=4,byrow=F),heights = c(4,4,4,4,4,2))
  par(oma=c(2,2,4,2))
  for (v in 1:length(vars)){
    var <- vars[v]
    data <- readRDS(paste0(path1,'C3S_mm_ndays_hwmi_seasonal_mean_05_15MJJ_',var,'_1993_2016.RDS'))
    lats <- seq(25.5,69.6,by=1)
    lons <- seq(-14.5,59.5,by=1)
    #for (n in 1:2){
    for (sys in 1:length(systems)){
      skill_scores <- data[[sys]]
      flags <- NA*drop(skill_scores[[n]][[1]])[2,,]
      flags[which(drop(skill_scores[[n]][[1]])[2,,]> drop(skill_scores[[n]][[1]])[4,,])]=1
      maps <- drop(skill_scores[[n]][[1]])[2,,] * flags
      PlotEquiMap(maps,lat =lats,lon=lons,drawleg = F,
                  brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
                  filled.oceans='lightgrey',
                  #margin_scale = c(1,1,2,1),
                  #dots = flags,dot_symbol = 15,dot_size = 0.8,
                  filled.continents = F,
                  colNA = 'white',
                  axelab = T)
      map('world',add=T,col='#525252')
      if (v==1){mtext(side=2,c('CMCC-35','DWD-21', 'ECMWF-5','MF-7','MM')[sys],line=2,font=2)}
      if (sys==1){mtext(side=3,var_labs[v],line=2,font=2)}
    }
  }
  ColorBar(brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
           extra_margin = c(2,0,0,0),
           subsampleg = 1,
           label_scale = 1,
           draw_separators = T,vertical = F)
  dev.off()
}
