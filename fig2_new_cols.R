library(ncdf4)
library(RColorBrewer)
library(s2dv)
library(ClimProjDiags)
library(maps)

ref <- '1993-2016'
path <- '/data/csp/vt17420/CLINT_proj/CLINT_products_low/'
var_list <- c('tmin','t2m_night','tmax','atemp2m_night')
years <- c(2003,2010,2015)
data <- list()
for (y in 1:length(years)){
  data_var1 <- data_var2 <- list()
  for (v in 1:length(var_list)){
    v1 <- paste0('nbdaygtpercentpct')
    v2 <- paste0('HWMI_',var_list[v],'_Europe_15MJJA_CV_percent90_daymin3_ref1993-2016_0')
    #v2 <- paste0('HWMI_',var_list[v])
    file <- paste0('ERA5_HWMI_',var_list[v],'/HWMI_',var_list[v],'_Europe_15MJJA_CV_percent90_daymin3_ref',ref,'_0_',years[y],'.nc')
    ncold <- nc_open(paste0(path,file))
    data_var1[[v]] <- ncvar_get(ncold,v1)
    data_var2[[v]] <- ncvar_get(ncold,v2)
    lats <- ncold$dim$lat$vals
    lons <- ncold$dim$lon$vals
    print(lats)
    print(lons)
    nc_close(ncold)
  }
  names(data_var1) <- names(data_var2)  <- var_list
  data[[y]] <- list(NBDAYSQ90=data_var1,HWMI=data_var2)
}

lons1 <- lons
lons1[which(lons>180)] <- lons1[which(lons>180)]-360


Diff <- list()
for (y in 1:length(years)){
  diff <- list()
  for (n in 1:3){
    diff[[n]] <- data[[y]][[2]][[4]]-data[[y]][[2]][[n]]
  }
  Diff[[y]] <- diff
}
brk1 <- seq(1.5,5,0.25)
#x11(width=12,height = 12)
cairo_ps('/work/csp/vt17420/figures_HW_WN_indices/Figure2_new.ps',width=12,height = 12)
par(oma=c(2,4,4,2))
layout(matrix(c(1:14,14,14),ncol=4,nrow=4,byrow=F),
       widths = c(4,4,4,1.5))
Cols <- colorRampPalette(brewer.pal(9,'YlOrRd'))(14)
col2 <- rev(brewer.pal(11,'RdBu'))
brk2 <- seq(-2,2,by=0.5)
col1 <- colorRampPalette(col2[c(1:5,8:11)])(10)
col1[6] <- '#F9B59E'

for (y in 1:3){
  PlotEquiMap(var=data[[y]][[2]][[4]],
              lat=lats,lon=lons1,
              brks=brk1,
              cols=Cols,
              col_inf = 'white',
              colNA='white',
              coast_width = 0.1,
              filled.oceans = '#E7E5E5',
              col_sup='#5F031E', 
              #country.borders=T,
              drawleg = F,
              filled.continents=F)
  map('world',lwd=0.5,add = T)
  if (y==1){mtext('HWMI_ATn', side=2,font=2,line=3)}
  mtext(years[y], side=3,line=1,font=2)
  for (n in 1:3){
    PlotEquiMap(var=Diff[[y]][[n]],
                lat=lats,lon=lons1,
                brks=brk2,
                coast_width = 0.1,
                cols=col1[2:(length(col1)-1)],
                col_inf = col1[1],
                colNA='lightgrey',
                col_sup=col1[length(col1)], 
                #country.borders=T,
                filled.oceans = '#E7E5E5',
                drawleg = F,
                filled.continents=F)
    map('world',lwd=0.5,add = T)
    if (y==1){mtext(c('HWMI_ATn - HWMI_Tmin', 'HWMI_ATn - HWMI_Tn', 'HWMI_ATn - HWMI_Tmax'
    )[n], side=2,font=2,line=3)}
  }
}
ColorBar(brks=brk1,
         cols=Cols,
         extra_margin = c(1,0,0,0),
         subsampleg = 2,
         col_inf = 'white',
         col_sup='#5F031E',vertical = T,draw_separators = T)

ColorBar(brks=brk2,
         cols=col1[2:(length(col1)-1)],
         extra_margin = c(1,0,0.5,0),
         col_inf = col1[1],
         subsampleg = 1,
         col_sup=col1[length(col1)],
         vertical = T,
         draw_separators = T)
dev.off()

