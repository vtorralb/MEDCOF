library(ncdf4)
library(RColorBrewer)
library(s2dv)
library(ClimProjDiags)
library(maps)

ref <- '1993-2016'
path <- '/data/csp/vt17420/CLINT_proj/CLINT_products_low/'
var_list <- c('tmax','tmin','t2m_night','atemp2m_night')
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


diff <- list()
for (y in 1:length(years)){
  diff[[y]] <- data[[y]][[2]][[1]]
}

brk <- seq(0,10,by=1)/2
cols <- colorRampPalette(c(brewer.pal(9,'YlOrRd')))(11)
path_out <- '/work/csp/vt17420/figures_HW_WN_indices/'
file_out <- paste0(path_out,'ERA5_HWMI_4vars_ref_',ref,'_vars_2003_2010_2022.ps')
print(file_out)
cairo_ps(file_out,width =10,height = 12)
layout(matrix(c(1:12,13,13,13),ncol=3,byrow=T),
       heights = c(4,4,4,4,1.5))
par(oma=c(2,2,2,2))
for (v in 1:length(var_list)){
for (y in 1:length(years)){
    dat=data[[y]][[2]][[v]]
    PlotEquiMap(dat,lat = lats,
                lon=lons1,drawleg = F,
                brks=brk,cols=cols[1:(length(cols)-1)],
                filled.continents = F,colNA = 'white',
                filled.oceans = 'white',
                col_sup = cols[length(cols)],axelab = T)
    map('world',add=T)
    if (y==1){mtext(side=2,c('Tmax','Tmin','Tnight','Tdiscomfort')[v],
                   font = 2,line=3)}
    if (v==1){mtext(side=3,years[y],font=2)}
  }
}

ColorBar(brks=brk,cols=cols[1:(length(cols)-1)],col_sup = cols[length(cols)],
         draw_separators = T,vertical = F,title = 'HWMI',
         extra_margin = c(0,1,0,0))
dev.off()




cairo_ps(paste0(path_out,'ERA5_NBDAYSQ90_4vars_ref_',ref,'_2003_2010_2015.ps'),
      width=10,height=12)
brk <- seq(0,10,by=1)*5
cols <- colorRampPalette(c(brewer.pal(9,'YlGn')))(11)
layout(matrix(c(1:12,13,13,13),ncol=3,byrow=T),
       heights = c(4,4,4,4,1.5))
par(oma=c(2,2,2,2))
for (v in 1:length(var_list)){
  for (y in 1:length(years)){
    dat=data[[y]][[1]][[v]]/1.09
    PlotEquiMap(dat,lat = lats,
                lon=lons1,drawleg = F,
                brks=brk,cols=cols[1:(length(cols)-1)],
                filled.continents = F,colNA = 'white',
                filled.oceans = 'white',
                col_sup = cols[length(cols)],axelab = T)
    map('world',add=T)
    if (y==1){mtext(side=2,c('Tmax','Tmin','Tnight','Tdiscomfort')[v],
                    font = 2,line=3)}
    if (v==1){mtext(side=3,years[y],font=2)}
  }
}
ColorBar(brks=brk,cols=cols[1:(length(cols)-1)],col_sup = cols[length(cols)],
         draw_separators = T,vertical = T,
         title = 'Percentage of days above the 90th climatological percentile (%)',
         extra_margin = c(1.5,0,0,0))
dev.off()


