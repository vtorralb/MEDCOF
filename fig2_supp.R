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
