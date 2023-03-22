  #############################################
  # PLOT INDIVIDUAL MAPS
  # HWMI TMAX,TMIN,TNIGHT,ATEMPNIGHT
  # Europe ERA5 original resolution
  # 1993-2022 (ref period 1993-2016)
  # Veronica Torralba (02/2023)
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
  
  
  ind <- c('NDAYSQ90','HWMI')
  i <- 2
  var_tit <- c('tmax','tmin','tnight','app_tnight')
  for (year in years){
    y <- which(years==year)
    for (v in 1:length(var_tit)){
      fname=paste0('/work/csp/vt17420/ERA5_',ind[i],'_',var_tit[v],'_',year,'.ps')
      postscript(file=fname,width=12,height = 8)
      brk1 <- seq(0,5,0.5)
      Cols <- colorRampPalette(brewer.pal(9,'YlOrRd'))(10)
      Cols[1:3] <- rep('white',3)
      layout(matrix(c(1:2),ncol=2,nrow=1,byrow=F),widths = c(4,1))
      PlotEquiMap(var=vars[[v]][[i]][y,,],
                  lat=mask1$lat,lon=mask1$lon,
                  brks=brk1,
                  cols=Cols,
                  toptitle = paste0('ERA5-',ind[i],'-',var_tit[v],'-',year),
                  col_inf = 'white',
                  colNA='white',
                  filled.oceans = 'white',
                  coast_color = 'white',
                  coast_width = 0.2,
                  col_sup='#5F031E',
                  #country.borders=T,
                  drawleg = F,
                  filled.continents=F)
      map('world',lwd=1,add = T)
      ColorBar(brks=brk1[4:length(brk1)],
               cols=Cols[4:length(Cols)],
               extra_margin = c(1,0.5,1,1),
               subsampleg = 1,
               label_scale = 1.3,
               #col_inf = 'white',
               col_sup='#5F031E',vertical = T,draw_separators = T)
      dev.off()
    }
  }
