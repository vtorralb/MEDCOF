library(ncdf4)
library(RColorBrewer)
library(s2dverification)
library(multiApply)
library(easyVerification)
library(ClimProjDiags)
library(s2dv)
library(corplot)

years <- 1993:2016
#data <- readRDS('/home/veronica/BSS_066_080_090_allsystems_15MJJA_1993_2016.RDS')
data <- readRDS('/work/csp/vt17420/BSS_075_allsystems_15MJJA_1993_2016.RDS')
lats <- seq(25.5,69.5, by=1)
lons <- seq(-14.5, 59.5, by=1)
reg <- list(
  NE=c(-10,30,55,65),
  WE=c(-10,5,40,60),
  MED=c(-10,25,35,45),
  NAF=c(-10,12,25,36),
  CE=c(5,30,45,55),
  EE=c(30,60,40,60),
  ME=c(25,43,25,42),
  ALL=c(-15,60,25,70))


funaux <- function(x,la,lo,rg){
  reg <- SelBox(x, lat=as.vector(la),
                lon=as.vector(lo),
                region=rg)
  return(reg$data)
}

ngp <- array(dim=c(length(reg), 2,length(data)))
for (r in 1:length(reg)){
  for (var in 1:2){
    for (sys in 1:length(data)){
      data_exp <- data[[sys]][[var]]
      dataReg <- lapply(data_exp,funaux,la=lats,lo=lons,rg=reg[[r]])[[1]]
      ngp[r,var,sys] <- length(which(dataReg[1,,]>0))*100/(length(dataReg[1,,])-length(which(is.na(dataReg[1,,]))))
    }
  }
}

cairo_ps('~/number_of_grid_points_with_positive_BSS.ps',width=10, height=12)
#cols <- c('#984ea3','#ff7f00')
cols <- c('#fdbb84','#e34a33')
layout(matrix(1:8,ncol=2,byrow=T))
par(cex=0.8,mar=c(4,4,2,2),oma=c(2,2,4,2))
for (r in 1:length(reg)){
  barplot(ngp[r,,],beside = T,col=cols, ylim=c(0,100),ylab=' % points with BSS>0',main=names(reg)[r],
          names.arg = c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM'))
  box()
}
dev.off()

  cairo_ps(file='~/number_of_gp_bss_075_system.ps',width = 17,height = 5)
  #x11(width = 17,height = 5)
  #cairo_ps('~/number_of_grid_points_with_positive_BSS.ps',width=10, height=12)
  cols <- c('#984ea3','#ff7f00')
  #cols <- c('#fdbb84','#e34a33')
  layout(matrix(1:5,ncol=5,byrow=T),widths = c(4,4,4,4,4))
  par(cex=1,mar=c(4,0,2,0),oma=c(2,4,4,2))
  for (sys in 1:length(data)){
    if (sys==1){
      barplot(t(ngp[,,sys]),beside = T,col=cols, ylim=c(0,100),ylab=' % points with BSS>0',names.arg = names(reg),
              main = c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')[sys],las=2)
      abline(h=seq(20,80,by=20),col='#525252',lty=3)
    }else{
      barplot(t(ngp[,,sys]),beside = T,col=cols, ylim=c(0,100),ylab='',names.arg = names(reg),axes=F,
              main = c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')[sys],las=2)
      abline(h=seq(20,80,by=20),col='#525252',lty=3)
    }
    box()
  }
  dev.off()
# 
# cairo_ps('legend_barplot.ps')
# plot(1:10,type='n',axes=F,xlab='',ylab='')
# legend(x=1,y=5,legend=c('HWMI','NQ90'),fill = cols,horiz = T)
# dev.off()


#for (r in 1:length(reg)){
  #data_obs_reg <- lapply(data_obs,funaux,la=lats,lo=lons,rg=reg[[r]])
  
#probs <- c(0.66, 0.80, 0.90)
# 
# p <- 1
# var <- 1
# cairo_ps('~/BSS_sensitivity_threshold_MM_15MJJA_1993_2016.ps',width=12,height=8)
# par(oma=c(2,4,4,2))
# layout(matrix(c(1,2,3,4,5,6,7,7,7),ncol=3,byrow=T),heights=c(4.5,4.5,1.5))
# for (var in 1:2){
#   for (p in 1:length(probs)){
#     map <- data[[5]][[var]][[p]]
#     map[which(map=='-Inf')] <- -20
#     flags <- F*map[1,,]
#     flags[which(map[3,,]<=0.05)]=T
#     pal1 <-colorRampPalette(rev(brewer.pal(10,'RdBu')))(22)
#     pal1[11:12] <- c('white','white')
#     PlotEquiMap(map[1,,],lat=lats,
#                 lon=lons,filled.continents = F,
#                 dots=flags,
#                 dot_symbol = 16,
#                 dot_size = 1.5,
#                 cols=pal1[2:(length(pal1)-1)],
#                 col_sup=pal1[length(pal1)],
#                 #toptitle = 'BSS (prob >0.75)',
#                 col_inf = pal1[1],
#                 brks=seq(-1,1,by=0.1)/2,
#                 colNA='grey',drawleg=F)
#     map('world',add=T) 
#     if (var==1){mtext(side=3,text=c('BSS 0.66','BSS 0.8','BSS 0.9')[p],line=1,cex=1.5)}
#     if (p==1){mtext(side=2,text=c('HWMI','NQ90')[var],line=2,cex=1.5)}
#   }
# }
# 
# ColorBar(cols=pal1[2:(length(pal1)-1)],
#          col_sup=pal1[length(pal1)],
#          #toptitle = 'BSS (prob >0.75)',
#          col_inf = pal1[1],subsampleg = 2,label_scale = 1.5,draw_separators = T,
#          brks=seq(-1,1,by=0.1)/2,vertical = F)
# dev.off()

cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

#cbar_cmcc <- c("#BC01FF", "#6600CC", "#6666FF", "#4366D5" ,"#2166AC", "#3885BC" ,
#               "#62A6CD", "#98C8DF" ,"#C4DEEC", "#DAF2FE" ,"#FBD0B9","#F4AA88",
#               "#E27B62" ,"#CB4A42", "#B2182B", "#993404" ,"#cc6600" ,"#FFCA4D",
#               "#FFE426", "#FFFF00")

pal1 <- cbar_cmcc

labs <- c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)')
dim(labs) <- c(3,5)
cairo_ps('/work/csp/vt17420/BSS_075_allsys_MM_15MJJA_1993_2016_area.ps',width=12,height=16)
par(oma=c(2,4,4,2),mar=c(4,2,2,2))
layout(matrix(c(1:15,rep(16,2),17),ncol=3,byrow=T),heights=c(4.5,4.5,4.5,4.5,4.5,2))
p <- 2
  for (s in 1:5){
    for (var in 1:2){
    map <- data[[s]][[var]][[1]]
    map[which(map=='-Inf')] <- -20
    flags <- F*map[1,,]
    flags[which(map[3,,]<=0.05)]=T
    # pal1 <-colorRampPalette(rev(brewer.pal(10,'RdBu')))(22)
    # pal1[11:12] <- c('white','white')
    PlotEquiMap(map[1,,],lat=lats,
                lon=lons,filled.continents = F,
                dots=flags,
                dot_symbol = 16,
                dot_size = 1.5,
                cols=pal1[2:(length(pal1)-1)],
                col_sup=pal1[length(pal1)],
                #toptitle = 'BSS (prob >0.75)',
                col_inf = pal1[1],
                brks=seq(-0.45,0.45,by=0.05),
                colNA='grey',drawleg=F)
    
    text(x=-8,y=65,labs[var,s],font=2,cex=2)
    map('world',add=T) 
    if (var==1){mtext(side=2,text= c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')[s],line=3,cex=1.5,font=2)}
    if (s==1){mtext(side=3,text=c('HWMI','NQ90')[var],line=2,cex=1.5,font=2)}
    }
    par(mar=c(3,2,1,2))
    cols <- c('#984ea3','#ff7f00')
    barplot(t(ngp[,,s]),beside = T,col=cols, ylim=c(0,100),ylab='',names.arg = names(reg),cex.lab=0.8,
            #main = c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')[s],
            las=2)
    text(x=2,y=90,labs[3,s],font=2,cex=2)
    
    if (s==1){mtext(side=3,text= c('Area (%) with BSS > 0'),line=2,cex=1.5,font=2)}
    
    abline(h=seq(20,80,by=20),col='#525252',lty=3)
    box()
}



ColorBar(cols=pal1[2:(length(pal1)-1)],
         col_sup=pal1[length(pal1)],
         title = 'BSS (prob >0.75)',
         title_scale = 1.5,
         col_inf = pal1[1],subsampleg = 2,label_scale = 1.1,draw_separators = T,
         brks=seq(-0.45,0.45,by=0.05),vertical = F)
dev.off()
# 
# p <- 2
# 
# 
# cairo_ps(file='~/BSS_rank_system_ndq90_075.ps',width=12,height=12)
# names(dim(Maps)) <- c('map','lat','lon')
# var <- 2
# Maps <- array(dim=c(5,45,75))
# for (s in 1:5){
#   map <- data[[s]][[var]][[1]]
#   map[which(map=='-Inf')] <- -20
#   Maps[s,,] <- map[1,,]
# }
# names(dim(Maps)) <- c('map','lat','lon')
# PlotCombinedMap(maps=Maps,map_dim=1,map_select_fun = max,lon=lons,lat=lats,display_range = c(0,0.5),colNA='grey',
#                 bar_titles =  c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM'))
# 
# dev.off()
# 
# library(CSTools)
# cairo_ps(file='~/BSS_rank_system_hwmi_075.ps',width=12,height=12)
# names(dim(Maps)) <- c('map','lat','lon')
# var <- 1
# Maps <- array(dim=c(5,45,75))
# for (s in 1:5){
#   map <- data[[s]][[var]][[1]]
#   map[which(map=='-Inf')] <- -20
#   Maps[s,,] <- map[1,,]
# }
# names(dim(Maps)) <- c('map','lat','lon')
# PlotCombinedMap(maps=Maps,map_dim=1,map_select_fun = max,lon=lons,lat=lats,display_range = c(0,0.5),colNA='grey',
#                 bar_titles =  c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM'))
# 
# dev.off()
