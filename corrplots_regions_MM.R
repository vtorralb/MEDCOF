cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

labs <- c('NQ90','WNMI')
for (n in 1:length(labs)){
  cairo_ps(file=paste0('/work/csp/vt17420/figures_HW_WN_indices/regions/corrplot_regions_',labs[n],'.ps'),width=10,height = 6)
  M <- cor_vals[,,n,1]
  pvals <- cor_vals[,,n,2]
  colnames(M) <- names(reg)
  rownames(M) <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
  colnames(pvals) <- names(reg)
  rownames(pvals) <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
  p1 <- corrplot(M,col=cbar_cmcc,
                 #p.mat=pvals,
                 #sig.level=0.05,
                 method = 'color',tl.col="black",
                 cl.pos = 'b',
                 number.cex=1.2,
                 #insig = "n",
                 tl.cex = 1.2,
                 cl.cex = 1.2,
                 pch.cex = 1.2,pch.col="white",
                 addgrid.col = 'black', addCoef.col = 'black')
  
  v1 <- as.vector(pvals)
  text(x=p1$corrPos$x[which(v1<=0.05)]+0.3,
       y=p1$corrPos$y[which(v1<=0.05)]+0.1,
       '*',font=2,cex=1.2)
  dev.off()
}
