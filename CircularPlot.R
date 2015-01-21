# Plots based on the cirlize package
#     Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
# The code is adapted from the one provided by the migest package :   
#     Guy J. Abel (2014). migest: Useful R code for the Estimation of Migration. R package version 1.6.
#     http://CRAN.R-project.org/package=migest
# The myCirclePlot function expect:
# df: a data frame of links (origin, destination, weight of the flow), 
# marg: a numeric value between 0 and 0.4 to set the space dedicated to the names of the regions,
# nflows:  an integer to set the number of flows to plot


myCirclePlot <- function(df, marg, nflows){
  library(circlize)
  library(reshape2)
  names(df) <- c("i", "j", "fij")
  region <- data.frame (id = unique(c(unique(df$i), unique(df$j))), x = 0)
  m <- merge(region, region, by = "x")
  m <- m[,2:3]
  names(m) <- c("i", "j")
  mat <- merge(m,df, by = c("i", "j"), all.x = T  )
  mat <- dcast(mat,i~j,value.var="fij", fun.aggregate = sum, na.rm=T)
  lim <- mat$i
  com <- colnames(mat)[-1]
  mat <- as.matrix(mat[,-1])
  dimnames(mat)<-list(orig=lim,dest=com)
  region$id <- factor(region$id, levels = region$id)
  mat <- mat[levels(region$id),levels(region$id)]
  region$xmin <- 0
  region$xmax <- rowSums(mat)+colSums(mat)
  x <- sample(x = nrow(region), replace = F)
  region$rcol<- rainbow(nrow(region))[x]
  region$lcol<- rainbow(nrow(region), alpha = 0.8)[x]
  par(mar=rep(0,4))
  circos.clear()
  circos.par(cell.padding=c(0,0,0,0), start.degree = 45, gap.degree =4, track.margin=c(-marg,marg))
  circos.initialize(factors = region$id, xlim = cbind(region$xmin, region$xmax))
  circos.trackPlotRegion(ylim = c(0, 1), factors = region$id, track.height=0.1, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                         panel.fun = function(x, y) {
                           name = get.cell.meta.data("sector.index")
                           i = get.cell.meta.data("sector.numeric.index")
                           xlim = get.cell.meta.data("xlim")
                           ylim = get.cell.meta.data("ylim")
                           theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                           dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                           aa = c(1, 0.5)
                           if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                           circos.text(x=mean(xlim), y=1.1, labels=name, facing = dd, cex=0.8,  adj = aa)
                           circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                       col = region$rcol[i], border="white")
                           circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(mat)[i], ytop=ylim[1]+0.3, 
                                       col = "white", border = "#ffffff00")
                           circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         })
  region$sum1 <- colSums(mat)
  n <- nrow(region)
  region$sum2 <- numeric(n)
  df2 <- cbind(as.data.frame(mat),orig=rownames(mat),  stringsAsFactors=FALSE)
  df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long", timevar="dest", time=rownames(mat),  v.names = "m")
  df2 <- df2[order(df2$m, decreasing = T), ]
  if (nflows <= nrow(df2)){  
    df2 <- df2[1:nflows,]
  }
  for(k in 1:nrow(df2)){
    i<-match(df2$orig[k],region$id)
    j<-match(df2$dest[k],region$id)
    circos.link(sector.index1=region$id[i], point1=c(region$sum1[i], region$sum1[i] + abs(mat[i, j])),
                sector.index2=region$id[j], point2=c(region$sum2[j], region$sum2[j] + abs(mat[i, j])),
                col = region$lcol[i])
    region$sum1[i] = region$sum1[i] + abs(mat[i, j])
    region$sum2[j] = region$sum2[j] + abs(mat[i, j])
  }
}



