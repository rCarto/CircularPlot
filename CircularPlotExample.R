load("data/regmig.RData")
source("CircularPlot.R")

## Circular plot of all residential migrations
# png("img/CircularPlotAll.png", width = 500, height = 500)
myCirclePlot(df = df, marg = 0.4, nflows = 40)
# dev.off()

## Circular plot of all residential migrations except those coming from abroad
df2 <- df[df$i!='ETRANGER' & df$j!='ETRANGER',]
# png("img/CircularPlotFR.png", width = 500, height = 500)
myCirclePlot(df = df2, marg = 0.4, nflows = 40)
# dev.off()

## Circular plot of all residential migrations except those coming from abroad and internal migrations
df3 <- df2[df2$i!=df2$j,]
# png("img/CircularPlotFRInter.png", width = 500, height = 500)
myCirclePlot(df = df3, marg = 0.4, nflows = 40)
# dev.off()