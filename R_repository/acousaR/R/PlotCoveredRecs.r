PlotCoveredRecs <- function(acoustic_data) {

### function to create NASC bubble plot for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information

### load required packages
    library(data.table)  
    library(mapdata)
    #data(nseaBathy)
  
### get covered rectangle ICES code list
    rec_data <- as.data.frame(unique(acoustic_data$ICES))
    names(rec_data) <- "ICES"
    rec_latlon <- ICESrectangle2LonLat(rec_data$ICES,midpoint=TRUE)
    recs <- cbind(rec_data,rec_latlon)

### bubble plot
    minlon <- min(recs$long,na.rm=TRUE)
    maxlon <- max(recs$long,na.rm=TRUE)
    minlat <- min(recs$lat,na.rm=TRUE)
    maxlat <- max(recs$lat,na.rm=TRUE)
    extfact <- 0.3
    x1 <- round((minlon - extfact*(maxlon-minlon))/1)*1
    x2 <- round((maxlon + extfact*(maxlon-minlon))/1)*1
    y1 <- round((minlat - extfact*(maxlat-minlat))/0.5)*0.5
    y2 <- round((maxlat + extfact*(maxlat-minlat))/0.5)*0.5
    
    tiff(file=paste(output.dir,"/RecCovplot.tiff",sep=""),width=10,height=(y2-y1)*2/(x2-x1)*10,units="cm",res=300)
    par(mfrow=c(1,1), mar=c(4,4,2,0.5), oma=c(1,1,1,1),family="sans", font=2, xaxs="i",yaxs="i",bty="n")
    
    plot(recs$long,
         recs$lat,
         col="white",
         xlim=c(x1,x2),
         ylim=c(y1,y2),
         yaxs="i",
         xaxs="i",
         xlab="",
         ylab="",
         axes=FALSE)
    grid(nx=(x2-x1)/1,ny=(y2-y1)/0.5,lty=1)
    symbols(recs$long,
            recs$lat,
            squares=rep(1,nrow(recs)),
            inches=(10-4.5)/((x2-x1)/1)/2.54,
            lwd=1,
            yaxs="i",
            bg="orange",
            xaxs="i",
            xlim=c(x1,x2),
            ylim=c(y1,y2),
            xlab="",
            ylab="",add=TRUE)
    #contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,add=TRUE,col=colors()[245])
    map('worldHires',add=TRUE,col=colors()[226],fill=T)
    
    title(paste(unique(acoustic_data$CRUISE),", covered rectangles",sep=""),cex.main=0.75)
    box(bty="O",lwd=1) # bty=box type to surround plot (e.g. "O", "L", "7", "C", "U" to set box shape or "n" for none)
    axis(side = 1, at = c(seq(x1,x2,1)), labels = as.character(format(seq(x1,x2,1), nsmall = 0)),las=1, cex.axis = 0.5,lwd=1,font=1,mgp=c(3,0.5,0))
    mtext("Longitude", side=1,line=2, cex=1,las=1)
    axis(side = 2, at = seq(y1,y2,0.5), labels = as.character(format(seq(y1,y2,0.5), nsmall = 1)),las=1, cex.axis = 0.5,lwd=1,font=1,mgp=c(3,1,0))
    mtext("Latitude", side=2,line=3, cex=1,las=3)
    
    dev.off()     

}

