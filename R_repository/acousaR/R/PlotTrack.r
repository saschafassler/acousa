PlotTrack <- function(acoustic_data) {

### function to create plot of survey track
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information

### load required packages
    library(data.table)  
    library(mapdata)
    #data(nseaBathy)
  
### get interval data
    int_data <- acoustic_data[order(acoustic_data$LOG),]

### bubble plot
    minlon <- min(int_data$ACLON,na.rm=TRUE)
    maxlon <- max(int_data$ACLON,na.rm=TRUE)
    minlat <- min(int_data$ACLAT,na.rm=TRUE)
    maxlat <- max(int_data$ACLAT,na.rm=TRUE)
    extfact <- 0.3
    x1 <- round((minlon - extfact*(maxlon-minlon))/1)*1
    x2 <- round((maxlon + extfact*(maxlon-minlon))/1)*1
    y1 <- round((minlat - extfact*(maxlat-minlat))/0.5)*0.5
    y2 <- round((maxlat + extfact*(maxlat-minlat))/0.5)*0.5
    
    tiff(file=paste(output.dir,"/SurvTrackplot.tiff",sep=""),width=10,height=(y2-y1)*2/(x2-x1)*10,units="cm",res=300)
    par(mfrow=c(1,1), mar=c(4,4,2,0.5), oma=c(1,1,1,1),family="sans", font=2, xaxs="i",yaxs="i",bty="n")
    
    plot(int_data$ACLON,
         int_data$ACLAT,
         col="white",
         xlim=c(x1,x2),
         ylim=c(y1,y2),
         yaxs="i",
         xaxs="i",
         xlab="",
         ylab="",
         axes=FALSE)
    grid(nx=(x2-x1)/1,ny=(y2-y1)/0.5,lty=1)
    points(int_data$ACLON,
           int_data$ACLAT,
            lwd=2,
            yaxs="i",
            xaxs="i",
            xlim=c(x1,x2),
            ylim=c(y1,y2),
            xlab="",
            ylab="",type="l")
    #contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,add=TRUE,col=colors()[245])
    map('worldHires',add=TRUE,col=colors()[226],fill=T)
    
    title(paste(unique(acoustic_data$CRUISE),", survey track",sep=""),cex.main=0.75)
    box(bty="O",lwd=1) # bty=box type to surround plot (e.g. "O", "L", "7", "C", "U" to set box shape or "n" for none)
    axis(side = 1, at = c(seq(x1,x2,1)), labels = as.character(format(seq(x1,x2,1), nsmall = 0)),las=1, cex.axis = 0.5,lwd=1,font=1,mgp=c(3,0.5,0))
    mtext("Longitude", side=1,line=2, cex=1,las=1)
    axis(side = 2, at = seq(y1,y2,0.5), labels = as.character(format(seq(y1,y2,0.5), nsmall = 1)),las=1, cex.axis = 0.5,lwd=1,font=1,mgp=c(3,1,0))
    mtext("Latitude", side=2,line=3, cex=1,las=3)
    
    dev.off()     

}

