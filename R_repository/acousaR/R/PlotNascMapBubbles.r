PlotNascMapBubbles <- function(acoustic_data,spp,SAres = 5) {

### function to create NASC bubble plot for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information
###        -spp: 3 capital letter species code with apostrophes [e.g. "HER"]

### load required packages
    library(data.table)  
  
### get interval data summed over depth channels
    spp_data <- data.table(acoustic_data[which(acoustic_data$SPECIES==spp),])
    spp_data_int <- spp_data[,list(SA=sum(SA), LAT=mean(ACLAT), LON=mean(ACLON)), by='LOG']
    spp_data_int <- spp_data_int[order(spp_data_int$LOG),]

### sub-sample by SAres interval
    int <- SAres
    int_steps <- seq(ceiling(int/2),length(spp_data_int$LOG),int)
    int_data <- matrix(NA,length(int_steps),3)
    for(step in 1:length(int_steps)){
      int_data[step,1] <- spp_data_int$LON[int_steps[step]]
      int_data[step,2] <- spp_data_int$LAT[int_steps[step]]
      int_data[step,3] <- mean(spp_data_int$SA[(step*int-(int-1)):(step*int)])
    }
    
### bubble plot
    minlon <- min(int_data[,1],na.rm=TRUE)
    maxlon <- max(int_data[,1],na.rm=TRUE)
    minlat <- min(int_data[,2],na.rm=TRUE)
    maxlat <- max(int_data[,2],na.rm=TRUE)
    extfact <- 0.3
    x1 <- round(minlon - extfact*(maxlon-minlon),1)
    x2 <- round(maxlon + extfact*(maxlon-minlon),1)
    y1 <- round(minlat - extfact*(maxlat-minlat),1)
    y2 <- round(maxlat + extfact*(maxlat-minlat),1)
    
    tiff(file=paste(output.dir,"/",spp,"_SAplot.tiff",sep=""),width=10,height=(y2-y1)*2/(x2-x1)*10,units="cm",res=300)
    par(mfrow=c(1,1), mar=c(2,2,0.5,0.5), oma=c(1,1,1,1),family="serif", font=2)
    
    maxSA <- max(int_data[,3],na.rm=TRUE)
    symbols(int_data[,1],
            int_data[,2],
            circles=sqrt(int_data[,3]/maxSA),
            inches=0.3,
            lwd=2,
            yaxs="i",
            xaxs="i",
            xlim=c(x1,x2),
            ylim=c(y1,y2),
            xlab="",
            ylab="")
              
    dev.off()     




    
plot(1,1,type='n', xlim=xlim0,ylim=ylim0,xlab='',ylab='')
title(paste(what.year," Q",what.quarter," ",input$scientific.name[1],sep=''))

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

ndatq <- input[!is.na(input[,what.cpue]) & input$quarter == what.quarter & input$year == what.year,]     # select year and quarter
ww <- (1:length(ndatq[,1]))[ndatq[,what.cpue]==0]

if(length(ww>0))
{
ndatq0 <- ndatq[ww,]  # the zeros
ndatq1 <- ndatq[-ww,] # the +ve component
}
else{
ndatq1 <- ndatq }

m1 <- min(ndatq1[,what.cpue],na.rm=T)
m2 <- max(ndatq1[,what.cpue],na.rm=T)
if (m2 > m1) {
qq<-seq(m1,m2,length=9)

ll <- length(qq)

legVals <- cut(ndatq1[,what.cpue], breaks=qq,labels=as.character(1:(ll-1)));

blobSize <- cut(ndatq1[,what.cpue], breaks=qq, labels=as.character(1:(ll-1)))
blobSize <- as.numeric(as.character(blobSize));

if(length(ww>0)){
points(ndatq1$shootlong,ndatq1$shootlat,cex=(blobSize*scaling.factor),pch=21,col='black',bg='yellow')
points(ndatq0$shootlong,ndatq0$shootlat,pch=16,col='black',cex=.5)
}
#Just plot the positive data
else{
points(ndatq1$shootlong,ndatq1$shootlat,cex=(blobSize*scaling.factor),pch=21,col='black',bg='yellow')
}


#legend("topright",
#legend=round(c(0,seq(min(ndatq1[,what.cpue]),max(ndatq1[,what.cpue]),length=6)),2),
#pch=c(16,rep(21,7)),
#pt.cex=seq(min(blobSize,na.rm=T),max(blobSize,na.rm=T),length=7)*scaling.factor,
#pt.bg=c('black',rep('yellow',6)),

#bg='white',

#x.intersp=1.5,xjust=0.5,col=c('black',rep('black',6)),horiz=F,cex=.6,title=what.cpue)

}

else{print("Insufficient data")}

}

