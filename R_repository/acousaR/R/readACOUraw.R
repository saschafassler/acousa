readACOUraw <-
function(directory,cruise,surv_yr){

### function to load, check and format raw acoustic survey data files in PNAPES database format 
###
### input: -directory containing data files: "Acoustic" and "AcousticValues"
###        -cruise: cruise code (e.g. "HERAS")
###        -surver year: e.g. 1982

### load required packages
    require(tcltk)
    require(plyr)

### load acoustic files
    acoustic_data <- read.csv(paste(directory,"Acoustic",cruise,"_",surv_yr,".csv",sep=""))
    acousticvalues_data <- read.csv(...)
    colnames(acoustic_data)[1:16] <- c("country","vessel","cruise","log","year","month","day","hour","min","sec","declat","declon","logint","freq","Sv_thresh","interval")

### assign ICES rectangles and calculate areas
    orig_lat <- 35.5
    orig_lon <- -50
    letter <- c("A","B","C","D","E","F","G","H")
    ICES_a <- sprintf("%02.0f",floor(2*(acoustic_data$declat-orig_lat)))
    ICES_b <- letter[floor((acoustic_data$declon-orig_lon)/10)+1]
    ICES_c <- sprintf("%1.0f",floor((acoustic_data$declon-orig_lon)%%10))
    acoustic_data$ICES <- paste(ICES_a,ICES_b,ICES_c,sep="") #ICES area code
    ICES_low <- as.numeric(ICES_a)/2+35.5
    Dist_ICES_bott <- 60*(sqrt((cos(ICES_low*pi/180))^2))
    Dist_ICES_top <- 60*(sqrt((cos((ICES_low+0.5)*pi/180))^2))
    acoustic_data$ICES_area_tot <- 60*(0.5)*((Dist_ICES_bott+Dist_ICES_top)/2) #ICES surface area (nm^2)

acoustic_data

}

