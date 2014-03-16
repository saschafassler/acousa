readACOUraw <- function(directory,cruise,surv_yr){

### function to load, check and format raw acoustic survey data files in PGNAPES database format 
###
### input: -directory containing data files: "Acoustic" and "AcousticValues"
###        -cruise: cruise code (e.g. "HERAS")
###        -surver year: e.g. 1982

### load required packages
    require(tcltk)
    require(plyr)
    require(data.table)

### load and merge acoustic files 
    acoustic_data <- read.csv(paste(data.dir,(list.files(path = data.dir, pattern = "Acoustic.csv", ignore.case = FALSE)),sep="/"))
    acousticvalues_data <- read.csv(paste(data.dir,(list.files(path = data.dir, pattern = "AcousticValues.csv", ignore.case = FALSE)),sep="/"))
    ACOUraw <- merge(acoustic_data,acousticvalues_data,by=(names(acoustic_data)[1:7]),sort=TRUE)
    #ACOUraw <- ACOUraw[order(ACOUraw$Interval),]
    #ACOUraw <- data.table(ACOUraw)
    ACOUraw <- setNames(ACOUraw,toupper(names(ACOUraw)))
    ACOUraw <- subset(ACOUraw, select = -c(FREQUENCY,SV_THRESHOLD,CHUPPDEPTH,CHLOWDEPTH) )

### assign ICES rectangles and calculate areas
    ACOUraw$ICES <- ICESrectangle(ACOUraw,"ACLON","ACLAT")
    DistICESbott <- 60*(sqrt((cos((as.numeric(substr(ACOUraw$ICES,1,2))/2+35.5)*pi/180))^2))
    DistICEStop <- 60*(sqrt((cos((as.numeric(substr(ACOUraw$ICES,1,2))/2+36)*pi/180))^2))
    ACOUraw$ICESTOTALAREA <- 60*(0.5)*((DistICESbott+DistICEStop)/2) #ICES surface area (nm^2)
    #ACOUraw$ICESTOTALAREA <- 60*(0.5)*((DistICESbott+DistICEStop)/2) #ICES surface area (nm^2) -> WRONG CODE!!!
    
### show identified regions and/or species
    tkmessageBox(title="Classes",message=paste("The following species/echo trace classes were found in the acoustic data file: ",paste(levels(ACOUraw$SPECIES),collapse=" / ")))

ACOUraw

}