addStrata <- function(spp,surv_yr) {

### function to create survey area strata file
###
### input: -spp: 3 capital letter species code with apostrophes [e.g. "HER"]
###        -surv_year: 4 digit year of survey [e.g. 1982]

### load required packages
    require(xlsReadWrite)

### check if strata and coast factor files exist for selected species
    if(paste("strata_",surv_yr,"_",spp,".xls",sep="") %in% list.files(data.dir))
    {tkmessageBox(message = paste("strata file for ",spp," was found successfully!",sep=""),icon="info",type="ok")}
    if(!paste("strata_",surv_yr,"_",spp,".xls",sep="") %in% list.files(data.dir))
    {tkmessageBox(message = paste("No strata file for ",spp," was found!",sep=""),icon="warning",type="ok")}

### load strata file
    files <- list.files(data.dir)
    strata_file <- list.files(data.dir,pattern = paste("strata_",surv_yr,"_",spp,".xls",sep=""))
    strata <- read.xls(file=paste(data.dir,strata_file,sep="/"),type="character",colNames=TRUE,rowNames=TRUE,sheet=1)

### get ICES rectangle list and strata per species
    rect <- matrix("",length(strata[strata!=""]),3)
    coastFact <- read.xls(paste(data.dir,"/coastFact_",spp,".xls",sep=""),type="double",colNames=TRUE,rowNames=TRUE,sheet=1)
    for (r in 1:nrow(strata)){
      for (c in 1:ncol(strata)){
        pos_rect <- length(rect[rect!=""])/ncol(rect)+1
        if(strata[r,c]!=""){rect[pos_rect,1] <- paste(rownames(strata)[r],colnames(strata)[c],sep="")}
        if(strata[r,c]!=""){rect[pos_rect,2] <- strata[r,c]}
        if(strata[r,c]!=""){rect[pos_rect,3] <- coastFact[r,c]}
      }
    }
    colnames(rect) <- c("ICES","STRATA","COASTFACT")

    as.data.frame(rect)
    
}

