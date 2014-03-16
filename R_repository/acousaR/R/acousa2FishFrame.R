acousa2FishFrame <- function(strata_numbers_all,acoustic_data,spp,numbers){

### function to convert acousa output into FishFrame format for the selected species
###
### input: -strata numbers: data frame with species numbers by strata and ICES rectangle information

### load required packages
    require(xlsReadWrite)
    require(XML)
    require(tcltk)
    require(matlab)

### convert output tables into AB format (XLM)
    len <- nrow(strata_numbers_all)
    AB <- data.frame("Year"=numeric(len),"Ship"=character(len),"Species"=numeric(len),"StatisticalRectangle"=character(len),"SubStatisticalRectangle"=character(len),"BiologicalSubArea"=character(len),"Number"=numeric(len),"Milage"=numeric(len))
    AB$Year <- unique(acoustic_data$YEAR)
    AB$Ship <- "TRI2" #hard coded for "F/V Tridens 2"
    if(spp=="HER") AB$Species <- 161722
    if(spp=="SPR") AB$Species <- 161789 
    AB$StatisticalRectangle <- strata_numbers_all$ICES
    AB$SubStatisticalRectangle <- "X" #hard coded for non-use of sub-rectangles
    AB$BiologicalSubArea <- strata_numbers_all$STRATA
    AB$StatisticalRectangle <- as.character(AB$StatisticalRectangle)
    AB$BiologicalSubArea <- as.character(AB$BiologicalSubArea)
    AB$Number <- round(strata_numbers_all$mills_per_ICES*1e6)
    AB <- AB[order(AB$StatisticalRectangle),]
    spp_sel <- acoustic_data[which(acoustic_data$SPECIES==spp),]
    uni_int <- spp_sel[!duplicated(spp_sel$INTERVAL),]
    AB$Milage <- aggregate(uni_int$LOGINT, by=list(uni_int$ICES),FUN=sum)[,2]
    
### save AB file for selected species
    y_n <- tkmessageBox(message = paste("Do you want to save the AB file for '",spp,"'?",sep=""),
    icon = "question", type = "yesno", default = "yes")
    answer <- tclvalue(y_n)
    if(answer=="yes")
    fileName <- tclvalue(tkgetSaveFile(initialfile = paste(output.dir,"/NED",surv_yr,"TRI_AB_",spp,".xml",sep=""),
    filetypes = "{{Excel Files} {.xml}}"))
    if(answer=="no")
    fileName<-""
    if (!nchar(fileName)) {
    tkmessageBox(message = "The AB file was not saved!")
    } else {
    tkmessageBox(message = paste("The file saved was", fileName))
    AB_xml1 <- xmlNode("Aggregated")
    for (i in 1:nrow(AB)){
    AB_xml1$children[[i]] <- xmlNode("Abundance", attrs = c(as.vector((AB)[i,1:6])), xmlNode(names(AB)[7], AB[i,7]), xmlNode(names(AB)[8],AB[i,8]))
    }
    AB_xml <- xmlNode("FishFrameAcoustics")
    AB_xml$children[[1]] <- AB_xml1
    saveXML(AB_xml,file=paste(output.dir,"/NED",surv_yr,"TRI_AB_",spp,".xml",sep=""),prefix='<?xml version="1.0" encoding="utf-8" standalone="yes"?>')
    } 
    
### convert output tables into SD format (XLM)
    len <- 0
    for (strat in 1:length(levels(acoustic_data$STRATA))){     
    stratum <- toupper(letters[strat])
    age_matrix <- numbers[[2]][[strat]]
    stratmat_len <- length(which(colSums(age_matrix)>0))
    stratrec_len <- length(which(strata_numbers_all$STRATA==stratum))
    len <- len + stratmat_len*stratrec_len
    }
    SD <- data.frame("Year"=numeric(len),"Ship"=character(len),"Species"=numeric(len),"Stock"=character(len),"Age"=numeric(len),"AgePlusGroup"=character(len),"Maturity"=character(len),"MaturityDetermination"=character(len),"StatisticalRectangle"=character(len),"SubStatisticalRectangle"=character(len),"BiologicalSubArea"=character(len),"Fraction"=numeric(len),"MeanWeight"=numeric(len),"MeanLength"=numeric(len))
    SD$Year <- unique(acoustic_data$YEAR)
    SD$Ship <- "TRI2" #hard coded for "F/V Tridens 2"
    if(spp=="HER") SD$Species <- 161722 #hard coded species code for for herring
    if(spp=="SPR") SD$Species <- 161789 #hard coded species code for sprat
    if(spp=="HER") SD$Stock <- "her-47d3" #hard coded stock code for North Sea herring
    if(spp=="SPR") SD$Stock <- "spr-nsea" #hard coded stock code for for sprat
    SD$MaturityDetermination <- "M" #hard coded for measured
    SD$SubStatisticalRectangle <- "X" #hard coded for non-use of sub-rectangles
    SD$Maturity <- as.character(SD$Maturity)    
    SD$StatisticalRectangle <- as.character(SD$StatisticalRectangle)
    SD$BiologicalSubArea <- as.character(SD$BiologicalSubArea)
    rpos <- 1
    for (strat in 1:length(levels(acoustic_data$STRATA))){     
      stratum <- toupper(letters[strat])
      age_matrix <- numbers[[2]][[strat]]
      nums_at_age <- as.data.frame(numbers[[2]][strat])
      #nums_at_age <- eval(as.name(paste("nums_at_age_",stratum,sep="")))
      weight_at_age <- as.data.frame(weights[[2]][strat])
      #weight_at_age <- eval(as.name(paste("weight_at_age_",stratum,sep="")))
      stratmat_len <- length(which(colSums(age_matrix)>0))
      stratrec_len <- length(which(strata_numbers_all$STRATA==stratum))
    SD$Age[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <-  rep(surv_yr - as.numeric(substr(names(which(colSums(age_matrix)>0)),1,4))-1,stratrec_len)
      mat <- character(stratmat_len)
      mat[(substr(names(which(colSums(age_matrix,na.rm=TRUE)>0)),5,6))=="im"] <- "I"
      mat[(substr(names(which(colSums(age_matrix,na.rm=TRUE)>0)),5,6))!="im"] <- "M"
    SD$Maturity[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- rep(mat,stratrec_len)
    SD$StatisticalRectangle[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- rep(strata_numbers_all$ICES[strata_numbers_all$STRATA==stratum],stratmat_len)[order(rep(strata_numbers_all$ICES[strata_numbers_all$STRATA==stratum],stratmat_len))]
    SD$BiologicalSubArea[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- as.character(rep(strata_numbers_all$STRATA[strata_numbers_all$STRATA==stratum],stratmat_len))
      fract <- colSums(nums_at_age)[colSums(nums_at_age)>0]/sum(strata_numbers_all$mills_per_ICES[which(strata_numbers_all$STRATA==stratum)])
    SD$Fraction[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- rep(as.data.frame(fract)[[1]],stratrec_len)
      wATa <- colSums(weight_at_age,na.rm=TRUE)[which(colSums(weight_at_age,na.rm=TRUE)>0)]
    SD$MeanWeight[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- rep(as.data.frame(wATa)[[1]],stratrec_len)
      lATa <- colSums(as.numeric(rownames(nums_at_age))*nums_at_age)/colSums(nums_at_age)
    SD$MeanLength[rpos:(rpos+(stratmat_len*stratrec_len)-1)] <- rep(as.data.frame(lATa[which(lATa>0)])[[1]],stratrec_len)*10
    rpos <- rpos + stratmat_len*stratrec_len
    }        
    SD$AgePlusGroup <- "-"
    SD$AgePlusGroup[which(SD$Age>=9)] <- "+"
    
### save SD file for selected species
    y_n <- tkmessageBox(message = paste("Do you want to save the SD file for '",spp,"'?",sep=""),
    icon = "question", type = "yesno", default = "yes")
    answer <- tclvalue(y_n)
    if(answer=="yes")
    fileName <- tclvalue(tkgetSaveFile(initialfile = paste(output.dir,"/NED",surv_yr,"TRI_SD_",spp,".xml",sep=""),
    filetypes = "{{Excel Files} {.xml}}"))
    if(answer=="no")
    fileName<-""
    if (!nchar(fileName)) {
    tkmessageBox(message = "The SD file was not saved!")
    } else {
    tkmessageBox(message = paste("The file saved was", fileName))
    SD_xml1 <- xmlNode("Aggregated")
    for (i in 1:nrow(SD)){
    SD_xml1$children[[i]] <- xmlNode("StockDetail", attrs = c(as.vector((SD)[i,1:11])), xmlNode(names(SD)[12], SD[i,12]), xmlNode(names(SD)[13],SD[i,13]), xmlNode(names(SD)[14],SD[i,14]))
    }
    SD_xml <- xmlNode("FishFrameAcoustics")
    SD_xml$children[[1]] <- SD_xml1
    saveXML(SD_xml,file=paste(output.dir,"/NED",surv_yr,"TRI_SD_",spp,".xml",sep=""),prefix='<?xml version="1.0" encoding="utf-8" standalone="yes"?>') 
    }

}
