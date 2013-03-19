nascBYrect <- function(acoustic_data,spp) {

### function to create NASC by ICES rectangle for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information
###        -spp: 3 capital letter species code with apostrophes [e.g. "HER"]

### calculate nr of EDSUs and average SA per ICES rectangle
    mileage_list <- as.data.frame(matrix(0,length(levels(as.factor(acoustic_data$ICES))),3))
    colnames(mileage_list) <- c("ICES","EDSUperICES","MeanNASC")
    mileage_list$ICES <- levels(as.factor(acoustic_data$ICES))
    for (recta in 1:nrow(mileage_list)){
    mileage_list$EDSUperICES[recta] <- nrow(acoustic_data[acoustic_data$Species==spp & acoustic_data$ICES==mileage_list$ICES[recta],]) #nr of EDSU's per species
    mileage_list$MeanNASC[recta] <- sum(acoustic_data[which(acoustic_data$Species==spp & acoustic_data$ICES==mileage_list$ICES[recta]),]$SA)/mileage_list$EDSUperICES[recta]
    }
    acoustic_data <- merge(acoustic_data,mileage_list,by=c("ICES"))

### double check final NASC values with export files
### ... UNDER DEVELOPMENT ...

### save SA/EDSU per ICES list for the species
    y_n <- tkmessageBox(message = paste("Do you want to save the SA-table for '",spp,"'?",sep=""),
        icon = "question", type = "yesno", default = "yes")
    answer <- tclvalue(y_n)
    if(answer=="yes")
    fileName <- tclvalue(tkgetSaveFile(initialfile = paste(output.dir,"/SA_table_",spp,".xls",sep=""),
        filetypes = "{{Excel Files} {.xls}}"))
    if(answer=="no")
    fileName<-""
    if (!nchar(fileName)) {
        tkmessageBox(message = "The SA-table was not saved!")
    } else {
        tkmessageBox(message = paste("The file saved was", fileName))
        write.xls(mileage_list,file=paste(output.dir,"/SA_table_",spp,".xls",sep=""),colNames=TRUE,rowNames=NA,sheet=1)
    }
    
    mileage_list
    
}

