### analyse acoustic survey data

### load required packages
    rm(list=ls())
    require(tcltk)
    require(plyr)
    require(xlsReadWrite)

### define directories
    AsaR_dir <- choose.dir('D:/Sascha/Projects/AsaR/',"find AsaR working directory")
    scripts_folder <- paste(AsaR_dir,"\\scripts\\",sep="")
    setwd(scripts_folder)

    analysis_folder <- choose.dir('D:/Sascha/Projects/AsaR/analysis',"find survey 'analysis' folder") #this eventually needs to be the survey "input data" folder
    survey_folder <- list.files(analysis_folder)
    input_folder <- paste(analysis_folder,"\\",survey_folder,"\\input files\\",sep="")
    output_folder <- paste(analysis_folder,"\\",survey_folder,"\\results\\",sep="")
    
### load AsaRaw file file
    source("readAsaRaw.r")
    acoustic_data <- readAsaRaw(input_folder,strsplit(survey_folder," ")[[1]][1],as.numeric(strsplit(survey_folder," ")[[1]][2]))
    surv_yr <- unique(acoustic_data$year)
    surv_cd <- as.character(unique(acoustic_data$cruise))    

    ### show identified regions and species
    nr_spp <- ncol(acoustic_data) - 18 # 18 = nr of primary fields in file plus ICES rectangle codes and total surface areas
    tkmessageBox(title="Classes",message=paste("The following species/echo trace classes were found in the AsaRaw file: ",paste(colnames(acoustic_data)[c(17:(nr_spp+16))],collapse=" / ")),icon="info",type="ok")

################################################################################################################################
################################################################################################################################
### calculate estimates for every species

### select species
    source("SPPselect.r")
    spp <- SPPselect(acoustic_data)

### input coast factors and strata
    source("addStrataR.r")
    rectangles <- addStrataR(spp,surv_yr)
    colnames(rectangles) <- c("ICES","strata","coastfact")
    acoustic_data <- merge(acoustic_data,rectangles,by="ICES")
    acoustic_data$ICES_surf <- as.numeric(as.character(acoustic_data$coastfact)) * acoustic_data$ICES_area_tot
    
### calculate NASC by ICES rectangle
    source("nascBYrect.r")
    SA_table <- nascBYrect(acoustic_data,spp)
    
### calculate NUMBERS
    source("nBYstrataR.r")
    nBYstrataR(SA_table,spp)
    
### calculate BIOMASS
    source("wBYstrataR.r")
    wBYstrataR(SA_table,spp)    
    
### produce FishFrame XML files
    source(paste(AsaR_dir,"\\scripts\\AsaR2FishFrame.r",sep=""))
    AsaR2FishFrame(strata_numbers_all,acoustic_data,spp)