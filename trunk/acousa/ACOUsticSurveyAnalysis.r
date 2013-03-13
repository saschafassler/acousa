################################################################################
#
# ACOUstic Survey Analysis
#
# Date: 2012-03-21 20:23:43 CET
#
# Author: Sascha Fässler
#
# Calculates abundance and biomass from acoustic 
# survey data based on ICES rectangles 
#
# Developed with:
#   - R version 2.14.2
#   - acousaR 1.0
#
################################################################################

### ============================================================================
### load required packages
### ============================================================================
    rm(list=ls())
    require(tcltk)
    require(plyr)
    require(xlsReadWrite)
    require(acousaR)

### ============================================================================
### setup survey
### ============================================================================
    survey <- "HERAS"
    year <- 2011

### ============================================================================
### define directories
### ============================================================================
    path <- choose.dir(".","find working directory")
    setwd(path)
    data.dir          <-  file.path(".","input files")
    output.dir        <-  file.path(".","results")
    
### load AsaRaw file file
    acoustic_data <- readACOUraw(data.dir,survey,year)
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