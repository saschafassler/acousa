################################################################################
#
# ACOUstic Survey Analysis
#
# Date: 2013-03-19 23:13:39 CET
#
# Author: Sascha F�ssler
#
# Calculates abundance and biomass from acoustic 
# survey data based on ICES rectangles 
#
# Developed with:
#   - R version 2.14.2
#   - acousaR 1.02
#
################################################################################

### ============================================================================
### load required packages
### ============================================================================
    rm(list=ls())
    require(tcltk)
    require(plyr)
    require(xlsReadWrite)
    xls.getshlib()
    require(acousaR)
    require("RODBC")
    
### ============================================================================
### setup survey
### ============================================================================
    survey <- "HERAS"
    year <- 2013

### ============================================================================
### define directories
### ============================================================================
    path <- "D:/Repository/ACOUSA/acousa/trunk/acousa/survey analysis/"
    try(setwd(path),silent=TRUE)
    data.dir          <-  file.path(".",paste(as.character(survey)," ",as.character(year),sep=""),"input files")
    output.dir        <-  file.path(".",paste(as.character(survey)," ",as.character(year),sep=""),"results")
    
### load acoustic data file
    acoustic_data <- readACOUraw(data.dir,survey,year)
    surv_yr <- unique(acoustic_data$YEAR)
    surv_cd <- as.character(unique(acoustic_data$CRUISE))
    
# ### load biological data from IMARES database
#     #Define needed variables 
#     user<-"..."
#     pass<-"..."
#     billie.ex(user,pass,surv_cd,surv_yr)

################################################################################################################################
################################################################################################################################
### calculate estimates for every species

### select species
    spp <- SPPselect(acoustic_data)

### input coast factors and strata
    rectangles <- addStrata(spp,surv_yr)
    acoustic_data <- merge(acoustic_data,rectangles,by="ICES")
    acoustic_data$ICESsurfarea <- as.numeric(as.character(acoustic_data$COASTFACT)) * acoustic_data$ICESTOTALAREA

### map plots
    PlotTrack(acoustic_data)
    PlotCoveredRecs(acoustic_data)
    PlotNascMapBubbles(acoustic_data,spp)
        
### calculate NASC by ICES rectangle
    SA_table <- nascBYrect(acoustic_data,spp)
    
### calculate NUMBERS per stratum
    numbers <- nBYstrata(SA_table,spp)
    
### calculate BIOMASS per stratum
    weights <- wBYstrata(SA_table,spp,numbers)    
    
### produce FishFrame XML files
    acousa2FishFrame(as.data.frame(numbers[1][1]),acoustic_data,spp,numbers)
    