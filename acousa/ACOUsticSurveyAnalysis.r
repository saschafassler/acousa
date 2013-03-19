################################################################################
#
# ACOUstic Survey Analysis
#
# Date: 2013-03-19 23:13:39 CET
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
    year <- 2010

### ============================================================================
### define directories
### ============================================================================
    path <- choose.dir(".","find survey working directory")
    setwd(path)
    data.dir          <-  file.path(".",paste(as.character(survey)," ",as.character(year),sep=""),"input files")
    output.dir        <-  file.path(".",paste(as.character(survey)," ",as.character(year),sep=""),"results")
    
### load acoustic data file
    acoustic_data <- readACOUraw(data.dir,survey,year)
    surv_yr <- unique(acoustic_data$Year)
    surv_cd <- as.character(unique(acoustic_data$Cruise))    

################################################################################################################################
################################################################################################################################
### calculate estimates for every species

### select species
    spp <- SPPselect(acoustic_data)

### input coast factors and strata
    rectangles <- addStrata(spp,surv_yr)
    acoustic_data <- merge(acoustic_data,rectangles,by="ICES")
    acoustic_data$ICESsurfarea <- as.numeric(as.character(acoustic_data$coastfact)) * acoustic_data$ICEStotalarea
    
### calculate NASC by ICES rectangle
    SA_table <- nascBYrect(acoustic_data,spp)
    
### calculate NUMBERS
    source("nBYstrata.r")
    nBYstrata(SA_table,spp)
    
### calculate BIOMASS
    source("wBYstrata.r")
    wBYstrata(SA_table,spp)    
    
### produce FishFrame XML files
    source(paste(AsaR_dir,"\\scripts\\AsaR2FishFrame.r",sep=""))
    AsaR2FishFrame(strata_numbers_all,acoustic_data,spp)