nBYstrata <- function(SA_table,spp) {

### function to calculate abundance by ICES rectangle and stratum for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information
###        -spp: 3 capital letter species code with apostrophes [e.g. "HER"]

### load required packages
    library(pgirmess)

### load catch length-frequency data
    catch_data <- read.csv(paste(data.dir,"/catchLF_",survey,"_",surv_yr,"_",spp,".csv",sep=""),header=TRUE)
    catch_data[is.na(catch_data)] <- 0

### associate hauls with strata
    haul_pos <- read.csv(paste(data.dir,"/trawl_",survey,"_",surv_yr,".csv",sep=""),header=TRUE)
    if(max(abs(haul_pos$lat)-floor(abs(haul_pos$lat)))<0.6){
    haul_pos$lat <- trunc(haul_pos$lat)+(haul_pos$lat-trunc(haul_pos$lat))/0.6} # convert positions from degrees into decimal if necessary
    if(max(abs(haul_pos$lon)-floor(abs(haul_pos$lon)))<0.6){
    haul_pos$lon <- trunc(haul_pos$lon)+(haul_pos$lon-trunc(haul_pos$lon))/0.6} # convert positions from degrees into decimal if necessary
    orig_lat <- 35.5
    orig_lon <- -50
    letter <- c("A","B","C","D","E","F","G","H")
    ICES_a <- sprintf("%02.0f",floor(2*(haul_pos$lat-orig_lat)))
    ICES_b <- letter[floor((haul_pos$lon-orig_lon)/10)+1]
    ICES_c <- sprintf("%1.0f",floor((haul_pos$lon-orig_lon)%%10))
    haul_pos$ICES <- paste(ICES_a,ICES_b,ICES_c,sep="") #ICES area code
    haul_pos <- merge(haul_pos,rectangles)
    
### select haul information per strata
    for (strat in 1:length(levels(haul_pos$STRATA))){
    stratum <- letter[strat]
    hauls <- haul_pos$haul[which(haul_pos$STRATA == stratum)]
    strat_name <- paste("hauls_",stratum,sep="")
    assign(strat_name,hauls)}

### get mean L-F per strata
    
    #** TEMPORARY SOLUTION: DEFINING THRESHOLD LEVEL OF FISH NR **#
    thresh <- 20 #number of fish in haul required to be representative
    for (strat in 1:length(levels(haul_pos$STRATA))){
    stratum <- letter[strat]
    strata_hauls <- eval(as.name(paste("hauls_",stratum,sep="")))
    
    # window displaying strata hauls
    ### ... UNDER DEVELOPMENT ...
    strata_hauls_LF_all <- as.data.frame(catch_data[,strata_hauls+1])
    names(strata_hauls_LF_all) <- as.character(c(strata_hauls))
    strata_hauls_sums_all <- as.data.frame(colSums(strata_hauls_LF_all))[,1]
    # only select hauls containing >threshold level fish
    strata_hauls_LF <- strata_hauls_LF_all[which(strata_hauls_sums_all>=thresh)]
    strata_hauls_sums <- as.data.frame(colSums(strata_hauls_LF))[,1]
    
    selected_strata_hauls_name <- paste("selected_hauls_",stratum,sep="")
    assign(selected_strata_hauls_name,substr(names(strata_hauls_LF),1,2))
    
    haul_matrix <- matrix(0,nrow(strata_hauls_LF),length(strata_hauls_LF))
    for (strata_haul in 1:length(strata_hauls_LF)){
    haul_matrix[,strata_haul] <- as.matrix(strata_hauls_LF[strata_haul]*100/rep(strata_hauls_sums[strata_haul],each=nrow(strata_hauls_LF[strata_haul])))}
    #*** need a way to check data in order to remove hauls with low observations (or do this in "setStrata.r") ***#
    ### ... UNDER DEVELOPMENT ...    
    
    haul_matrix <- as.matrix(haul_matrix[,!apply(haul_matrix,2,function(haul_matrix) all(is.na(haul_matrix)))])
    mean_strata_LF <- rowSums(haul_matrix)/ncol(haul_matrix)
    strat_name <- paste("mean_hauls_LF_",stratum,sep="")
    assign(strat_name,mean_strata_LF)
    }

### calculate fish numbers at length per stratum

### enter survey code
    dlg <- tktoplevel()
    tkwm.title(dlg,"Survey code") 
    textEntryVarTcl1 <- tclVar(paste(""))
    textEntryWidget1 <- tkentry(dlg,width=paste(20),textvariable=textEntryVarTcl1)
    tkgrid(tklabel(dlg,text="       "))
    tkgrid(tklabel(dlg,text=paste("Enter b20 value for '",spp,"' : ",sep="")),textEntryWidget1)
    tkgrid(tklabel(dlg,text="       "))
    ReturnVal <- "ID_CANCEL"
    onOK <- function() {
      ReturnVal1 <<- tclvalue(textEntryVarTcl1)
      tkgrab.release(dlg)
      tkdestroy(dlg)}
    OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
    tkgrid(OK.but)
    tkgrid(tklabel(dlg,text="    "))
    tkfocus(dlg)
    tkwait.window(dlg)
    tkmessageBox(title="Survey code",message=paste("b20 value: ",ReturnVal1,sep=""))
    b_20 <- as.numeric(ReturnVal1)

    length_increment <- (catch_data[2,1]-catch_data[1,1])/2
    sbs_length <- 10^((20*log10(catch_data[,1]+length_increment)+b_20)/10)*4*pi  # according to Bram Couperus
    
    strata_numbers_all <- data.frame("ICES"=character(0),"EDSU_PER_ICES"=numeric(0),"MEAN_NASC"=numeric(0),"STRATA"=character(0),"COASTFACT"=numeric(0),"ICES_surf"=numeric(0),"spp_nr_pernm2"=numeric(0),"mills_per_ICES"=numeric(0)) 
    for (strat in 1:length(levels(haul_pos$STRATA))){                                    
    stratum <- letter[strat]
    stratum_mean_LF <- eval(as.name(paste("mean_hauls_LF_",stratum,sep="")))
    nr_sbs <- sbs_length*stratum_mean_LF
    mean_strat_sbs <- sum(nr_sbs)/100  # according to Bram Couperus
    strata_SA_a <- merge(SA_table,rectangles)
    strata_SA_a$ICES_surf <- unique(acoustic_data[,c("ICES","ICESsurfarea")])[,2]
    strata_SA <- strata_SA_a[which(strata_SA_a$STRATA==stratum),] 

    strata_SA$spp_nr_pernm2 <- strata_SA$MEAN_NASC/(mean_strat_sbs*1000) # density 1000 fish * nm-2
    strata_SA$mills_per_ICES <- strata_SA$spp_nr_pernm2 * strata_SA$ICES_surf/1000
    strat_name <- paste("strata_numbers_",stratum,sep="")
    assign(strat_name,strata_SA)
    strata_numbers_all <- merge(strata_numbers_all,strata_SA,all=TRUE)
    
    nums_at_length_name <- paste("nums_at_length_",stratum,sep="")
    nums_at_length <- matrix(0,length(stratum_mean_LF),1)
    nums_at_length[,1] <- sum(strata_SA$mills_per_ICES)*stratum_mean_LF/100
    rownames(nums_at_length) <- catch_data[,1]
    colnames(nums_at_length) <- "numbers (millions)"
    assign(nums_at_length_name,nums_at_length)
    } 
  
### calculate fish numbers at age per stratum
  
### load fish samples data
    if(paste("samples_",survey,"_",surv_yr,"_",spp,".csv",sep="") %in% list.files(data.dir))
      {tkmessageBox(message = paste("sample data file for ",spp," was found successfully!",sep=""),icon="info",type="ok")} 
    if(!paste("samples_",survey,"_",surv_yr,"_",spp,".csv",sep="") %in% list.files(data.dir))
      {tkmessageBox(message = paste("No sample data file for ",spp," was found!",sep=""),icon="warning",type="ok")}
#      if(!paste("samples_",surv_cd,"_",surv_yr,"_",spp,".csv",sep="") %in% list.files(input_folder))
#        {source(paste(AsaR_dir,"\\scripts\\numsPERstrata.R",sep=""))}
    #*** need alternative action if no file available ***#
    ### ... UNDER DEVELOPMENT ...    
    sample_data <- read.csv(paste(data.dir,"/samples_",survey,"_",surv_yr,"_",spp,".csv",sep=""),header=TRUE)
         
### generate age-length-maturity table per strata      
    ages <- surv_yr - c(rep(1:6,each=2),seq(7,9),10)
    subs <- c("im","ad","im","ad","im","ad","im","ad","im","ad","im","ad","","","","+")
    char_ages <- paste(ages,subs,sep="") 

### generate age-length matrix per strata        
    for (strat in 1:length(levels(haul_pos$STRATA))){ 
    stratum <- letter[strat]
    strata_samples <- sample_data[which(sample_data$haul%in%as.integer(eval(as.name(paste("selected_hauls_",stratum,sep=""))))),]
    strata_samples$length <- floor((strata_samples$length*10*2+0.5)/10)/2 #use floor of sample lengths
    
    age_distr <- matrix(0,nrow(strata_samples),length(ages))
    rownames(age_distr) <- strata_samples$length
    colnames(age_distr) <- char_ages
    for(im_year in seq(1,11,2)){
      yr <- ages[im_year]
      age_distr[which(strata_samples$mat<3&strata_samples$age==yr),im_year] <- strata_samples$length[which(strata_samples$mat<3&strata_samples$age==yr)]
      }
    for(ad_year in seq(2,12,2)){
      yr <- ages[ad_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age==yr),ad_year] <- strata_samples$length[which(strata_samples$mat>2&strata_samples$age==yr)]
      }
    for(plus_year in seq(13,15,1)){
      yr <- ages[plus_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age==yr),plus_year] <- strata_samples$length[which(strata_samples$mat>2&strata_samples$age==yr)]
      }
    for(plus_year in seq(16,16,1)){
      yr <- ages[plus_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age<yr+1),plus_year] <- strata_samples$length[which(strata_samples$mat>2&strata_samples$age<(yr+1))]
      }
    strata_length_samples_name <- paste("strata_length_samples_",stratum,sep="")
    assign(strata_length_samples_name,age_distr)
    }  
 
    for (strat in 1:length(levels(haul_pos$STRATA))){ 
    stratum <- letter[strat]
    strata_age_samples <- eval(as.name(paste("strata_length_samples_",stratum,sep="")))
    strata_numbers <- eval(as.name(paste("strata_numbers_",stratum,sep="")))
    mean_hauls_LF <- eval(as.name(paste("mean_hauls_LF_",stratum,sep="")))
    age_matrix <- matrix(0,nrow(strata_hauls_LF),length(char_ages))
    lngs <- catch_data[,1]
    ava_lngs <- lngs[which(lngs%in%strata_age_samples)]
    for(len in 1:length(ava_lngs)){ # produce samples collected @ age
      L <- as.numeric(ava_lngs[len])
      age_matrix[which(catch_data[1]==L),] <- colSums(rbind(matrix(0,1,16),strata_age_samples[which(as.numeric(rownames(strata_age_samples))==L),]>0))
      rownames(age_matrix) <- lngs
      colnames(age_matrix) <- char_ages
      age_matrix_name <- paste("age_matrix_",stratum,sep="")
      assign(age_matrix_name,age_matrix)
      }
    nums_at_age <- matrix(0,dim(age_matrix)[1],dim(age_matrix)[2])
    for(age in 1:length(char_ages)){
      nums_at_age[,age] <- age_matrix[,age]/(rowSums(age_matrix)+0.0000000000001)*(sum(strata_numbers$mills_per_ICES)*mean_hauls_LF/100)
      rownames(nums_at_age) <- lngs
      colnames(nums_at_age) <- char_ages
      nums_at_age_name <- paste("nums_at_age_",stratum,sep="")
      assign(nums_at_age_name,nums_at_age)
    }
    }
    
}
