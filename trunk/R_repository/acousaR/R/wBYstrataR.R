wBYstrata <- function(SA_table,spp,numbers) {

### function to calculate biomass by ICES rectangle and stratum for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information
###        -spp: 3 capital letter species code with apostrophes [e.g. "HER"]
###        -numbers: list object with numbers at length/age by stratum 

### load required packages

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

### load fish samples data
    sample_data <- read.csv(paste(data.dir,"/samples_",survey,"_",surv_yr,"_",spp,".csv",sep=""),header=TRUE)
        
### generate age-weight-maturity table per strata
    ages <- surv_yr - c(rep(1:6,each=2),seq(7,9),10)
    subs <- c("im","ad","im","ad","im","ad","im","ad","im","ad","im","ad","","","","+")
    char_ages <- paste(ages,subs,sep="")

### generate age-weight matrix per strata
    for (strat in 1:length(levels(haul_pos$STRATA))){
    stratum <- letter[strat]
    strata_samples <- sample_data[which(sample_data$haul%in%as.integer(eval(as.name(paste("selected_hauls_",stratum,sep=""))))),]

    age_distr <- matrix(0,nrow(strata_samples),length(ages))
    rownames(age_distr) <- strata_samples$length
    colnames(age_distr) <- char_ages
    for(im_year in seq(1,11,2)){
      yr <- ages[im_year]
      age_distr[which(strata_samples$mat<3&strata_samples$age==yr),im_year] <- strata_samples$weight[which(strata_samples$mat<3&strata_samples$age==yr)]
      }
    for(ad_year in seq(2,12,2)){
      yr <- ages[ad_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age==yr),ad_year] <- strata_samples$weight[which(strata_samples$mat>2&strata_samples$age==yr)]
      }
    for(plus_year in seq(13,15,1)){
      yr <- ages[plus_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age==yr),plus_year] <- strata_samples$weight[which(strata_samples$mat>2&strata_samples$age==yr)]
      }
    for(plus_year in seq(16,16,1)){
      yr <- ages[plus_year]
      age_distr[which(strata_samples$mat>2&strata_samples$age<yr+1),plus_year] <- strata_samples$weight[which(strata_samples$mat>2&strata_samples$age<yr+1)]
      }
    strata_weight_samples_name <- paste("strata_weight_samples_",stratum,sep="")
    assign(strata_weight_samples_name,age_distr)
    }

### calculate biomass per stratum and rectangle
    weights <- list()
    strata_numbers_all <- data.frame("ICES"=character(0),"EDSU_PER_ICES"=numeric(0),"MEAN_NASC"=numeric(0),"STRATA"=character(0),"COASTFACT"=numeric(0),"ICES_surf"=numeric(0),"spp_nr_pernm2"=numeric(0),"mills_per_ICES"=numeric(0),"ttons_per_ICES"=numeric(0)) 
    for (strat in 1:length(levels(haul_pos$STRATA))){
      stratum <- letter[strat]
      strata_weight_samples <- eval(as.name(paste("strata_weight_samples_",stratum,sep="")))
      strata_weight_samples[which(strata_weight_samples==0)] <- NA
      strata_nums <- as.data.frame(numbers[1])
      strata_nums <- strata_nums[which(strata_nums$STRATA == stratum),]
      nums_at_age <- as.data.frame(numbers[[2]][(strat)])
      names(nums_at_age) <- char_ages
      #strata_nums <- eval(as.name(paste("strata_numbers_",stratum,sep="")))
      #nums_at_age <- eval(as.name(paste("nums_at_age_",stratum,sep="")))
      nums_at_age[nums_at_age==0] <- NA 

      weight_matrix <- matrix(NA,nrow(strata_hauls_LF),length(char_ages))
      lngs <- catch_data[,1]
      ava_lngs <- lngs[which(lngs%in%rownames(strata_weight_samples))]
      for(len in 1:length(ava_lngs)){ # produce mean weight @ length of samples collected
        L <- as.numeric(ava_lngs[len])
        weight_matrix[which(catch_data[1]==L),] <- colMeans(rbind(matrix(NA,1,16),strata_weight_samples[which(as.numeric(rownames(strata_weight_samples))==L),]),na.rm=TRUE)
        rownames(weight_matrix) <- lngs
        colnames(weight_matrix) <- char_ages
        weight_matrix_name <- paste("weight_matrix_",stratum,sep="")
        assign(weight_matrix_name,weight_matrix)
      }

      weight_at_age <- matrix(0,dim(weight_matrix)[1],dim(weight_matrix)[2])
      for(age in 1:length(char_ages)){
        weight_at_age[,age] <- weight_matrix[,age]*nums_at_age[,age]/(colSums(nums_at_age,na.rm=TRUE)[age])
        rownames(weight_at_age) <- lngs
        colnames(weight_at_age) <- char_ages
        weight_at_age_name <- paste("weight_at_age_",stratum,sep="")
        assign(weight_at_age_name,weight_at_age)
      
      }
      
      for(rec in 1:nrow(strata_nums)){
      ton_sum <- numeric(ncol(nums_at_age))
      for(colum in 1:ncol(nums_at_age)){
      tonnage <- sum(strata_nums$mills_per_ICES[rec]/sum(sum(nums_at_age,na.rm=TRUE))*nums_at_age[,colum],na.rm=TRUE)*sum(weight_at_age[,colum],na.rm=TRUE)/1000
      ton_sum[colum] <- tonnage
      }
      strata_nums$ttons_per_ICES[rec] <- sum(ton_sum,na.rm=TRUE)
      }
      strata_nums_name <- paste("strata_numbers_",stratum,sep="")
      assign(strata_nums_name,strata_nums)
      
      strata_numbers_all <- rbind(strata_numbers_all,strata_nums)

    weights[strat] <- list(weight_at_age)

    }

    list(strata_numbers_all, weights)

}

