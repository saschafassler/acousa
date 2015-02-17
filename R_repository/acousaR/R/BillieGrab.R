billie.ex <- function(user,pass,survey,year){

##################################################################################################### 
#  																                                                                  #
#	CONNECT TO IMARES BILLIE DATABASE AND EXTRACT LENGTH/WEIGHT INFORMATION	IN PGNAPES FILE FORMAT    #
#	NOTE THAT YOU HAVE TO RUN SCRIPT IN 32(64) BIT VERSION IF YOU RUN ORACLE IN 32(64) BIT 	          #
#																                                                                    #
#	Sven Gastauer 2013												                                                        #
#####################################################################################################


	##################################
	##Connect to to FRISBEP databes ##
  ##################################
		db.con	<- odbcConnect(dsn = "FRISBEP", uid = user, pwd = pass)

	#Year as numeric
		year 		<-as.numeric(as.character(year))

	#SQL code to grab id for selected survey and Year
		sql_id 	<- paste("select id from Vis_stations where PGM_CODE = '", survey, "' and STN_DATE between '1-jan-",year,"' and '1-jan-",year+1,"'",collapse = ", ", sep = "")
		sql_ids 	<- paste("select * from Vis_stations where PGM_CODE = '", survey, "' and STN_DATE between '1-jan-",year,"' and '1-jan-",year+1,"'",collapse = ", ", sep = "")

	#Make SQL request for available trawls
		id 		<- sqlQuery(db.con, sql_id)
		ids		<- sqlQuery(db.con, sql_ids)

	#################
	## Get SAMPLES ##
	#################

		for (i in 1:length(id[,1])){
		#Get fish samples
			sql_sam 	<- paste("select * from vis_samples 	where stn_id = '",id[i,1],"'",sep="")
			sam		<- sqlQuery(db.con, sql_sam)
			sam	
			
		if (i == 1){
			sql_samples 	<- sam
		}else{
			sql_samples		<- rbind(sql_samples,sam)
		}
		}

	#########################
	### Get subsamples ######
	#########################

	#Get Sample IDs
		sam_id<-sql_samples$ID
	#SQL code to get subsamples
		for (i in 1:length(sam_id)){
			#sql_subsam  <- paste("select id from Vis_subsamples 	where SPE_ID = '", sam_id[i],"'",sep="")
			sql_subsam 	<- paste("select *  from vis_subsamples 	where SPE_ID = '", sam_id[i],"'",sep="")
			subsam	<- sqlQuery(db.con, sql_subsam)
			
		if (i == 1){
			sql_subsamples 	<- subsam
			#sql_ages		<- age
		}else{
			sql_subsamples	<- rbind(sql_subsamples,subsam)
			#sql_ages		<- rbind(sql_ages, age)
		}
		}

	#########################
	### Get Classes    ######
	#########################
	#Get SubSample IDs
		subsam_id<-sql_subsamples$ID
	#SQL code to getclkasses
		for (i in 1:length(subsam_id)){
			#sql_class	<- paste("select id from Vis_classes 	where SSE_ID = '", subsam_id[i],"'",sep="")
			sql_class	<- paste("select *  from vis_classes  where SSE_ID = '", subsam_id[i],"'",sep="")
			class		<- sqlQuery(db.con, sql_class)
			
		if (i == 1){
			sql_classes 	<- class
		}else{
			sql_classes		<- rbind(sql_classes,class)
		}
		}
	
	#########################
	### Get Ages    ######
	#########################

	#Define Species Code of main Species
	SpeciesCode	 <- sql_classes$TXN_NODC_CODE[which(sql_classes$HAS_AGES=='y')]
	SpeciesCode  <- sql_classes$TXN_NODC_CODE[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]

	#Get Class IDs
		class_id<-sql_classes$ID
	#SQL code to get ages
		for (i in 1:length(class_id)){
			#sql_age	<- paste("select id from Vis_ages	where CSS_ID = '", class_id[i],"'",sep="")
			sql_age	<- paste("select *  from vis_ages 	where CSS_ID = '", class_id[i],"'",sep="")
			age		<- sqlQuery(db.con, sql_age)
			
		if (i == 1){
			sql_ages 	<- age
		}else{
			sql_ages		<- rbind(sql_ages, age)
		}
		}
	#Add Na values to age
	ageNA <- as.data.frame(cbind(ID=sql_classes$ID[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])],HAS=sql_classes$HAS_AGES[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]))
	ageNA$AGE<-NA
	ageNA$AGE[which(ageNA$HAS=="y")]<- sql_ages$YEARCLASS

##############################
### PGNAPES output  ##########
##############################


Recnr		 <- sql_classes$FISH_NUMBER[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]
Country	 <- rep("NL",length(Recnr))
Vessel	 <- rep("PBVO",length(Recnr))
Cruise	 <- rep(ids$PGM_CODE[1],length(Recnr))
Station	 <- sql_samples$SEQ_NO
Stat		 <- NA

#Get number of samples per station
NStat		 <- aggregate(sql_classes$FISH_NUMBER[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])],by=list(sql_classes$SSE_ID[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]),FUN=length)
names(NStat) <- c("SSE_ID","N")
for (a in 1:length(NStat[,1])){
	NStat$SPE_ID[a] <- sql_subsamples$SPE_ID[which(sql_subsamples$ID==NStat$SSE_ID[c(a)])]
	NStat$STN_ID[a] <- sql_samples$SEQ_NO[which(sql_samples$ID==NStat$SPE_ID[c(a)])]
	temp<-rep(NStat$STN_ID[a],NStat$N[a])
	if(a==1){Stat=temp
			}else{
				Stat=c(Stat,temp)
			}
}

StType	 <- "PTRAWL"
Year		 <- year
Length	 <- sql_classes$LENGTH[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]
Weight	 <- sql_classes$CSS_WEIGHT[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]
AgeScale	 <- ""
AgeOtolith	 <- ageNA$AGE
Sex		 <- as.character(sql_classes$GENDER[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])])
Maturation	 <- sql_classes$MATURITY[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]
StomFullness <- sql_classes$DIGESTION_STAGE[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]
StomWt	 <- sql_classes$STOMACH_WEIGHT[which(sql_classes$TXN_NODC_CODE==SpeciesCode[1])]

PGNAPES <- as.data.frame(cbind(	Country 	 = Country, 
						Vessel	 = Vessel, 
						Cruise	 = Cruise, 
						Station	 = Stat,
						StType	 = StType,
						Year		 = Year,
						SpeciesCode	 = SpeciesCode,
						Length	 = Length*100,
						Weight	 = Weight,
						AgeScale	 = AgeScale,
						AgeOtolith	 = year-AgeOtolith,
						Sex		 = as.character(Sex),	
						Maturation	 = as.character(Maturation),
						StomFullness = StomFullness,
						StomachWT	 = StomWt,
						Recnr		 = Recnr
						),stringsAsFactors=FALSE)
#Sex to numeric
PGNAPES$Sex <- as.character(PGNAPES$Sex)
PGNAPES$Sex[which(PGNAPES$Sex=="u")] <- NA
PGNAPES$Sex[which(PGNAPES$Sex=="f")] <- "1"
PGNAPES$Sex[which(PGNAPES$Sex=="m")] <- 2
#Replace Species code by Name
PGNAPES$SpeciesCode	<- as.character(PGNAPES$SpeciesCode)
PGNAPES$SpeciesCode[which(PGNAPES$SpeciesCode==874701020100)]<-"HER"
PGNAPES$SpeciesCode[which(PGNAPES$SpeciesCode==879103220100)]<-"WHB"

#New Maturity scale (from 2011 onwards)
PGNAPES$Maturation<-as.character(PGNAPES$Maturation)
PGNAPES$Maturation[which(PGNAPES$Maturation=="u")] <- NA
PGNAPES$Maturation<-as.numeric(as.character(PGNAPES$Maturation))

if(year>=2011){
	PGNAPES$Maturation<- PGNAPES$Maturation-60
	#Blue Whiting
		PGNAPES$Maturation[which(PGNAPES$Maturation==1 & PGNAPES$SpeciesCode=="WHB")]<-1 
		PGNAPES$Maturation[which(PGNAPES$Maturation==2 & PGNAPES$SpeciesCode=="WHB")]<-5 
		PGNAPES$Maturation[which(PGNAPES$Maturation==3 & PGNAPES$SpeciesCode=="WHB")]<-7 
		PGNAPES$Maturation[which(PGNAPES$Maturation==4 & PGNAPES$SpeciesCode=="WHB")]<-8 
	#Herring
		PGNAPES$Maturation[which(PGNAPES$Maturation==1 & PGNAPES$SpeciesCode=="HER")]<-2 
		PGNAPES$Maturation[which(PGNAPES$Maturation==2 & PGNAPES$SpeciesCode=="HER")]<-4 
		PGNAPES$Maturation[which(PGNAPES$Maturation==3 & PGNAPES$SpeciesCode=="HER")]<-6 
		PGNAPES$Maturation[which(PGNAPES$Maturation==4 & PGNAPES$SpeciesCode=="HER")]<-8 
	}

#get rid of NA values
PGNAPES <- replace(PGNAPES, is.na(PGNAPES), "")


write.csv(PGNAPES, file=paste(data.dir,"/PGNAPES_",survey,"_",year,".csv",sep=""), row.names=FALSE)

}


##################################################################################################
#########################                 ########################################################
########################       EXAMPLE     #######################################################
#########################                 ########################################################
##################################################################################################

##Define needed variables 
#user<-"..."
#pass<-"..."
#year=2013
#survey="HERAS"
#
##Run the function
#billie.ex(user,pass,survey,year)
