### OneChannel

# load Biology file
AcousticValues <- read.csv(file="D:/Sascha/Projects/WKEVAL/Dutch HERAS data/PGNAPES format/2014TRI2HERAS_AcousticValues.csv",header=TRUE)

AV_channel <- data.frame(matrix(ncol = length(colnames(AcousticValues)), nrow = length(unique(AcousticValues$Log))))
colnames(station_recs) <- paste(colnames(AcousticValues)) 

temp <- aggregate(. ~ Log, data=AcousticValues, FUN=sum)
temp$chans <- temp$Year / unique(AcousticValues$Year)

temp$Country <- unique(AcousticValues$Country)
temp$Vessel <- unique(AcousticValues$Vessel)
temp$Cruise <- unique(AcousticValues$Cruise)
temp$Year <- unique(AcousticValues$Year)
temp$Month <- temp$Month/temp$chans
temp$Day <- temp$Day/temp$chans
temp$Species <- "HER"
temp$ChUppDepth <- 0
temp$ChLowDepth <- seq(50,500,unique(AcousticValues$ChLowDepth-AcousticValues$ChUppDepth))[temp$chans]

AV_channel <- temp[c(colnames(AcousticValues))]

write.csv(AV_channel, file="D:/Sascha/Projects/WKEVAL/Dutch HERAS data/PGNAPES format/2014TRI2HERAS_AcousticValuesONECHANNEL.csv")
