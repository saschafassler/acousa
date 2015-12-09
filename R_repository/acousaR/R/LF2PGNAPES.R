### LF2PGNAPES

# load Biology file
  biology <- read.csv(file="D:/Sascha/Projects/WKEVAL/Dutch HERAS data/PGNAPES format/2014TRI2HERAS_Biology.csv",header=TRUE)

# load file with LF data (of fish with just length measurements)
  lf_data <- read.csv(file="D:/Sascha/Projects/WKEVAL/Dutch HERAS data/acousa format/catchLF_HERAS_2014_HER.csv",header=FALSE)
  lf_data[is.na(lf_data)] <- 0

# list relevant trawl numbers
  trawls <- unique(biology$Station)
  if(unique(biology$Year)==2012)
    trawls <- trawls[-c(7,8)]

# table of aged biological samples: numbers at length
  lf_biology <- t(table(biology[c("Station","Length")]))

# vector of measured lengths
  lengths <- as.numeric(row.names(lf_biology))

# creating table of additional records (with only length observations) per station
  for(t in 1: length(trawls)){
  
  station <- trawls[t]
  Nl_recs <- lf_data[-1,lf_data[1,]==station] - as.numeric(lf_biology[,colnames(lf_biology)==station]) #numbers at length of required records to be added to biological samples
  reclength <- sum(Nl_recs) #additional records to be added by station
  
  station_recs <- data.frame(matrix(ncol = length(colnames(biology)), nrow = reclength))
  colnames(station_recs) <- paste(colnames(biology))
  
  # fill additional records table
  station_recs$Country <- unique(biology$Country)
  station_recs$Vessel <- unique(biology$Vessel)
  station_recs$Cruise <- unique(biology$Cruise)
  station_recs$Station <- station
  station_recs$StType <- unique(biology$StType)
  station_recs$Year <- unique(biology$Year)
  station_recs$Species <- unique(biology$Species)
  station_recs$Length <- rep(lengths,Nl_recs)
  station_recs$Recnr <- seq(max(biology$Recnr[which(biology$Station==station)])+1,length=reclength)
  
  biology <- rbind(biology,station_recs)
  }
  
write.csv(biology, file="D:/Sascha/Projects/WKEVAL/Dutch HERAS data/PGNAPES format/2014TRI2HERAS_BiologyALL.csv")
