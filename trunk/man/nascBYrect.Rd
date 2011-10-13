\name{nascBYrect}
\alias{nascBYrect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nascBYrect(acoustic_data, spp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acoustic_data}{
%%     ~~Describe \code{acoustic_data} here~~
}
  \item{spp}{
%%     ~~Describe \code{spp} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(acoustic_data,spp) {

### function to create NASC by ICES rectangle for the selected species
###
### input: -acoustic_data: acoustic data file with strata and ICES rectangle information
###        -spp: 3 capital letter species code with apostrophes [e.g. "HER"]

### calculate nr of EDSUs and average SA per ICES rectangle
    mileage_list <- as.data.frame(matrix(0,length(levels(as.factor(acoustic_data$ICES))),3))
    colnames(mileage_list) <- c("ICES","EDSU_per_ICES","Mean_NASC")
    mileage_list$ICES <- levels(as.factor(acoustic_data$ICES))
    for (recta in 1:nrow(mileage_list)){
    mileage_list$EDSU_per_ICES[recta] <- nrow(acoustic_data[acoustic_data$ICES==mileage_list$ICES[recta],]) #nr of EDSU's
    mileage_list$Mean_NASC[recta] <- sum(acoustic_data[paste(spp)][acoustic_data$ICES==mileage_list$ICES[recta],])/mileage_list$EDSU_per_ICES[recta]
    }
    acoustic_data <- merge(acoustic_data,mileage_list,by=c("ICES"))

### double check final NASC values with export files
### ... UNDER DEVELOPMENT ...

### save SA/EDSU per ICES list for the species
    y_n <- tkmessageBox(message = paste("Do you want to save the SA-table for '",spp,"'?",sep=""),
        icon = "question", type = "yesno", default = "yes")
    answer <- tclvalue(y_n)
    if(answer=="yes")
    fileName <- tclvalue(tkgetSaveFile(initialfile = paste(output_folder,"SA_table_",spp,".xls",sep=""),
        filetypes = "{{Excel Files} {.xls}}"))
    if(answer=="no")
    fileName<-""
    if (!nchar(fileName)) {
        tkmessageBox(message = "The SA-table was not saved!")
    } else {
        tkmessageBox(message = paste("The file saved was", fileName))
        write.xls(mileage_list,file=paste(output_folder,"SA_table_",spp,".xls",sep=""),colNames=TRUE,rowNames=NA,sheet=1)
    }
    
    mileage_list
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
