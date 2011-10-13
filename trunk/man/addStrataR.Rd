\name{addStrataR}
\alias{addStrataR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addStrataR(spp, surv_yr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spp}{
%%     ~~Describe \code{spp} here~~
}
  \item{surv_yr}{
%%     ~~Describe \code{surv_yr} here~~
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
function(spp,surv_yr) {

### function to create survey area strata file
###
### input: -spp: 3 capital letter species code with apostrophes [e.g. "HER"]
###        -surv_year: 4 digit year of survey [e.g. 1982]

### load required packages
    require(xlsReadWrite)

### check if strata and coast factor files exist for selected species
    if(paste("strataR_",surv_yr,"_",spp,".xls",sep="") \%in\% list.files(input_folder))
    {tkmessageBox(message = paste("strata file for ",spp," was found successfully!",sep=""),icon="info",type="ok")}
    if(!paste("strataR_",surv_yr,"_",spp,".xls",sep="") \%in\% list.files(input_folder))
    {tkmessageBox(message = paste("No strata file for ",spp," was found!",sep=""),icon="warning",type="ok")}

### load strata file in strataR format
    files <- list.files(input_folder)
    strata_file <- list.files(input_folder,pattern = paste("strataR_",surv_yr,"_",spp,".xls",sep=""))
    strata <- read.xls(file=paste(input_folder,strata_file,sep=""),type="character",colNames=TRUE,rowNames=TRUE,sheet=1)

### get ICES rectangle list and strata per species
    rect <- matrix("",length(strata[strata!=""]),3)
    coastFact <- read.xls(paste(input_folder,"coastFact_",spp,".xls",sep=""),type="double",colNames=TRUE,rowNames=TRUE,sheet=1)
    for (r in 1:nrow(strata)){
      for (c in 1:ncol(strata)){
        pos_rect <- length(rect[rect!=""])/ncol(rect)+1
        if(strata[r,c]!=""){rect[pos_rect,1] <- paste(rownames(strata)[r],colnames(strata)[c],sep="")}
        if(strata[r,c]!=""){rect[pos_rect,2] <- strata[r,c]}
        if(strata[r,c]!=""){rect[pos_rect,3] <- coastFact[r,c]}
      }
    }

    as.data.frame(rect)
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
