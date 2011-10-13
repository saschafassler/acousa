\name{SPPselect}
\alias{SPPselect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SPPselect(acoustic_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acoustic_data}{
%%     ~~Describe \code{acoustic_data} here~~
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
function(acoustic_data){

### function to select species for further calculations
###
### input: -acoustic_data

### load required packages
    require(tcltk)

### select the species used for calculations
    tt<-tktoplevel()
    tl<-tklistbox(tt,height=length(c(colnames(acoustic_data)[c(17:(nr_spp+16))])),selectmode="single",background="white")
    tkgrid(tklabel(tt,text="Select species for analysis"))
    tkgrid(tl)
    spp_select_names <- c(colnames(acoustic_data)[c(17:(nr_spp+16))])
    select <- tclVar(0)
    for (i in (1:length(c(colnames(acoustic_data)[c(17:(nr_spp+16))])))){
    tkinsert(tl,"end",spp_select_names[i])}
    tkselection.set(tl,0)  # Default species is herring
    OK.but <- tkbutton(tt,text="   OK   ",command=function() tclvalue(select) <- spp_select_names[as.numeric(tkcurselection(tl))+1])
    tkgrid(OK.but)
    tkfocus(tt)
    tkwait.variable(select)
    spp_selection <- tclvalue(select)
    tkdestroy(tt)

    spp_selection

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
