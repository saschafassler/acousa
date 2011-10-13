\name{AsaR2FishFrame}
\alias{AsaR2FishFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AsaR2FishFrame(strata_numbers_all, acoustic_data, spp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strata_numbers_all}{
%%     ~~Describe \code{strata_numbers_all} here~~
}
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
function(strata_numbers_all,acoustic_data,spp)

### function to convert AsaR output into FishFrame format for the selected species
###
### input: -strata numbers: data frame with species numbers by strata and ICES rectangle information

### load required packages
    require(xlsReadWrite)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
