\name{concentration}
\alias{concentration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate new concentration after a time interval
}
\description{
When using dilution ventilation, the flow rate of fresh air is determined by (a) contaminant
generation, (b) proper mixing, and (c) target final concentration. The methods used apply to
uniform rates of generation and low to moderate toxicity. This function is used to determine the concentration buildup that will occur over a given period measured in minutes.
}
\usage{
concentration(contaminant.generation.rate, flow.rate.cfm, timeframe, volume.cf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contaminant.generation.rate}{
the rate of contaminant released in cubic feet per minute (cfm)
}
  \item{flow.rate.cfm}{
the rate of air flow in cubic feet per minute (cfm)
}
  \item{timeframe}{
the number of minutes (min) it takes to reach the new concentration
}
  \item{volume.cf}{
the volume of the room in cubic feet (cf)
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
## concentration(1.4, 2500, 11, 75000)
## contaminant generation is 1.4 cfm
## air flow is 2500 cfm
## 11 minutes is the time interval
## room is 75,000 cubic feet
## "Concentration: 171.90 ppm"

  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
