\name{time.interval}
\alias{time.interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Time Interval in Minutes for New Concentration of Contaminant
}
\description{
is used to determine the time period for a concentration buildup that will occur
}
\usage{
time.interval(volume.cf, flow.rate.cfm, contaminant.generation.rate.cfm, concentration.ppm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{volume.cf}{
volume of room or enclosure (cubic feet)
}
  \item{flow.rate.cfm}{
flow rate of the air (cfm)
}
  \item{contaminant.generation.rate.cfm}{
rate of generation of contaminant (cfm)
}
  \item{concentration.ppm}{
concentration of the contaminant to predict (ppm)
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
## time.interval(75000, 2500, 1.4, 175)
## 75000 is room volume
## 2500 is air flow rate
## 1.4 cfm of contaminant generation
## "Time interval is 11.24 minutes for 175.0 ppm"

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
