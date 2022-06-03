\name{min.air.volume}
\alias{min.air.volume}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate minimum air sampling volume
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
min.air.volume(limit.of.quantification.mg, contaminant.target.concentration.mgm3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limit.of.quantification.mg}{
the smallest amount or the lowest concentration of a substance that is possible to be determined
by means of a given analytical procedure with the established accuracy, precision, and uncertainty
}
  \item{contaminant.target.concentration.mgm3}{
an estimate of the airborne concentrations of the contaminant being tested.
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
## min.air.volume(0.0027, 500)
## a 0.0027 mg/m3 limit of quantification (concentration) exists for acetone
## we are interested in a target concentration of 500 mg/m3
"Minimum air volume required: 0.0054 liters"
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
