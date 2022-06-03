\name{dB.distance}
\alias{dB.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sound level at various distances
}
\description{
a function to calculate sound levels taken at one distance
and extrapolate the sound level at another distance
}
\usage{
dB.distance(dB0, distance.original, distance.new)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dB0}{
the original sound measurement (dB)
}
  \item{distance.original}{
the original distance where noise measurement was taken
}
  \item{distance.new}{
the second distance to calculate the sound level reading
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
## dB.distance(92, 4, 10)
## "Noise level in dB at distance 10.00 is 84.04"
## Sound level is 92 dB at 4 feet from a source. At 10 feet from source, sound
## level is measured as 84.04 dB.
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
