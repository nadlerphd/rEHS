\name{wind.chill}
\alias{wind.chill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wind chill
}
\description{
a function used to calculate a temperature based on wind speed.
temperature must be below 70 degrees Fahrenheit.
}
\usage{
wind.chill(temp.fahr, wind.speed.mph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp.fahr}{
degrees in Fahrenheit (must be below 70)
}
  \item{wind.speed.mph}{
measured wind speed in miles per hour (mph)
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
## wind.chill(40, 15)
## "Wind chill is 31.84 F when wind speed is 15.0 and air temperature is 40.0 F"
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
