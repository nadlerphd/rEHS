\name{mgm3.to.ppm}
\alias{mgm3.to.ppm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert mg/m3 to ppm
}
\description{
Converts a concentration in milligrams per cubic meter --> parts per million
}
\usage{
mgm3.to.ppm(mg.per.cubic.meter, molecular.weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mg.per.cubic.meter}{
milligrams per cubic meter
}
  \item{molecular.weight}{
molecular weight
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
## Using acrylamide
## mgm3.to.ppm(10 ,78)
## concentration of acyrlamide is 10 mg/m3
## molecular weight of acrylamide is 78 grams/mol
## "mg/m3: 9.99"
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
