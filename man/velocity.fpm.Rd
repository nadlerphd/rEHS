\name{velocity.fpm}
\alias{velocity.fpm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Air Velocity in fpm
}
\description{
Airflow velocity is used to capture contaminants and overcome cross-drafts,
transportation of contaminants through the duct, and balancing
of “losses” in the system and in the discharge of the contaminant from the stack.
This is described as the “magnitude” of the system, which is a function of the velocity pressure.
}
\usage{
velocity.fpm(velocity.pressure.as.h2o)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{velocity.pressure.as.h2o}{
pressure in the direction of flow necessary to caus the air at rest to flow at a given velocity
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
## velocity pressure of 0.2″
## velocity.fpm(.20)
## "Velocity is 1791.09 fpm"
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
