\name{dart}
\alias{dart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Days away/restricted or transfer rate
}
\description{
A calculation that describes the number of recordable injuries and illnesses per 100 full-time employees that resulted in days away from work, restricted work activity and/or job transfer that a company has experienced in any given time frame.
}
\usage{
dart(dart.incidents, total.labor.hours)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dart.incidents}{
number of dart-related incidents as defined above
}
  \item{total.labor.hours}{
total number of hours employees worked in a reporting period
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
## dart(2, 28000)
## "DART: 14.29"
## For every 100 employees, 14.29 incidents resulted in lost or restricted days or job transfer due to work related injuries or illnesses.
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
