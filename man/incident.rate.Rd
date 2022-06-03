\name{incident.rate}
\alias{incident.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OSHA incident rate
}
\description{
a calculation that describes the number of
recordable incidents that a company experiences per 100 full-time employees
in any given time frame.
}
\usage{
incident.rate(recordable.cases, total.hours.worked)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordable.cases}{
all work related deaths, illnesses, and injuries which result in a loss of consciousness, restriction of work or motion, permanent transfer to another job within the company, or that require some type of medical treatment or first-aid.
}
  \item{total.hours.worked}{
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
## incident.rate(17, 1500000)
## represents 17 recordable cases from 1.5 million hours
## For every 100 employees, 2.27 have been involved in a recordable injury or illness""
## "Incidence rate: 2.27"
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
