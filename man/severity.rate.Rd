\name{severity.rate}
\alias{severity.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Severity rate
}
\description{
a mathematical calculation that describes the number of lost days
experienced as compared to the number of incidents experienced
}
\usage{
severity.rate(total.days.lost, total.recordable.incidents)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{total.days.lost}{
total number of work days lost due to injury and illness. number of days lost
do not need to be consecutive
}
  \item{total.recordable.incidents}{
total number of recordable incidents that happened in a reporting period
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
## severity.rate(5, 2)
## 5 work days were lost from 2 recordable incidents
## "Severity rate: 2.50"
## the average recordable incident resulted in 2.5 lost days
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
