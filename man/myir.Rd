\name{myir}
\alias{myir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing an incident rate to an industry incident rate
}
\description{
This function returns TRUE if your incident rate (my.ir) is lower than the
industry incident rate for your NAICS (my.industry)
}
\usage{
my.ir.comparison(my.ir, my.naics.code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my.ir}{
input your OSHA-recordable incident rate
}
  \item{my.naics.code}{
input your industry's NAICS number
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
## incident rate = 2.7
## industry is dairy product manufacturing (3115)
## my.ir.comparison(2.7, 3115)
## # A tibble: 1 × 3
  Industry                    NAICS    IR
   0s<chr>                       <dbl> <dbl>
1 Dairy product manufacturing  3115   4.3
## NAICS industry has a 4.3 incident rate
       IR
[1,] TRUE
## you have a lower incident rate than the industry as a whole
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
