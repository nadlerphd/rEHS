\name{rwl}
\alias{rwl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recommended Weight Limit and Lifting Index
}
\description{
The primary product of the NIOSH equation is the recommended weight limit (RWL), which defines the maximum acceptable weight (load) that nearly all healthy employees could lift over the course of an 8-hour shift without increasing the risk of musculoskeletal disorders (MSD) to the lower back.
}
\usage{
rwl(horizontal.dist, vertical.dist, distance, angle, seconds.between.lifts,
    grasp, object.weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{horizontal.dist}{
horizontal location of the object relative to the body (cm)
}
  \item{vertical.dist}{
vertical location of the object relative to the floor (cm)
}
  \item{distance}{
distance the object is moved vertically (cm)
}
  \item{angle}{
the asymmetric angle is the amount (in degrees) of trunk and shoulder rotation
required by the lifting task. Angle must be input as 90, 60, 45, 30, or 0
}
  \item{seconds.between.lifts}{
time between lifts in seconds (s). Time must be entered as
300, 60, 30, 15, 10, or 6
}
  \item{grasp}{
quality of the workers grip on the object (1 = good, 2 = fair, 3 = poor)
}
  \item{object.weight}{
weight of the object in kilograms (kg)
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
## rwl(45, 25, 40, 0, 15, 1, 20)
## a worker is 45 cm from an object horizontally,
## object is 25 cm above the floor
## and must be lifted 40 cm up.
## worker is positioned 0 degrees relative to the object
## and has a good (1) grip.
## object weighs 20 kg.
## variable LI (lifting index) is created (5.328).
## variable weight.limit is created (3.75).

## LI interpretations:
## <=1: very low risk
## 1.01 - 1.50: low risk
## 1.51 - 2.00: moderate risk
## 2.01 - 3.00: high risk
## >3.01: very high risk

## weight.limit interpretation:
## the acceptable maximum weight (kg) that is safe under the
## horizontal, vertical, and distance data input in the rwl() function,
## including the angle, seconds between lifts, and grasp.
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
