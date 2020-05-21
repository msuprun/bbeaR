\name{bbea.QCSamples}
\alias{bbea.QCSamples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality control of samples by visualization of average counts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QCSamples(bbea.obj, filename='...')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{
     bbeaR data object
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{pavg}{Average counts of all samples}
\item{pmin}{Minimum counts of all samples}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maria Suprun
Randall J. Ellis
Mayte Suárez-Fariñas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bbea.QC.heatmap.counts.byPlate
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
